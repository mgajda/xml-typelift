{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ViewPatterns        #-}
-- | Here we aim to analyze the schema.
module CodeGen(codegen) where

import           Prelude hiding(lookup)

import           Control.Lens as Lens
--import qualified Control.Monad.State.Class  as St
--import qualified Control.Monad.Writer.Class as Writer
--import qualified Control.Monad.Reader.Class as Reader
import qualified Control.Monad.RWS.Strict   as RWS
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Builder    as B
import           Data.Generics.Uniplate.Operations
import qualified Data.Map.Strict            as Map
import           Data.Maybe(catMaybes)
import qualified Data.Set                   as Set

import           Xeno.Types(XenoException(..))

import           FromXML(getStartIndex, stripNS)
import           Identifiers
import           Schema

-- | State of code generator
data CGState =
  CGState {
    -- Translation of XML Schema identifiers to Haskell identifiers
    _translations :: Map.Map XMLString XMLString
  }
makeLenses ''CGState

type CG a = RWS.RWS Schema B.Builder CGState a

initialState = CGState
             $ Map.fromList [(bt, fromBaseXMLType bt)
                            | bt <- Set.toList predefinedTypes ]

bshow = BS.pack . show

data NameClass = Id
               | Key
               | Elt
               | Attr
               | Type

-- | Name of the object, with additional information
data Named = Named NameClass XMLString

proposeTranslations     :: XMLString -> [XMLString]
proposeTranslations name = [normName <> bshow i | i :: Int <- [1..]]
  where
    normName | name==""  = "XMLElem"
             | otherwise = normalizeTypeName name

-- | Translate XML Schema identifier into Haskell identifier,
--   maintaining dictionary to assure uniqueness of Haskell identifier.
translate :: XMLString -> CG XMLString
translate xmlName = do
  tr <- Lens.use translations
  case Map.lookup xmlName tr of
    Just r  -> return r
    Nothing ->
      let proposals = proposeTranslations xmlName
      in do
        case filter (`Map.notMember` tr) proposals of
          (goodProposal:_) -> do
            _ <- translations %= Map.insert xmlName goodProposal
            return goodProposal

generateElementType :: Element -> CG XMLString
-- Flatten elements with known type to their types.
generateElementType (eType -> Ref tyName) = return tyName
generateElementType  other                = return "NotYetImplemented"

generateContentType :: Type -> CG XMLString
generateContentType (Ref tyName) | tyName `Set.member` predefinedTypes = return tyName
generateContentType  other = return "NotYetImplemented"

-- | Make builder to generate schema code
codegen    :: Schema -> B.Builder
codegen sch = extractBuilder $ do
  -- First generate all types that may be referred by others.
  _               <- mapM generateContentType $ types sch
  -- Then generate possible top level types.
  topElementTypeNames <- mapM generateElementType $ tops sch
  case topElementTypeNames of
    []                   -> fail "No toplevel elements found!"
    [eltName]            -> RWS.tell $ "type TopLevel = "               <> B.byteString eltName
    (firstAlt:otherAlts) -> RWS.tell $ "data TopLevel =\n"              <>
                                          genFirstAlt             firstAlt <>
                                          mconcat (genNextAlt <$> otherAlts)
  where
    extractBuilder    :: CG () -> B.Builder
    extractBuilder rws = case RWS.runRWS rws sch initialState of
                           ((), _state, builder) -> builder
    genFirstAlt, genNextAlt, genAlt :: XMLString -> B.Builder
    genFirstAlt alt = "    " <> genAlt alt
    genNextAlt  alt = "  | " <> genAlt alt
    genAlt typeName = "Top" <> B.byteString typeName <> " " <> B.byteString typeName

-- | Translating base XML types.
fromBaseXMLType s = case s of
  "any"                -> "Xeno.Node"
  "string"             -> "String"
  "token"              -> "String"
  "integer"            -> "Int" -- or Integer
  "positiveInteger"    -> "Int" -- or Integer
  "float"              -> "Float"
  "date"               -> "Date"
  "decimal"            -> "Int"
  "positiveInteger"    -> "Int"
  "float"              -> "Float"
  "double"             -> "Double"
  otherwise            -> "Xeno.Node" -- or error?
