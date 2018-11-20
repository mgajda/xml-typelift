{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE ViewPatterns        #-}
-- | Here we aim to analyze the schema.
module CodeGen(codegen) where

import           Prelude hiding(lookup)

import           Control.Lens as Lens
import           Control.Monad(forM, forM_)
--import qualified Control.Monad.State.Class  as St
--import qualified Control.Monad.Writer.Class as Writer
--import qualified Control.Monad.Reader.Class as Reader
import qualified Control.Monad.RWS.Strict   as RWS
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy       as BSL(length, toStrict)
import qualified Data.ByteString.Builder    as B
import           Data.Generics.Uniplate.Operations
import qualified Data.Map.Strict            as Map
import           Data.Maybe(catMaybes)
import qualified Data.Set                   as Set
import           Data.String

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

data HDef = HType
          | HField

translateType  = translate' "UnnamedElementType" normalizeTypeName

translateField :: XMLString -> XMLString -> CG B.Builder
translateField = translate' "unnamedFieldName"   normalizeFieldName

-- | Translate XML Schema identifier into Haskell identifier,
--   maintaining dictionary to assure uniqueness of Haskell identifier.
translate' ::  XMLString               -- placeholder for empty inputs
           -> (XMLString -> XMLString) -- normalizer
           ->  XMLString               -- input container name
           ->  XMLString               -- input name
           -> CG B.Builder
translate' placeholder normalizer container xmlName = do
    tr <- Lens.use translations
    case Map.lookup xmlName tr of
      Just r  -> --return $ "Translation for " <> B.byteString xmlName <> " is " <> B.byteString r <> " normalizer gave " <>
                 return $ B.byteString $ normalizer xmlName
      Nothing ->
        let proposals = proposeTranslations xmlName
        in do
          case filter (`Map.notMember` tr) proposals of
            (goodProposal:_) -> do
              _ <- translations %= Map.insert xmlName goodProposal
              return $ B.byteString goodProposal
  where
    proposeTranslations     :: XMLString -> [XMLString]
    proposeTranslations (normalizer -> name) =
        [BS.take i container <> normName | i :: Int <- [0..BS.length container]] <>
        [normName <> bshow i | i :: Int <- [1..]]
      where
        normName | name==""  = placeholder
                 | otherwise = name


-- | Returns a pair of field name, and type code.
generateElementInstance :: XMLString -- container name
                        -> Element -> CG (B.Builder, B.Builder)
generateElementInstance container elt@(Element {minOccurs, maxOccurs, eName, ..}) =
    (,) <$>  translateField                  container eName
        <*> (wrapper <$> generateElementType container elt  )
  where
    wrapper tyName | minOccurs==1 && maxOccurs==MaxOccurs 1 =             tyName
                   | minOccurs==0 && maxOccurs==MaxOccurs 1 = "Maybe " <> tyName
                   | otherwise                              = "["      <> tyName <> "]"
generateElementInstance container _ = return ( B.byteString container
                                             , "generateElementInstanceNotFullyImplemented" )

type Field = (B.Builder, B.Builder)

generateElementType :: XMLString -- container name
                    -> Element
                    -> CG B.Builder
-- Flatten elements with known type to their types.
generateElementType container (eType -> Ref (stripNS -> ""    )) = return "ElementWithEmptyRefType"
generateElementType container (eType -> Ref (stripNS -> tyName)) = translateType container tyName
generateElementType container (Element {eName, eType = Complex attrs content})   = do
    myTypeName  <- translateType container eName
    attrFields  :: [Field] <- mapM makeAttrType attrs
    childFields :: [Field] <- case content of -- serving only simple Seq of elts or choice of elts for now
      Seq    ls -> seqInstance ls
      Choice ls -> (:[]) <$> makeAltType ls
    RWS.tell $ "data " <> myTypeName <> " ="
    makeSumType [(myTypeName, attrFields <> childFields)]
    return      myTypeName
  where
    makeAttrType :: Attr -> CG (B.Builder, B.Builder)
    makeAttrType Attr {..} = mapSnd (wrapper use) <$> makeFieldType aName aType
    mapSnd f (a, b) = (a, f b)
    wrapper :: Schema.Use -> B.Builder -> B.Builder
    wrapper  Optional   ty = "Maybe " <> ty
    wrapper  Required   ty =             ty
    wrapper (Default x) ty =             ty
    makeFieldType :: XMLString -> Type -> CG (B.Builder, B.Builder)
    makeFieldType  aName aType = (,) <$> translateField      eName aName
                                     <*> generateContentType eName aType
    makeAltType :: [TyPart] -> CG (B.Builder, B.Builder)
    makeAltType ls = return ("altFields", "AltTypeNotYetImplemented")
    seqInstance = mapM fun
      where
        fun (Elt (elem@(Element {eName=subName}))) = do
          (name, ty) <- generateElementInstance eName elem
          generateElementInstance eName elem


makeSumType :: [(B.Builder, [Field])] -> CG ()
makeSumType []                       = error "Empty list of records"
makeSumType (firstEntry:nextEntries) = do
    RWS.tell   $ "    " <> formatRecord firstEntry <> "\n"
    forM_ nextEntries $ \nextEntry ->
      RWS.tell $ "  | " <> formatRecord nextEntry

type Record = (B.Builder, [Field])

formatRecord :: Record -> B.Builder
formatRecord (name, (f:fields)) =
    builderUnlines
      ( formatHeading f
      :(formatFollowing <$> fields))
    <> trailer
  where
    formatHeading   f = header   <> formatField f
    formatFollowing f = follower <> formatField f
    header, follower, leftPad, trailer :: B.Builder
    header   =         name    <> " { "
    follower =         leftPad <> " , "
    trailer  = "\n" <> leftPad <> " }"
    leftPad  = B.byteString
             $ BS.replicate (fromIntegral $ BSL.length $ B.toLazyByteString name) ' '


builderUnlines :: [B.Builder] -> B.Builder
builderUnlines []     = ""
builderUnlines (l:ls) = l <> mconcat (("\n" <>) <$> ls)

formatField (fName, fTypeName) = fName <> " :: " <> fTypeName

wrapList  x = "["      <> x <> "]"
wrapMaybe x = "Maybe " <> x

    -- TODO: implement PROHIBITED

generateContentType :: XMLString -- container name
                    -> Type -> CG B.Builder
generateContentType container (Ref tyName) = translateType container tyName
  -- TODO: check if the type was already translated (as it should, if it was generated)
generateContentType _          other       = return "NotYetImplemented"

-- | Make builder to generate schema code
codegen    :: Schema -> B.Builder
codegen sch = extractBuilder $ generateSchema sch
  where
    extractBuilder    :: CG () -> B.Builder
    extractBuilder rws = case RWS.runRWS rws sch initialState of
                           ((), _state, builder) -> builder
generateSchema sch = do
    RWS.tell "module XMLSchema where\n"
    RWS.tell "import FromXML\n"
    -- First generate all types that may be referred by others.
    _               <- generateContentType "Top" `mapM` types sch
    -- Then generate possible top level types.
    topElementTypeNames <- generateElementType "Top" `mapM` tops sch
    case topElementTypeNames of
      []                   -> fail "No toplevel elements found!"
      [eltName] | baseHaskellType (builderString eltName) ->
           RWS.tell $ "newtype TopLevel = TopLevel "    <> eltName
      [eltName]                                           ->
           RWS.tell $ "type " <> topLevelConst <> " = " <> eltName
      (firstAlt:otherAlts) -> RWS.tell $ "data TopLevel =\n"                 <>
                                            genFirstAlt             firstAlt <>
                                            mconcat (genNextAlt <$> otherAlts)
    RWS.tell "\n"
  where
    genFirstAlt, genNextAlt, genAlt :: B.Builder -> B.Builder
    genFirstAlt alt = "    " <> genAlt alt
    genNextAlt  alt = "  | " <> genAlt alt
    genAlt typeName = "Top" <> typeName <> " " <> typeName
    topLevelConst = "TopLevel"

-- | Translating base XML types.
fromBaseXMLType :: (Eq a, IsString a, IsString b) => a -> b
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
  otherwise            -> "Xeno.Node" -- or error?\

builderString = BSL.toStrict
              . B.toLazyByteString

baseHaskellType = (`Set.member` baseHaskellTypes)

baseHaskellTypes :: Set.Set XMLString
baseHaskellTypes  = Set.fromList $ map fromBaseXMLType
                                 $ Set.toList predefinedTypes

