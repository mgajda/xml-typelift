{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE ViewPatterns          #-}
-- | Here we aim to analyze the schema.
module CodeGen(codegen) where

import           Prelude hiding(lookup)

import           Control.Monad(forM)
import qualified Control.Monad.RWS.Strict   as RWS
import qualified Data.ByteString.Builder    as B
import qualified Data.ByteString.Char8      as BS
import           Data.String
import qualified Data.Map.Strict            as Map

import           FromXML(stripNS)

import           Schema
import           CodeGenMonad
import           BaseTypes
import           TypeDecls

import           Debug.Trace

-- | Returns a pair of field name, and type code.
generateElementInstance :: XMLString -- container name
                        -> Element -> CG Field
generateElementInstance container elt@(Element {minOccurs, maxOccurs, eName, ..}) =
    (,) <$>  translateField                  container eName
        <*> (wrapper <$> generateElementType container elt  )
  where
    wrapper tyName | minOccurs==1 && maxOccurs==MaxOccurs 1 =             tyName
                   | minOccurs==0 && maxOccurs==MaxOccurs 1 = "Maybe " <> tyName
                   | otherwise                              = "["      <> tyName <> "]"
generateElementInstance container _ = return ( B.byteString container
                                             , "generateElementInstanceNotFullyImplemented" )

tracer lbl a = trace (lbl <> show a) a

instance Show B.Builder where
  show = BS.unpack . builderString

generateElementType :: XMLString -- container name
                    -> Element
                    -> CG B.Builder
-- Flatten elements with known type to their types.
generateElementType container (eType -> Ref (stripNS -> ""    )) = return "ElementWithEmptyRefType"
generateElementType container (eType -> Ref (stripNS -> tyName)) = translateType container tyName
generateElementType container (Element {eName, eType = Complex attrs content})   = do
    myTypeName  <- translateType container eName
    attrFields  :: [Field] <- tracer "attr fields" <$> mapM makeAttrType attrs

    childFields :: [Field] <- tracer "child fields" <$> case content of -- serving only simple Seq of elts or choice of elts for now
      Seq    ls -> seqInstance ls
      Choice ls -> (:[]) <$> makeAltType ls
    RWS.tell $ "data " <> myTypeName <> " ="
    declareAlgebraicType [(myTypeName, attrFields <> childFields)]
    return      myTypeName
  where
    makeAttrType :: Attr -> CG (B.Builder, B.Builder)
    makeAttrType Attr {..} = mapSnd (wrapper use) <$> makeFieldType aName aType
    makeFieldType :: XMLString -> Type -> CG (B.Builder, B.Builder)
    makeFieldType  aName aType = (,) <$> translateField      eName aName
                                     <*> generateContentType eName aType
    makeAltType :: [TyPart] -> CG (B.Builder, B.Builder)
    makeAltType ls = return ("altFields", "AltTypeNotYetImplemented")
    seqInstance = mapM fun
      where
        fun (Elt (elem@(Element {eName=subName}))) = do
          generateElementInstance eName elem

mapSnd f (a, b) = (a, f b)

-- | Wraps type according to XML Schema "use" attribute value.
wrapper :: Schema.Use -> B.Builder -> B.Builder
wrapper  Optional   ty = "Maybe " <> ty
wrapper  Required   ty =             ty
wrapper (Default x) ty =             ty

generateContentType :: XMLString -- container name
                    -> Type -> CG B.Builder
generateContentType container (Ref tyName) = translateType container tyName
  -- TODO: check if the type was already translated (as it should, if it was generated)
generateContentType _          other       = return "NotYetImplemented"

-- | Make builder to generate schema code
codegen    :: Schema -> B.Builder
codegen sch = runCodeGen sch $ generateSchema sch

-- | Generate content type, and put an type name on it.
generateNamedContentType :: (XMLString, Type) -> CG ()
generateNamedContentType (name, ty) = do
  contentTypeName <- translateType    "" name
  contentTypeCode <- generateContentType name ty
  if baseHaskellType $ builderString contentTypeCode
    then RWS.tell $ "\nnewtype " <> contentTypeName <> " = " <> contentTypeName <> " " <> contentTypeCode
    else RWS.tell $ "\ndata " <> contentTypeName <> " = " <> contentTypeCode

generateSchema :: Schema -> CG ()
generateSchema sch = do
    RWS.tell "{-# LANGUAGE DuplicateRecordFields #-}"
    RWS.tell "module XMLSchema where\n"
    RWS.tell "import FromXML\n"
    -- First generate all types that may be referred by others.
    mapM_ generateNamedContentType $ Map.toList $ types sch
    -- Then generate possible top level types.
    topElementTypeNames <- generateElementType "Top" `mapM` tops sch
    case topElementTypeNames of
      []                                          -> fail "No toplevel elements found!"
      [eltName]
        | baseHaskellType (builderString eltName) ->
           RWS.tell $ "newtype " <> topLevelConst
                   <> " = "      <> topLevelConst
                   <> " "        <> eltName
      [eltName]                                   ->
           RWS.tell $ "type " <> topLevelConst <> " = " <> eltName
      altTypes                                    -> do
           -- Add constructor name for each type
           -- TODO: We would gain from separate dictionary for constructor names!
           alts <- (`zip` altTypes) <$> forM altTypes (translateType topLevelConst . builderString)
           declareSumType (topLevelConst, alts)
    RWS.tell "\n"

topLevelConst :: IsString a => a
topLevelConst = "TopLevel"

