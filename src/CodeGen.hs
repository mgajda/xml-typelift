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

import           Prelude hiding(lookup, id)

import           Control.Monad(when, void)
import           Data.String
import qualified Data.Map.Strict            as Map
import qualified Data.Set                   as Set
import qualified Data.ByteString.Builder    as B

import           FromXML(XMLString)

import           Schema
import           CodeGenMonad
import           BaseTypes
import           Types
import           TypeCtx

import           Debug.Trace(trace)

-- | Returns a pair of field name, and type code.
--   That means that type codes are in ElementName namespace, if described in-place,
--   or standard SchemaType, if referred inside ComplexType declaration.
--generateElementInstance :: XMLString -- container name
--                        -> Element -> CG Field
elementInstance :: TyCtx -> Element -> CG TyCtx
elementInstance tyCtx elt@(Element {minOccurs, maxOccurs, eName, eType}) = do
    -- After computing type context, we need to find what type to assign it...
    ty <- (Whole . wrapper) <$> complexType myCtx eType
    return $ myCtx { ty }
  where
    myCtx = tyCtx `parents` (ElementName, eName)
    wrapper t | minOccurs==1 && maxOccurs==MaxOccurs 1 =           t
              | minOccurs==0 && maxOccurs==MaxOccurs 1 = wrapMaybe t
              | otherwise                              = wrapList  t

-- | Extract complex type
complexType :: TyCtx -> Type -> CG HType
complexType tyCtx (Ref      ""           ) = do
  warn  ["ElementWithEmptyRefType: ", show tyCtx] -- error code
  return anyXML
complexType tyCtx (Ref      tyName       ) = referType (tyCtx `parents` (SchemaType, tyName))
complexType tyCtx (Complex {attrs, inner}) = do
    attrFields <- makeAttrType  tyCtx `mapM` attrs
    -- innerCtx   <- freshInnerCtx tyCtx "content"
    innerTy    <- contentType tyCtx $ flatten inner
    composite  <- tySequence (innerTy:attrFields)
    fragType    $ tyCtx { ty = ty composite }
complexType tyCtx (Extension   {}) = do
    warn ["Extension not implemented yet"]
    return anyXML
complexType tyCtx (Restricted {base, restriction=None}) = do
    warn ["Empty restriction"]
    referType (tyCtx `parents` (SchemaType, base))
complexType tyCtx (Restricted {base, restriction=Pattern _}) = do
    warn ["Pattern"] -- no pattern validation yet!
    referType (tyCtx `parents` (SchemaType, base))
complexType tyCtx (Restricted {restriction=Enum (uniq -> values)}) = do
    warn ["Enum ", show values]
    tyPart  <- Sum <$> mapM (enumCons tyCtx) values
    fragType $ tyCtx { ty=tyPart }
complexType tyCtx (Restricted {restriction}) = do
    warn ["Restriction type not implemented yet", show restriction]
    return anyXML

makeAttrType :: TyCtx -> Attr -> CG TyCtx
makeAttrType tyCtx (Attr { use, aName, aType }) = do
    hType <- wrapAttr use <$> complexType aCtx aType
    return $ aCtx { ty=Whole hType }
  where
    aCtx = tyCtx `parents` (AttributeName, aName)

-- | Wraps type according to XML Schema "use" attribute value.
wrapAttr :: Schema.Use -> HType -> HType
wrapAttr  Optional   ty = wrapMaybe ty
wrapAttr  Required   ty =           ty
wrapAttr (Default _) ty =           ty

topTypeCtx, topEltCtx :: XMLString -> TyCtx
(topTypeCtx,
 topEltCtx ) = (topCtx SchemaType ,
                topCtx ElementName)
  where
    topCtx klass name = TyCtx { containerId=topLevelConst, schemaType=klass, ctxName=name, ty=undefined }

-- | Convert TyPart xs:sequence, xs:choice or a single xs:element into type fragment
contentType :: TyCtx -> TyPart -> CG TyCtx
contentType tyCtx (Elt    e) = do
  elementInstance tyCtx e
contentType tyCtx (Seq   []) = trace ("Empty Seq " <> show (ctxName     tyCtx)
                                   <> " in "       <> show (containerId tyCtx)) $ do
  return tyCtx { ty = Rec [] }
contentType tyCtx (Seq    s) = trace "tySequence in contentType" $ do
  tySequence =<< mapM (contentType tyCtx) s
contentType tyCtx (Choice c) = do
  tyChoice   =<< mapM (contentType tyCtx) c

ensureTypeIsNamed :: XMLString -> HType -> XMLIdNS -> CG ()
ensureTypeIsNamed name ty klass = case ty of
    Named  n -> do
     undeclared <- not <$> isTypeDefinedYet name
     undeclared `when` void (declare tyCtx)
    TyExpr e -> void $ declare $ tyCtx
  where
    tyCtx = TyCtx { ty = Whole ty, containerId = topLevelConst, ctxName = name, schemaType=klass }

namedType :: (XMLString, Type) -> CG ()
namedType (name, ty) = do
    hTy <- complexType (topTypeCtx name) ty
    ensureTypeIsNamed   name hTy SchemaType

topElement :: Element -> CG TyCtx
topElement elt@(Element { eName, eType }) = do
    elementInstance (topEltCtx eName) elt

generateSchema :: Schema -> CG ()
generateSchema sch = do
    gen ["{-# LANGUAGE DuplicateRecordFields #-}\n"
        ,"-- | Autogenerated by xml-typelift\n"
        ,"--   DO NOT CHANGE - rather update the XML Schema it was made from\n"
        ,"module XMLSchema where\n\n"
        ,basePrologue
        ,"\n\n"]
    -- First generate all types that may be referred by others.
    mapM_ namedType $ Map.toList $ types sch
    -- Then generate possible top level types.
    tops <- topElement `mapM` tops sch
    null tops `when` fail "No toplevel elements found!"
    compositeTop <- fragType =<< tyChoice tops
    ensureTypeIsNamed topLevelConst compositeTop ElementName
    return ()

topLevelConst :: IsString a => a
topLevelConst = "TopLevel"

-- | Eliminate duplicates from the list
uniq :: Ord a => [a] -> [a]
uniq  = Set.toList . Set.fromList

-- * Debugging
tracer :: Show a => String -> a -> a
tracer lbl a = trace (lbl <> show a) a
--tracer _ a = a

-- | Make builder to generate schema code
codegen :: Schema -> B.Builder
codegen  = runCodeGen . generateSchema

