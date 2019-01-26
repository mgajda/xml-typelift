{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE PartialTypeSignatures #-}
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
elementInstance :: Element -> CG _
elementInstance elt@(Element {minOccurs, maxOccurs, eName, eType}) =
    -- After computing type context, we need to find what type to assign it...
    inScope ElementName eName $
      (Whole . wrapper) <$> complexType eType
  where
    --myCtx = tyCtx `parents` (ElementName, eName)
    wrapper t | minOccurs==1 && maxOccurs==MaxOccurs 1 =           t
              | minOccurs==0 && maxOccurs==MaxOccurs 1 = wrapMaybe t
              | otherwise                              = wrapList  t

referType :: XMLString -> CG HType
referType tyId =  Named
              <$> globalScope SchemaType tyId (translate TargetTypeName)

-- | Extract complex type
complexType :: _ -> CG HType
complexType (Ref      ""           ) = do
  warn  ["ElementWithEmptyRefType"] -- error code
  return anyXML
complexType (Ref      tyName       ) = referType tyName
--referType (tyCtx `parents` (SchemaType, tyName))
complexType (Complex {attrs, inner}) = do
    attrFields <- makeAttrType `mapM` attrs
    -- innerCtx   <- freshInnerCtx tyCtx "content"
    innerTy    <- contentType $ flatten inner
    composite  <- tySequence (innerTy:attrFields)
    declare    composite
complexType (Extension   {}) = do
    warn ["Extension not implemented yet"]
    return anyXML
complexType (Restricted {base, restriction=None}) = do
    warn ["Empty restriction"]
    referType base
    --referType (tyCtx `parents` (SchemaType, base))
complexType (Restricted {base, restriction=Pattern _}) = do
    warn ["Patterns are not validated yet"] -- no pattern validation yet!
    referType base
    --referType (tyCtx `parents` (SchemaType, base))
complexType (Restricted {restriction=Enum (uniq -> values)}) = do
    warn ["Enum ", show values]
    tyPart  <- Sum <$> mapM (enumCons []) values
    declare tyPart
complexType (Restricted {restriction}) = do
    warn ["Restriction type not implemented yet", show restriction]
    return anyXML

makeAttrType :: Attr -> CG HTyFrag
makeAttrType (Attr { use, aName, aType }) =
    inScope AttributeName aName $ do
      hType <- wrapAttr use <$> complexType aType
      return $ Whole hType
--  where
--    aCtx = tyCtx `parents` (AttributeName, aName)

-- | Wraps type according to XML Schema "use" attribute value.
wrapAttr :: Schema.Use -> HType -> HType
wrapAttr  Optional   ty = wrapMaybe ty
wrapAttr  Required   ty =           ty
wrapAttr (Default _) ty =           ty

topTypeScope    tName = globalScope SchemaType  tName
topElementScope eName = globalScope ElementName eName
{-
topTypeCtx, topEltCtx :: XMLString -> TyCtx
(topTypeCtx,
 topEltCtx ) = (topCtx SchemaType ,
                topCtx ElementName)
  where
    topCtx klass name = TyCtx { containerId=topLevelConst, schemaType=klass, ctxName=name, ty=undefined }
 -}

-- | Convert TyPart xs:sequence, xs:choice or a single xs:element into type fragment
contentType :: TyPart -> CG HTyFrag
contentType (Elt    e) = do
  elementInstance e
contentType (Seq   []) = do
  warn ["Empty Seq"]
  return $ Rec []
contentType (Seq    s) = trace "tySequence in contentType" $ do
  tySequence =<< mapM contentType s
contentType (Choice c) = do
  tyChoice   =<< mapM contentType c

ensureTypeIsNamed :: XMLString -> HType -> XMLIdNS -> CG ()
ensureTypeIsNamed name ty klass = case ty of
    Named  n -> do
     undeclared <- not <$> isTypeDefinedYet name
     undeclared `when` void (declare $ Whole ty)
    TyExpr e -> void $ declare e
  --where
  --  tyCtx = TyCtx { ty = Whole ty, containerId = topLevelConst, ctxName = name, schemaType=klass }

namedType :: (XMLString, Type) -> CG ()
namedType (name, ty) =
    inScope SchemaType name $ do
      hTy <- complexType ty
      ensureTypeIsNamed   name hTy SchemaType

topElement :: Element -> CG _ -- TyCtx
topElement elt@(Element { eName, eType }) =
  topElementScope eName $ elementInstance elt

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
    compositeTop <- tyChoice tops
    ensureTypeIsNamed topLevelConst (Whole compositeTop) ElementName
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

