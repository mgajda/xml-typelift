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

import           Control.Monad(forM, when)
import qualified Data.ByteString.Builder    as B
import qualified Data.ByteString.Char8      as BS
import           Data.String
import qualified Data.Map.Strict            as Map
import qualified Data.Set                   as Set

import           FromXML(XMLString)

import           Schema
import           CodeGenMonad
import           BaseTypes
import           Types
import           TypeDecls
import           TypeAlg

-- | Returns a pair of field name, and type code.
--   That means that type codes are in ElementName namespace, if described in-place,
--   or standard SchemaType, if referred inside ComplexType declaration.
--generateElementInstance :: XMLString -- container name
--                        -> Element -> CG Field
elementInstance :: TyCtx -> Element -> CG TyCtx
elementInstance tyCtx elt@(Element {minOccurs, maxOccurs, eName, eType}) = do
    -- After computing type context, we need to find what type to assign it...
    ty <- (Whole . wrapper) <$> elementType myCtx eType
    return $ myCtx { ty }
  where
    myCtx = tyCtx `parents` (ElementName, eName)
    wrapper t | minOccurs==1 && maxOccurs==MaxOccurs 1 =           t
              | minOccurs==0 && maxOccurs==MaxOccurs 1 = wrapMaybe t
              | otherwise                              = wrapList  t

elementType :: TyCtx -> Type -> CG HType
elementType tyCtx (Ref      ""           ) = return "ElementWithEmptyRefType" -- error code
elementType tyCtx (Ref      tyName       ) = referType (tyCtx `parents` (SchemaType, tyName))
elementType tyCtx (Complex {attrs, inner}) = do
    attrs <- makeAttrType
    warn ["ComplexType not implemented yet"]
    return anyXMLType
elementType tyCtx (Extension   {}) = do
    warn ["Extension not implemented yet"]
    return anyXMLType
elementType tyCtx (Restriction {}) = do
    warn ["Restriction type not implemented yet"]
    return anyXMLType

namedType :: TyCtx -> HType -> CG HType
namedType tyCtx  (Named n) = do
  areWeDone <- isTypeDefinedYet $ ctxName tyCtx
  if areWeDone
     then return                       $ Named n
     else declare $ tyCtx { ty = Whole $ Named n }
namedType tyCtx (TyExpr e) = declare $ tyCtx { ty = Whole $ TyExpr e }

-- | Wraps type according to XML Schema "use" attribute value.
wrapAttr :: Schema.Use -> HType -> HType
wrapAttr  Optional   ty = wrapMaybe ty
wrapAttr  Required   ty =           ty
wrapAttr (Default _) ty =           ty

(topTypeCtx,
 topEltCtx ) = (topCtx SchemaType ,
                topCtx ElementName)
  where
    topCtx klass name = TyCtx { containerId="Top", schemaType=klass, ctxName=name, ty=undefined }

-- | Convert TyPart xs:sequence, xs:choice or a single xs:element into type fragment
contentType :: TyCtx -> TyPart -> CG TyCtx
contentType tyCtx (Elt    e) = do
  elementInstance tyCtx e
contentType tyCtx (Seq    s) = do
  tySequence =<< mapM (contentType tyCtx) s
contentType tyCtx (Choice c) = do
  tyChoice   =<< mapM (contentType tyCtx) c

-- | Given a container with ComplexType details (attributes and children),
--   generate the type to hold them.
--   Or if it turns out these are referred types - just return their names.
--   That means that our container is likely 'SchemaType' namespace
--   NOTE: This function *has* to *always* return a type name translated from XMLString argument,
--         in case we use this name as reference for ComplexType!
generateContentType :: XMLString -- container name
                    -> Type -> CG B.Builder
generateContentType container (Ref tyName) = translate (SchemaType, TargetTypeName) container tyName
  -- TODO: check if the type was already translated (as it should, if it was generated)
generateContentType eName (Complex {attrs, inner=content}) = do
    myTypeName  <- translate (SchemaType, TargetTypeName) eName eName
    attrFields  :: [Field] <- tracer "attr fields"  <$> mapM makeAttrType attrs

    case flatten content of -- serving only simple Seq of elts or choice of elts for now
                    -- These would be in ElementType namespace.
      Seq    ls -> seqInstance myTypeName attrFields eName ls
      Choice ls -> makeAltType myTypeName attrFields eName ls
      Elt     e -> seqInstance myTypeName attrFields eName [Elt e]
  where
    makeAttrType :: Attr -> CG (B.Builder, B.Builder)
    makeAttrType Attr {..} = mapSnd (wrapAttr use) <$> makeFieldType aName aType
    makeFieldType :: XMLString -> Type -> CG (B.Builder, B.Builder)
    makeFieldType  aName aType = (,) <$> translate (AttributeName, TargetFieldName) eName aName
                                     <*> generateContentType                        eName aType
generateContentType eName (Restriction _ (Enum (uniq -> values))) = do
  tyName     <- translate (SchemaType ,   TargetTypeName) eName        eName -- should it be split into element and type containers?
  translated <- translate (EnumIn eName,  TargetConsName) eName `mapM` values
  -- ^ TODO: consider enum as indexed family of spaces
  declareSumType (tyName, (,"") <$> translated)
  return tyName
generateContentType eName (Restriction base (Pattern _)) = do
  tyName   <- translate (ElementName, TargetTypeName) (eName <> "pattern") base
  consName <- translate (ElementName, TargetConsName) (eName <> "pattern") base
  baseTy   <- translate (SchemaType,  TargetTypeName)  eName               base
  warn ["Restriction pattern"]
  declareNewtype tyName consName baseTy
  return tyName
generateContentType eName (Extension   base (Complex False [] (Seq []))) = do
  tyName   <- translate (SchemaType,  TargetTypeName) base eName
  consName <- translate (ElementName, TargetConsName) base eName
  baseTy   <- translate (SchemaType,  TargetTypeName) base eName
  warn ["Empty extension of base type"]
  declareNewtype tyName consName baseTy
  return tyName
generateContentType eName  (Restriction base  None      ) =
  -- Should we do `newtype` instead?
  generateContentType eName $ Ref base
generateContentType eName (Extension   base  (cpl@Complex {inner=Seq []})) = do
  superTyLabel <- translate (SchemaType,TargetFieldName) eName "Super" -- should be: MetaKey instead of SchemaType
  generateContentType eName $ cpl
                  `appendElt` Element {eName=builderString superTyLabel
                                      ,eType=Ref base
                                      ,maxOccurs=MaxOccurs 1
                                      ,minOccurs=1
                                      ,targetNamespace=""}
  -- TODO: Refactor for parser generation!
generateContentType eName (Extension   base  otherType                   ) = do
  warn ["Complex extensions are not implemented yet"]
  tyName   <- translate (SchemaType,  TargetTypeName) base eName
  consName <- translate (ElementName, TargetConsName) base eName
  declareNewtype tyName consName anyXMLType
  return tyName
generateContentType _          other       = do
  warn ["Not yet implemented generateContentType ", show other]
  return "Xeno.Node"

seqInstance :: B.Builder -> [Field] -> XMLString -> [TyPart] -> CG B.Builder
seqInstance myTypeName attrFields eName ls = do
    myConsName  <- translate (SchemaType, TargetConsName) eName eName
    childFields <- mapM fun ls
    declareAlgebraicType (myTypeName, [(myConsName, attrFields <> childFields)])
    return                myTypeName
  where
    fun (Elt (elt@(Element {}))) = generateElementInstance eName elt
    fun  x = error $ "Not yet implemented nested sequence, all or choice:" <> show x

-- | Make sum type if element has <xs:choice> inside
--   Note that list of alternatives has at least one element!
makeAltType :: B.Builder -> [Field] -> XMLString -> [TyPart] -> CG B.Builder
makeAltType myTypeName attrFields container alts@(flattenAlts -> [Elt a, Elt b]) = do
    contentType <- makeAltContentType container alts
    case attrFields of
      [] -> return contentType
      _  -> do
        myConsName  <- translate (SchemaType, TargetConsName) container container
        declareAlgebraicType (myTypeName, [(myConsName, ("inner", contentType):attrFields)])
        return myTypeName
makeAltType myTypeName attrs container alts = do
  warn ["xs:choice implementation is yet limited to only two elements:", show alts]
  return anyXMLType

-- | Builds union type out of elements of xs:choice
makeAltContentType :: XMLString -> [TyPart] -> CG B.Builder
makeAltContentType container alts@(flattenAlts -> [Elt a, Elt b]) = do
    tyA <- generateElementType container a
    tyB <- generateElementType container b
    let realType = "Either " <> tyA <> " " <> tyB
    return realType

appendElt :: Type -> Element -> Type
appendElt cpl@Complex { inner=Seq sq } elt = cpl { inner=Seq (Elt elt:sq   ) }
appendElt cpl@Complex { inner=other  } elt = cpl { inner=Seq [Elt elt,other] }
appendElt other                        elt = error $ "Cannot append field for supertype to: " <> show other

-- | Make builder to generate schema code
codegen    :: Schema -> B.Builder
codegen sch = runCodeGen sch $ generateSchema sch

-- | Generate content type, and put an type name on it.
generateNamedContentType :: (XMLString, Type) -> CG ()
generateNamedContentType (name, ty) = do
    contentTypeCode <- generateContentType name ty
    when (needsWrapper $ builderString contentTypeCode) $ do
      warn ["-- Named base type\n"]
      contentTypeName <- translate (SchemaType, TargetTypeName) name name
      contentConsName <- translate (SchemaType, TargetConsName) name name
      declareNewtype contentTypeName contentConsName contentTypeCode
  where
    needsWrapper typeDesc = isBaseHaskellType         typeDesc
                         || "Either " `BS.isPrefixOf` typeDesc

generateSchema :: Schema -> CG ()
generateSchema sch = do
    gen ["{-# LANGUAGE DuplicateRecordFields #-}\n"
        ,"-- | Autogenerated by xml-typelift\n"
        ,"--   DO NOT CHANGE - rather update the XML Schema it was made from\n"
        ,"module XMLSchema where\n\n"
        ,B.byteString basePrologue
        ,"\n\n"]
    -- First generate all types that may be referred by others.
    mapM_ generateNamedContentType $ Map.toList $ types sch
    -- Then generate possible top level types.
    topElementTypeNames <- generateElementType "Top" `mapM` tops sch
    case topElementTypeNames of
      []                                          -> fail "No toplevel elements found!"
      [eltName]
        | isBaseHaskellType (builderString eltName) -> do
           gen ["-- Toplevel\n"]
           declareNewtype topLevelConst topLevelConst eltName
      [eltName]                                   ->
           declareTypeAlias topLevelConst eltName
      altTypes                                    -> do
           -- Add constructor name for each type
           -- TODO: We would gain from separate dictionary for constructor names!
           alts <- (`zip` altTypes) <$> forM altTypes
                                            (translate (SchemaType, TargetTypeName) topLevelConst . builderString)
           declareSumType (topLevelConst, alts)
    gen     ["\n"]

topLevelConst :: IsString a => a
topLevelConst = "TopLevel"

-- | Eliminate duplicates from the list
uniq :: Ord a => [a] -> [a]
uniq  = Set.toList . Set.fromList

-- * Debugging
tracer :: String -> p2 -> p2
--tracer lbl a = trace (lbl <> show a) a
tracer _ a = a

instance Show B.Builder where
  show = BS.unpack . builderString

declareTypeAlias topLevelConst eltName =
  gen      [ "type ", topLevelConst, " = ", eltName, "\n" ]
