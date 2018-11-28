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

import           FromXML(stripNS, XMLString)

import           Schema
import           CodeGenMonad
import           BaseTypes
import           TypeDecls

--import           Debug.Trace

-- | Returns a pair of field name, and type code.
--   That means that type codes are in ElementName namespace, if described in-place,
--   or standard SchemaType, if referred inside ComplexType declaration.
generateElementInstance :: XMLString -- container name
                        -> Element -> CG Field
generateElementInstance container elt@(Element {minOccurs, maxOccurs, eName, ..}) =
    (,) <$>  translate (ElementName, TargetFieldName) container eName
        <*> (wrapper <$> generateElementType container elt  )
  where
    wrapper tyName | minOccurs==1 && maxOccurs==MaxOccurs 1 =             tyName
                   | minOccurs==0 && maxOccurs==MaxOccurs 1 = "Maybe " <> tyName
                   | otherwise                              = "["      <> tyName <> "]"
{-generateElementInstance container _ = return ( B.byteString container
                                             , "generateElementInstanceNotFullyImplemented" )-}

-- | Generate type of given <element/>, if not already declared by type="..." attribute reference.
generateElementType :: XMLString -- container name
                    -> Element
                    -> CG B.Builder
-- Flatten elements with known type to their types.
generateElementType _         (eType -> Ref (stripNS -> ""    )) = return "ElementWithEmptyRefType"
generateElementType container (eType -> Ref (stripNS -> tyName)) =
  translate (SchemaType, TargetTypeName) container tyName
generateElementType _         (Element {eName, eType})   =
  case eType of
    Complex attrs children -> generateContentType eName $ Complex attrs children
    other                  -> do
      warn [ "Unimplemented type extension ", show other ]
      return "Xeno.Node"

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (a, b) = (a, f b)

-- | Wraps type according to XML Schema "use" attribute value.
wrapAttr :: Schema.Use -> B.Builder -> B.Builder
wrapAttr  Optional   ty = "Maybe " <> ty
wrapAttr  Required   ty =             ty
wrapAttr (Default _) ty =             ty

-- | Given a container with ComplexType details (attributes and children),
--   generate the type to hold them.
--   Or if it turns out these are referred types - just return their names.
--   That means that our container is likely 'SchemaType' namespace
generateContentType :: XMLString -- container name
                    -> Type -> CG B.Builder
generateContentType container (Ref (stripNS -> tyName)) = translate (SchemaType, TargetTypeName) container tyName
  -- TODO: check if the type was already translated (as it should, if it was generated)
generateContentType eName (Complex attrs content) = do
    myTypeName  <- translate (SchemaType, TargetTypeName) eName eName
    myConsName  <- translate (SchemaType, TargetConsName) eName eName
    attrFields  :: [Field] <- tracer "attr fields"  <$> mapM makeAttrType attrs

    childFields :: [Field] <- tracer "child fields" <$>
                              case content of -- serving only simple Seq of elts or choice of elts for now
                              -- These would be in ElementType namespace.
      Seq    ls -> seqInstance ls
      All    ls -> seqInstance ls -- handling the same way
      Choice ls -> (:[]) <$> makeAltType ls
      Elt     e -> error  $ "Unexpected singular Elt inside content of ComplexType: " <> show e
    gen ["\ndata ", myTypeName, " ="]
    declareAlgebraicType [(myConsName, attrFields <> childFields)]
    return      myTypeName
  where
    makeAttrType :: Attr -> CG (B.Builder, B.Builder)
    makeAttrType Attr {..} = mapSnd (wrapAttr use) <$> makeFieldType aName aType
    makeFieldType :: XMLString -> Type -> CG (B.Builder, B.Builder)
    makeFieldType  aName aType = (,) <$> translate (AttributeName, TargetFieldName) eName aName
                                     <*> generateContentType                        eName aType
    makeAltType :: [TyPart] -> CG (B.Builder, B.Builder)
    makeAltType ls = return ("altFields", "**AltTypeNotYetImplemented**")
    seqInstance = mapM fun
      where
        fun (Elt (elem@(Element {eName=subName}))) = do
          generateElementInstance eName elem
        fun  x = error $ "Not yet implemented nested sequence, all or choice:" <> show x
generateContentType eName (Restriction base (Enum (uniq -> values))) = do
  tyName     <- translate (SchemaType ,   TargetTypeName) eName        eName -- should it be split into element and type containers?
  translated <- translate (EnumIn eName,  TargetConsName) eName `mapM` values
  -- ^ TODO: consider enum as indexed family of spaces
  declareSumType (tyName, (,"") <$> translated)
  return tyName
generateContentType eName (Restriction base (Pattern _)) = do
  tyName   <- translate (ElementName, TargetTypeName) (eName <> "pattern") base
  consName <- translate (ElementName, TargetConsName) (eName <> "pattern") base
  base     <- translate (SchemaType,  TargetTypeName)  eName               base
  gen ["\nnewtype ", tyName, " = ", consName, " ", base]
  return tyName
generateContentType eName (Restriction base  None      ) =
  -- Should we do `newtype` instead?
  generateContentType eName $ Ref base
generateContentType eName (Extension   base  ext       ) = do
  warn ["Extension not yet implemented ", show ext]
  return "Xeno.Node"
generateContentType _          other       = do
  warn ["Not yet implemented generateContentType ", show other]
  return "Xeno.Node"

-- | Make builder to generate schema code
codegen    :: Schema -> B.Builder
codegen sch = runCodeGen sch $ generateSchema sch

-- | Generate content type, and put an type name on it.
generateNamedContentType :: (XMLString, Type) -> CG ()
generateNamedContentType (name, ty) = do
  contentTypeName <- translate (SchemaType, TargetTypeName) name name
  contentConsName <- translate (SchemaType, TargetConsName) name name
  contentTypeCode <- generateContentType name ty
  when (isBaseHaskellType $ builderString contentTypeCode) $
    gen ["\nnewtype ", contentTypeName, " = ", contentConsName, " ", contentTypeCode, "\n"]

generateSchema :: Schema -> CG ()
generateSchema sch = do
    gen ["{-# LANGUAGE DuplicateRecordFields #-}\n"
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
        | isBaseHaskellType (builderString eltName) ->
           gen [ "newtype ", topLevelConst
               , " = "     , topLevelConst
               , " "       , eltName       ]
      [eltName]                                   ->
           gen      [ "type ", topLevelConst, " = ", eltName ]
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

