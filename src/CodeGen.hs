{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
-- | Here we aim to analyze the schema.
module CodeGen(codegen, parserCodegen, parserCodegen1) where


import           Prelude hiding(lookup, id)

import           Control.Arrow
import           Control.Monad.Fix
import           Control.Monad(forM, forM_, when)
import qualified Data.ByteString.Builder     as B
import qualified Data.ByteString.Char8       as BS
import           Data.String
import qualified Data.Map.Strict             as Map
import qualified Data.Set                    as Set
import qualified Language.Haskell.TH         as TH
import           Text.InterpolatedString.Perl6 (qc)

import           FromXML(XMLString)

import           BaseTypes
import           CodeGenMonad
import           Schema
import           TypeDecls

import Data.Generics.Uniplate.Operations

import Data.Map (Map)


import qualified Control.Monad.RWS.Strict   as RWS -- TODO REMOVE AND use own methods

-- | Returns a pair of field name, and type code.
--   That means that type codes are in ElementName namespace, if described in-place,
--   or standard SchemaType, if referred inside ComplexType declaration.
generateElementInstance :: XMLString -- container name
                        -> Element -> CG TyField
generateElementInstance container elt@(Element {minOccurs, maxOccurs, eName, ..}) =
    (,) <$> (TyFieldName <$> translate (ElementName, TargetFieldName) container eName)
        <*> (TyType  <$> wrapper <$> generateElementType container elt)
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
generateElementType _         (eType -> Ref (""    )) = return "ElementWithEmptyRefType"
generateElementType container (eType -> Ref (tyName)) =
  translate (SchemaType, TargetTypeName) container tyName
generateElementType _         (Element {eName, eType})   =
  case eType of
    Complex   {} -> generateContentType eName eType
    --Extension {} -> do
    --  warn [qc|Did not implement elements with extension types yet {eType}|]
    --  return "Xeno.Node"
    other        -> do
      warn [qc|Unimplemented type {other}|]
      return "Xeno.Node"

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
generateContentType container (Ref (tyName)) = translate (SchemaType, TargetTypeName) container tyName
  -- TODO: check if the type was already translated (as it should, if it was generated)
generateContentType eName (Complex {attrs, inner=content}) = do
    myTypeName  <- translate (SchemaType, TargetTypeName) eName eName
    myConsName  <- translate (SchemaType, TargetConsName) eName eName
    attrFields  :: [TyField] <- return [] -- tracer "attr fields"  <$> mapM makeAttrType attrs
    childFields :: [TyField] <- tracer "child fields" <$>
                                  case content of -- serving only simple Seq of elts or choice of elts for now
                              -- These would be in ElementType namespace.
      Seq    ls -> seqInstance ls
      All    ls -> seqInstance ls -- handling the same way
      Choice ls -> return [] -- (:[]) <$> makeAltType ls
      Elt     e -> error  $ "Unexpected singular Elt inside content of ComplexType: " <> show e
    outCodeLine [qc|-- eName = {eName}|]
    declareAlgebraicType (TyData myTypeName, [(TyCon myConsName, attrFields <> childFields)])
    return      myTypeName
  where
    --makeAttrType :: Attr -> CG TyField
    --makeAttrType Attr {..} = second (\(TyType bs) -> TyType $ wrapAttr use bs) <$> makeFieldType aName aType
    --makeFieldType :: XMLString -> Type -> CG TyField
    --makeFieldType  aName aType = (,) <$> (TyFieldName <$> translate (AttributeName, TargetFieldName) eName aName)
    --                                 <*> (TyType      <$> generateContentType                        eName aType)
    --makeAltType :: [TyPart] -> CG TyField
    --makeAltType ls = do
    --  warn [qc|altType not yet implemented: {ls}|]
    --  return (TyFieldName "altFields", TyType "Xeno.Node")
    seqInstance = mapM fun
      where
        fun (Elt (elt@(Element {}))) = do
          generateElementInstance eName elt
        fun  x = error $ "Not yet implemented nested sequence, all or choice:" <> show x
--generateContentType eName (Restriction _ (Enum (uniq -> values))) = do
--  tyName     <- translate (SchemaType ,   TargetTypeName) eName        eName -- should it be split into element and type containers?
--  translated <- translate (EnumIn eName,  TargetConsName) eName `mapM` values
--  -- ^ TODO: consider enum as indexed family of spaces
--  declareSumType (TyData tyName, (\con -> (TyCon con, TyType "")) <$> translated)
--  return tyName
--generateContentType eName (Restriction base (Pattern _)) = do
--  tyName   <- translate (ElementName, TargetTypeName) (eName <> "pattern") base
--  consName <- translate (ElementName, TargetConsName) (eName <> "pattern") base
--  baseTy   <- translate (SchemaType,  TargetTypeName)  eName               base
--  warn "-- Restriction pattern"
--  declareNewtype (TyData tyName) (TyCon consName) (TyType baseTy)
--  return tyName
--generateContentType eName (Extension   base (Complex False [] (Seq []))) = do
--  tyName   <- translate (SchemaType,  TargetTypeName) base eName
--  consName <- translate (ElementName, TargetConsName) base eName
--  baseTy   <- translate (SchemaType,  TargetTypeName) base eName
--  declareNewtype (TyData tyName) (TyCon consName) (TyType baseTy)
--  return tyName
--generateContentType eName  (Restriction base  None      ) =
--  -- Should we do `newtype` instead?
--  generateContentType eName $ Ref base
--generateContentType eName (Extension   base  (cpl@Complex {inner=Seq []})) = do
--  superTyLabel <- translate (SchemaType,TargetFieldName) eName "Super" -- should be: MetaKey instead of SchemaType
--  generateContentType eName $ cpl
--                  `appendElt` Element {eName=builderString superTyLabel
--                                      ,eType=Ref base
--                                      ,maxOccurs=MaxOccurs 1
--                                      ,minOccurs=1
--                                      ,targetNamespace=""}
--  -- TODO: Refactor for parser generation!
--generateContentType eName (Extension   base  _otherType                  ) = do
--  warn "Complex extensions are not implemented yet"
--  tyName   <- translate (SchemaType,  TargetTypeName) base eName
--  consName <- translate (ElementName, TargetConsName) base eName
--  declareNewtype (TyData tyName) (TyCon consName) (TyType "Xeno.Node")
--  return tyName
generateContentType _ _ = error "Don't know hot to generateContentType"

appendElt :: Type -> Element -> Type
appendElt cpl@Complex { inner=Seq sq } elt  = cpl { inner=Seq (Elt elt:sq   ) }
appendElt cpl@Complex { inner=other  } elt  = cpl { inner=Seq [Elt elt,other] }
appendElt other                        _elt = error [qc|Cannot append field for supertype to: {other}|]


codegen' :: Schema -> CG () -> IO String
codegen' sch gen = do
    let output = runCodeGen sch gen
    codeLines <- mapM outputToString output
    return $ unlines codeLines
  where
    outputToString (CGCodeLine cmt) = return cmt
    outputToString (CGDec decl') = do
        decl <- TH.runQ decl'
        return $ concat ["\n", TH.pprint decl, "\n"]
    outputToString (CGDecs decl') = do
        decl <- TH.runQ decl'
        return $ concat ["\n", TH.pprint decl, "\n"]


-- | Make builder to generate schema code
-- TODO rename it!
codegen    :: Schema -> IO String
codegen sch = codegen' sch (generateSchema sch)


-- | Make parser for schema
parserCodegen :: Schema -> IO String
parserCodegen sch = codegen' sch (generateParser sch)


-- | Make parser for schema
parserCodegen1 :: Schema -> IO String
parserCodegen1 sch = codegen' sch (generateParser1 sch)


-- | Generate content type, and put an type name on it.
generateNamedContentType :: (XMLString, Type) -> CG ()
generateNamedContentType (name, ty) = do
  contentTypeName <- translate (SchemaType, TargetTypeName) name name
  contentConsName <- translate (SchemaType, TargetConsName) name name
  contentTypeCode <- generateContentType name ty
  when (isBaseHaskellType $ builderString contentTypeCode) $ do
    warn "-- Named base type"
    declareNewtype (TyData contentTypeName) (TyCon contentConsName) (TyType contentTypeCode)

generateSchema :: Schema -> CG ()
generateSchema sch = do
    outCodeLine "{-# LANGUAGE DuplicateRecordFields #-}"
    outCodeLine "module XMLSchema where"
    outCodeLine basePrologue
    -- First generate all types that may be referred by others.
    mapM_ generateNamedContentType $ Map.toList $ types sch
    -- Then generate possible top level types.
    topElementTypeNames <- generateElementType "Top" `mapM` tops sch
    case topElementTypeNames of
      []                                          -> fail "No toplevel elements found!"
      [eltName]
        | isBaseHaskellType (builderString eltName) -> do
           outCodeLine "-- Toplevel"
           declareNewtype (TyData topLevelConst) (TyCon topLevelConst) (TyType eltName)
      [eltName]                                   -> do
           -- TODO change to Template Haskell generation
           outCodeLine [qc|type {topLevelConst::String} = {eltName}|]
      altTypes                                    -> do
           -- Add constructor name for each type
           -- TODO: We would gain from separate dictionary for constructor names!
           alts <- (map (TyCon *** TyType)) <$>
                   (`zip` altTypes) <$>
                       forM altTypes
                            (translate (SchemaType, TargetTypeName) topLevelConst . builderString)
           declareSumType (TyData topLevelConst, alts)


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


-- * ----------------------------------------------------------------------------------------------
-- * ----------------------------------------------------------------------------------------------
-- * ----------------------------------------------------------------------------------------------
-- * ----------------------------------------------------------------------------------------------


outCodeLine' :: String -> CG ()
outCodeLine' msg = do
    ind <- getIndent
    outCodeLine ((replicate ind ' ') ++ msg)


withIndent :: CG a -> CG a
withIndent act = do -- TODO use `bracket`
    incIndent
    r <- act
    decIndent
    return r



generateParser1 :: Schema -> CG ()
generateParser1 Schema{..} = do
    outCodeLine [qc|-- PARSER --|]
    -- Generate parser header
    forM_ tops $ \topEl -> do
        let topName = eName topEl
        when (minOccurs topEl /= 1) $ error [qc|Wrong minOccurs = {minOccurs topEl}|]
        when (maxOccurs topEl /= MaxOccurs 1) $ error [qc|Wrong maxOccurs = {maxOccurs topEl}|]
        outCodeLine' [qc|parse{eName topEl}ToArray :: ByteString -> Either String TopLevelInternal|]
        outCodeLine' [qc|parse{eName topEl}ToArray bs = Right $ TopLevelInternal bs $ UV.create $ do|]
        withIndent $ do
            outCodeLine' [qc|vec <- UMV.unsafeNew ((BS.length bs `div` 7) * 2)|]
            outCodeLine' [qc|parse{topName} vec|]
            outCodeLine' [qc|return vec|]
            outCodeLine' [qc|where|]
            withIndent $ do
                outCodeLine' [qc|parse{topName} :: forall s . UMV.STVector s Int -> ST s ()|]
                outCodeLine' [qc|parse{topName} vec = do|]
                withIndent $ do
                    outCodeLine' [qc|let (_, _) <- inTag "{topName}" (skipSpaces $ skipHeader $ skipSpaces bs 0) $ parse{topName}Content|]
                    outCodeLine' [qc|return ()|]
    -- Generate parsers for certain types
    let additionalTypes = extractAdditionalTypes tops -- TODO filter out repeated types
    withIndent $ withIndent $ forM_ ((Map.toList types) ++ additionalTypes) $ \(typeName, ty) -> do
        outCodeLine' [qc|parse{typeName}Content arrStart strStart = do|]
        withIndent $ case ty of
            Complex _ _ (Seq elts) -> do
                let elements = map (\case (Elt e) -> e ; _ -> error [qc|Unsupported type: {take 100 $ show ty}|]) elts
                let ofsNames' = (("arrStart", "strStart") : [ ( [qc|arrOfs{i}|], [qc|strOfs{i}|]) | i <- [(1::Int)..]])
                                :: [(XMLString, XMLString)]
                    ofsNames = zip ofsNames' (tail ofsNames')
                forM_ (zip ofsNames elements) $ \(((arrOfs, strOfs), (arrOfs', strOfs')), el) -> do
                    let parserName = getParserName (eType el) (eName el)
                        (tagQuantifier::XMLString) = case el of
                                Element 0 (MaxOccurs 1) _ _ _ -> "inMabyeTag"
                                Element 1 (MaxOccurs 1) _ _ _ -> "inOneTag"
                                Element 0 Unbounded     _ _ _ -> "inManyTags"
                                Element m n             _ _ _ -> error [qc|Unsupported element quantities: ({m}, {n})|]
                    outCodeLine' [qc|({arrOfs'}, {strOfs'}) <- {tagQuantifier} "{eName el}" {strOfs} $ parse{parserName} {arrOfs}|]
                let endNum = length elements
                outCodeLine' [qc|return (arrOfs{endNum}, strOfs{endNum})|]
            Ref ref ->
                outCodeLine' [qc|parse{ref}Content arrStart strStart|]
            _ -> error [qc|Unsupported type: {take 100 $ show ty}|]
    -- Generate auxiliary functions
    withIndent $ do
        outCodeLine' [qc|inOneTag   tag ofs inParser = undefined|]
        outCodeLine' [qc|inMaybeTag tag ofs inParser = undefined|]
        outCodeLine' [qc|inManyTags tag ofs inParser = undefined|]
        outCodeLine' [qc|parseString   arrStart strStart = undefined|]
        outCodeLine' [qc|parseDecimal  arrStart strStart = undefined|]
        outCodeLine' [qc|parseDateTime arrStart strStart = undefined|]
        outCodeLine' [qc|parseInteger  arrStart strStart = undefined|]
  where
    getParserName :: Type -> XMLString -> XMLString
    getParserName (Ref "xs:integer") _  = "Integer"
    getParserName (Ref "xs:decimal") _  = "Decimal"
    getParserName (Ref "xs:string") _   = "String"
    getParserName (Ref "xs:token") _    = "String"
    getParserName (Ref "xs:dateTime") _ = "DateTime"
    getParserName (Ref r) _             = [qc|{r}Content|]
    getParserName (Complex {}) xname    = xname
    getParserName t _                   = [qc|???{t}|]
    extractAdditionalTypes :: [Element] -> [(XMLString, Type)]
    extractAdditionalTypes elts =
        let allElts = (universeBi elts :: [Element])
        in map (\(Element _ _ name typ _) -> (name, typ)) allElts





