{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE TupleSections         #-}
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
module CodeGen(codegen, parserCodegen) where


import           Prelude hiding(lookup, id)

import           Control.Arrow
import           Data.Maybe
import           Control.Monad
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

import Debug.Pretty.Simple
import Text.Pretty.Simple


import Identifiers


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
    Extension {} -> do
      warn [qc|-- Did not implement elements with extension types yet {eType}|]
      return "Xeno.Node"
    other        -> do
      warn [qc|-- Unimplemented type {other}|]
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
    attrFields  :: [TyField] <- tracer "attr fields"  <$> mapM makeAttrType attrs
    childFields :: [TyField] <- tracer "child fields" <$>
                                  case content of -- serving only simple Seq of elts or choice of elts for now
                              -- These would be in ElementType namespace.
      Seq    ls -> seqInstance ls
      All    ls -> seqInstance ls -- handling the same way
      Choice ls -> (:[]) <$> makeAltType ls
      Elt     e -> error  $ "Unexpected singular Elt inside content of ComplexType: " <> show e
    declareAlgebraicType (TyData myTypeName, [(TyCon myConsName, attrFields <> childFields)])
    return      myTypeName
  where
    makeAttrType :: Attr -> CG TyField
    makeAttrType Attr {..} = second (\(TyType bs) -> TyType $ wrapAttr use bs) <$> makeFieldType aName aType
    makeFieldType :: XMLString -> Type -> CG TyField
    makeFieldType  aName aType = (,) <$> (TyFieldName <$> translate (AttributeName, TargetFieldName) eName aName)
                                     <*> (TyType      <$> generateContentType                        eName aType)
    makeAltType :: [TyPart] -> CG TyField
    makeAltType ls = do
      warn [qc|"altType not yet implemented: {ls}|]
      return (TyFieldName "altFields", TyType "Xeno.Node")
    seqInstance = mapM fun
      where
        fun (Elt (elt@(Element {}))) = do
          generateElementInstance eName elt
        fun  x = error [qc|Type {eName}: not yet implemented nested sequence, all or choice: {x}|]
generateContentType eName (Restriction _ (Enum (uniq -> values))) = do
  tyName     <- translate (SchemaType ,   TargetTypeName) eName        eName -- should it be split into element and type containers?
  translated <- translate (EnumIn eName,  TargetConsName) eName `mapM` values
  -- ^ TODO: consider enum as indexed family of spaces
  declareSumType (TyData tyName, (\con -> (TyCon con, TyType "")) <$> translated)
  return tyName
generateContentType eName (Restriction base (Pattern _)) = do
  tyName   <- translate (ElementName, TargetTypeName) (eName <> "pattern") base
  consName <- translate (ElementName, TargetConsName) (eName <> "pattern") base
  baseTy   <- translate (SchemaType,  TargetTypeName)  eName               base
  warn "-- Restriction pattern"
  declareNewtype (TyData tyName) (TyCon consName) (TyType baseTy)
  return tyName
generateContentType eName (Extension   base (Complex False [] (Seq []))) = do
  tyName   <- translate (SchemaType,  TargetTypeName) base eName
  consName <- translate (ElementName, TargetConsName) base eName
  baseTy   <- translate (SchemaType,  TargetTypeName) base eName
  declareNewtype (TyData tyName) (TyCon consName) (TyType baseTy)
  return tyName
generateContentType eName  (Restriction base  None      ) =
  -- Should we do `newtype` instead?
  generateContentType eName $ Ref base
generateContentType eName (Extension   base  (cpl@Complex {inner=Seq []})) = do
  -- TODO Unite with next code
  superTyLabel <- translate (SchemaType,TargetFieldName) eName "Super" -- should be: MetaKey instead of SchemaType
  generateContentType eName $ cpl
                  `appendElt` Element {eName=builderString superTyLabel
                                      ,eType=Ref base
                                      ,maxOccurs=MaxOccurs 1
                                      ,minOccurs=1
                                      ,targetNamespace=""}
generateContentType eName ex@(Extension _    (Complex {inner=Seq{}})) =
    getExtendedType ex >>= generateContentType eName
generateContentType eName (Extension   _base  _otherType) = do
    error [qc|Can't generate extension "{eName}"|]


getExtendedType :: Type -> CG Type
getExtendedType (Extension base cpl@Complex {inner=Seq {}}) = do
  -- TODO resolve right naming of superTyLabel
  -- superTyLabel <- translate (SchemaType,TargetFieldName) eName ("Super" <> base) -- should be: MetaKey instead of SchemaType
  let superTyLabel = "super" <> normalizeTypeName base
  return $ cpl `appendElt` Element {eName=superTyLabel
                                   ,eType=Ref base
                                   ,maxOccurs=MaxOccurs 1
                                   ,minOccurs=1
                                   ,targetNamespace=""}
getExtendedType (Extension {}) = error "TODO"
getExtendedType _              = error "'getExtendedType' is available only for Extension"


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
parserCodegen sch = do
    --putStrLn "~~~~~~ Schema: ~~~~~~~~~"
    --pPrint sch
    --putStrLn "~~~~~~~~~~~~~~~~~~~~~~~~"
    codegen' sch (generateParser1 sch)


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
    -- TODO add codegen to parser
    outCodeLine "{-# LANGUAGE OverloadedStrings #-}"
    outCodeLine "{-# LANGUAGE RankNTypes #-}"
    outCodeLine "{-# LANGUAGE LambdaCase #-}"
    outCodeLine "{-# LANGUAGE DeriveGeneric #-}"
    outCodeLine "{-# LANGUAGE DeriveAnyClass #-}"
    outCodeLine "{-# LANGUAGE RecordWildCards #-}"
    -- TODO also add in parser generator
    --
    --
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
-- TODO use from library
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
generateParser1 schema = do
    generateSchema schema
    outCodeLine [qc|-- PARSER --|]
    generateParserInternalStructures schema
    generateParserInternalArray schema
    outCodeLine ""
    outCodeLine ""
    outCodeLine "-- extr --"
    outCodeLine ""
    outCodeLine ""
    generateParserExtractTopLevel schema
    outCodeLine ""
    outCodeLine ""
    outCodeLine "-- parser --"
    outCodeLine ""
    outCodeLine ""
    generateParserTop schema
    generateAuxiliaryFunctions schema


generateParserInternalStructures :: Schema -> CG ()
generateParserInternalStructures Schema{..} = do
    outCodeLine [qc|-- | Internal representation of TopLevel|]
    outCodeLine [qc|data TopLevelInternal = TopLevelInternal !ByteString !(UV.Vector Int) deriving (G.Generic, NFData, Show)|] -- TODO qualify all imports to avoid name clush
    outCodeLine ""


data Repeatedness = RepMaybe
                  | RepOnce
                  | RepMany
                  | RepNotLess Int
                  | RepRange Int Int
                  deriving Show


eltToRepeatedness :: Element -> Repeatedness
eltToRepeatedness (Element 0 Unbounded     _ _ _) = RepMany
eltToRepeatedness (Element 0 (MaxOccurs 1) _ _ _) = RepMaybe
eltToRepeatedness (Element 0 _             _ _ _) = RepMany
eltToRepeatedness (Element 1 (MaxOccurs 1) _ _ _) = RepOnce
eltToRepeatedness (Element 1 _             _ _ _) = RepMany
eltToRepeatedness (Element m Unbounded     _ _ _) = RepNotLess m
eltToRepeatedness (Element m (MaxOccurs n) _ _ _) = RepRange m n


-- TODO use `baseTranslations`
getParserForStandardXsd :: XMLString -> Maybe XMLString
getParserForStandardXsd "xs:integer"            = Just "Integer"
getParserForStandardXsd "xs:int"                = Just "Int"
getParserForStandardXsd "xs:byte"               = Just "Integer"
getParserForStandardXsd "xs:long"               = Just "Int64"
getParserForStandardXsd "xs:negativeInteger"    = Just "Integer"
getParserForStandardXsd "xs:nonNegativeInteger" = Just "Integer"
getParserForStandardXsd "xs:positiveInteger"    = Just "Integer"
getParserForStandardXsd "xs:nonPositiveInteger" = Just "Integer"
getParserForStandardXsd "xs:short"              = Just "Integer"
getParserForStandardXsd "xs:unsignedLong"       = Just "Integer"
getParserForStandardXsd "xs:unsignedInt"        = Just "Integer"
getParserForStandardXsd "xs:unsignedShort"      = Just "Integer"
getParserForStandardXsd "xs:unsignedByte"       = Just "Integer"
getParserForStandardXsd "xs:stringContent"      = Just "String"
getParserForStandardXsd "xs:decimal"            = Just "Decimal"
getParserForStandardXsd "xs:string"             = Just "String"
getParserForStandardXsd "xs:token"              = Just "String"
getParserForStandardXsd "xs:date"               = Just "Day"
getParserForStandardXsd "xs:duration"           = Just "Duration" -- TODO
getParserForStandardXsd "xs:dateTime"           = Just "DateTime"
getParserForStandardXsd "xs:gYearMonth"         = Just "Day"
getParserForStandardXsd "xs:gMonth"             = Just "Day"
getParserForStandardXsd "xs:gYear"              = Just "Day"
getParserForStandardXsd "xs:boolean"            = Just "Boolean"
getParserForStandardXsd "xs:anyURI"             = Just "String" -- TODO
getParserForStandardXsd _                       = Nothing


generateParserInternalArray :: Schema -> CG ()
generateParserInternalArray Schema{..} = do
    outCodeLine [qc|-- PARSER --|]
    when (length tops /= 1) $ error "tops must contain only one element"
    let topEl = head tops
    -- Generate parser header
    let topName = eName topEl
    when (minOccurs topEl /= 1) $ error [qc|Wrong minOccurs = {minOccurs topEl}|]
    when (maxOccurs topEl /= MaxOccurs 1) $ error [qc|Wrong maxOccurs = {maxOccurs topEl}|]
    outCodeLine' [qc|parseTopLevelToArray :: ByteString -> Either String TopLevelInternal|]
    outCodeLine' [qc|parseTopLevelToArray bs = Right $ TopLevelInternal bs $ UV.create $ do|]
    withIndent $ do
        outCodeLine' [qc|vec <- UMV.new ((max 1 (BS.length bs `div` 7)) * 2)|] -- TODO back to UMV.unsafeNew
        outCodeLine' [qc|parse{topName} vec|]
        outCodeLine' [qc|return vec|]
        outCodeLine' [qc|where|]
        withIndent $ do
            outCodeLine' [qc|parse{topName} :: forall s . UMV.STVector s Int -> ST s ()|]
            outCodeLine' [qc|parse{topName} vec = do|]
            withIndent $ do
                outCodeLine' [qc|UMV.unsafeWrite vec (0::Int) (0::Int)|]
                outCodeLine' [qc|(_, _) <- inOneTag "{topName}" (skipSpaces $ skipHeader $ skipSpaces 0) $ parse{topName}Content 0|]
                outCodeLine' [qc|return ()|]
                outCodeLine' [qc|where|]
                withIndent $ do
                    -- Generate parsers for certain types
                    let additionalTypes = extractAdditionalTypes tops -- TODO filter out repeated types
                    forM_ ((Map.toList types) ++ additionalTypes) $ \(typeName, ty) -> do
                        outCodeLine' [qc|parse{typeName}Content arrStart strStart = do|]
                        withIndent $ generateContentParserIA typeName ty
                    -- Generate auxiliary functions
                    generateAuxiliaryFunctionsIA
  where
    generateElementsOfComplexParser :: (XMLString, XMLString) -> [Element] -> CG (XMLString, XMLString)
    generateElementsOfComplexParser (arrStart, strStart) elements = do
        let ofsNames' = ((arrStart, strStart) : [ ( [qc|arrOfs{i}|], [qc|strOfs{i}|]) | i <- [(1::Int)..]])
                        :: [(XMLString, XMLString)]
            ofsNames = zip ofsNames' (tail ofsNames')
            endNum = length elements
        forM_ (zip ofsNames elements) $ \(((arrOfs, strOfs), (arrOfs', strOfs')), el) -> do
            let parserName = getParserName (eType el) (eName el)
                (isUseArrOfs, tagQuantifier::XMLString) = case eltToRepeatedness el of
                    RepMaybe      -> (True,  "inMaybeTag")
                    RepOnce       -> (False, "inOneTag")
                    _             -> (True,  "inManyTags")
                (arrOfs1, arrOfs2)::(XMLString,XMLString) =
                    if isUseArrOfs then ([qc| {arrOfs}|],"") else ("", [qc| {arrOfs}|])
            -- TODO parse with attributes!
            outCodeLine' [qc|({arrOfs'}, {strOfs'}) <- {tagQuantifier} "{eName el}"{arrOfs1} {strOfs} $ parse{parserName}{arrOfs2}|]
        return $ fst $ ofsNames !! endNum
    ofsToReturn :: (XMLString, XMLString) -> CG ()
    ofsToReturn (arrLastOfs, strLastOfs) = outCodeLine' [qc|return ({arrLastOfs}, {strLastOfs})|]
    generateContentParserIA typeName ty = do
        case ty of
            Complex _ _attrs (Seq elts) -> do
                generateElementsOfComplexParser ("arrStart", "strStart") (onlyElements elts) >>= ofsToReturn
            c@(Complex _ _attrs (Choice _elts)) -> do
                -- XXX
                outCodeLine' [qc|-- Complex: {c}|]
            r@(Ref {}) -> do
                let parserName = getParserName r ""
                outCodeLine' [qc|parse{parserName} arrStart strStart -- !! <{typeName}> / <{ty}>|]
            Restriction _ _ ->
                outCodeLine' [qc|parseString arrStart strStart|]
            Extension base (Complex {inner=Seq exFields}) -> do
                let baseParserName = fromMaybe [qc|{base}Content|] $ getParserForStandardXsd base
                outCodeLine' [qc|(arrOfs', strOfs') <- parse{baseParserName} arrStart strStart|]
                generateElementsOfComplexParser ("arrOfs'", "strOfs'") (onlyElements exFields) >>= ofsToReturn
            _ -> error [qc|Unsupported type: {ty}|]
      where
        onlyElements :: [TyPart] -> [Element]
        onlyElements = map (\case (Elt e) -> e ; _ -> error [qc|Unsupported type: {take 100 $ show ty}|])
    -- TODO unite with extractor
    getParserName :: Type -> XMLString -> XMLString
    getParserName (Ref r) _ =
        case getParserForStandardXsd r of
            Nothing ->
                if "xs:" `BS.isPrefixOf` r
                then (error [qc|Standard type `{r}` is not supported|])
                else [qc|{r}Content|]
            Just rr -> rr
    getParserName (Complex {}) xname              = [qc|{xname}Content|]
    getParserName t _                             = [qc|???{t}|]
    extractAdditionalTypes :: [Element] -> [(XMLString, Type)]
    extractAdditionalTypes elts =
        let allElts = (universeBi elts :: [Element])
        in map (\(Element _ _ name typ _) -> (name, typ)) allElts
    generateAuxiliaryFunctionsIA = do
        --
        -- TODO read this from file!
        --
        outCodeLine' [qc|toError tag strOfs act = do|]
        outCodeLine' [qc|    act >>= \case|]
        outCodeLine' [qc|        Nothing -> failExp ("<" <> tag) strOfs|]
        outCodeLine' [qc|        Just res -> return res|]
        outCodeLine' [qc|inOneTag          tag strOfs inParser = toError tag strOfs $ inOneTag' True tag strOfs inParser|] -- TODO add attributes processing
        outCodeLine' [qc|inOneTagWithAttrs tag strOfs inParser = toError tag strOfs $ inOneTag' True  tag strOfs inParser|]
        outCodeLine' [qc|inOneTag' hasAttrs tag strOfs inParser = do|]
        outCodeLine' [qc|    case ensureTag hasAttrs tag (skipToOpenTag strOfs + 1) of|]
        outCodeLine' [qc|        Nothing -> return Nothing|]
        outCodeLine' [qc|        Just (ofs', True) -> do|]
        outCodeLine' [qc|            (arrOfs, strOfs) <- inParser (ofs' - 1)|] -- TODO points to special unparseable place
        outCodeLine' [qc|            return $ Just (arrOfs, ofs')|]
        outCodeLine' [qc|        Just (ofs', _) -> do|]
        outCodeLine' [qc|            (arrOfs, strOfs) <- inParser ofs'|]
        outCodeLine' [qc|            let ofs'' = skipToOpenTag strOfs|]
        outCodeLine' [qc|            if bs `BSU.unsafeIndex` (ofs'' + 1) == slashChar then do|]
        outCodeLine' [qc|                case ensureTag False tag (ofs'' + 2) of|]
        outCodeLine' [qc|                    Nothing     -> return Nothing|]
        outCodeLine' [qc|                    Just (ofs''', _) -> return $ Just (arrOfs, ofs''')|]
        outCodeLine' [qc|            else do|]
        outCodeLine' [qc|                return Nothing|]
        -- ~~~~~~~~
        outCodeLine' [qc|inMaybeTag tag arrOfs strOfs inParser = inMaybeTag' True tag arrOfs strOfs inParser|] -- TODO add attributes processing
        outCodeLine' [qc|inMaybeTag' hasAttrs tag arrOfs strOfs inParser = do|]
        outCodeLine' [qc|    inOneTag' hasAttrs tag strOfs (inParser $ arrOfs + 1) >>= \case|]
        outCodeLine' [qc|        Just res -> do|]
        outCodeLine' [qc|            UMV.unsafeWrite vec arrOfs 1|]
        outCodeLine' [qc|            return res|]
        outCodeLine' [qc|        Nothing -> do|]
        outCodeLine' [qc|            UMV.unsafeWrite vec arrOfs 0|]
        outCodeLine' [qc|            return (arrOfs + 1, strOfs)|]
        outCodeLine' [qc|inManyTags tag arrOfs strOfs inParser = inManyTags' True tag arrOfs strOfs inParser|] -- TODO add attributes processing
        outCodeLine' [qc|inManyTagsWithAttrs tag arrOfs strOfs inParser = inManyTags' True tag arrOfs strOfs inParser|]
        outCodeLine' [qc|inManyTags' hasAttrs tag arrOfs strOfs inParser = do|]
        outCodeLine' [qc|    (cnt, endArrOfs, endStrOfs) <- flip fix (0, (arrOfs + 1), strOfs) $ \next (cnt, arrOfs', strOfs') ->|]
        outCodeLine' [qc|        inOneTag' hasAttrs tag strOfs' (inParser arrOfs') >>= \case|]
        outCodeLine' [qc|            Just (arrOfs'', strOfs'') -> next   (cnt + 1, arrOfs'', strOfs'')|]
        outCodeLine' [qc|            Nothing                   -> return (cnt,     arrOfs', strOfs')|]
        outCodeLine' [qc|    UMV.unsafeWrite vec arrOfs cnt|]
        outCodeLine' [qc|    return (endArrOfs, endStrOfs)|]
        -- ~~~~~~~~
        outCodeLine' [qc|ensureTag True expectedTag ofs|]
        outCodeLine' [qc|  | expectedTag `BS.isPrefixOf` (BS.drop ofs bs) =|]
        outCodeLine' [qc|      if bs `BSU.unsafeIndex` ofsToEnd == closeTagChar|]
        outCodeLine' [qc|        then Just (ofsToEnd + 1, False)|]
        outCodeLine' [qc|      else if isSpaceChar (bs `BSU.unsafeIndex` ofsToEnd)|]
        outCodeLine' [qc|        then let ofs' = skipToCloseTag (ofs + BS.length expectedTag)|]
        outCodeLine' [qc|             in Just (ofs' + 1, bs `BSU.unsafeIndex` (ofs' - 1) == slashChar)|]
        outCodeLine' [qc|      else|]
        outCodeLine' [qc|        Nothing|]
        outCodeLine' [qc|  | otherwise = Nothing|]
        outCodeLine' [qc|  where ofsToEnd = ofs + BS.length expectedTag|]
        outCodeLine' [qc|ensureTag False expectedTag ofs|]
        outCodeLine' [qc|  | expectedTag `BS.isPrefixOf` (BS.drop ofs bs) && (bs `BSU.unsafeIndex` ofsToEnd == closeTagChar)|]
        outCodeLine' [qc|        = Just (ofsToEnd + 1, False)|]
        outCodeLine' [qc|  | otherwise|]
        outCodeLine' [qc|        = Nothing|]
        outCodeLine' [qc|  where ofsToEnd = ofs + BS.length expectedTag|]
        outCodeLine' [qc|failExp expStr ofs = fail $ BSC.unpack ("Expected '" <> expStr <> "' but got '" <> ptake bs ofs (BS.length expStr + 100) <> "'")|]
        outCodeLine' [qc|ptake :: ByteString -> Int -> Int -> ByteString|]
        outCodeLine' [qc|ptake bs ofs len = BS.take len $ BS.drop ofs bs -- TODO replace with UNSAFE?|]
        outCodeLine' [qc|--|]
        outCodeLine' [qc|parseString arrStart strStart = do|]
        outCodeLine' [qc|  let strEnd = skipToOpenTag strStart|]
        outCodeLine' [qc|  UMV.unsafeWrite vec arrStart     strStart|]
        outCodeLine' [qc|  UMV.unsafeWrite vec (arrStart+1) (strEnd - strStart)|]
        outCodeLine' [qc|  return (arrStart+2, strEnd)|]
        outCodeLine' [qc|parseDecimal = parseString|]
        outCodeLine' [qc|parseDateTime = parseString|]
        outCodeLine' [qc|parseDuration = parseString|]
        outCodeLine' [qc|parseInteger = parseString|]
        outCodeLine' [qc|parseInt = parseString|]
        outCodeLine' [qc|parseInt64 = parseString|]
        outCodeLine' [qc|parseDay = parseString|]
        outCodeLine' [qc|parseBoolean = parseString|]
        outCodeLine' [qc|skipSpaces ofs|]
        outCodeLine' [qc|  | isSpaceChar (BSU.unsafeIndex bs ofs) = skipSpaces (ofs + 1)|]
        outCodeLine' [qc|  | otherwise = ofs|]
        outCodeLine' [qc|isSpaceChar :: Word8 -> Bool|]
        outCodeLine' [qc|isSpaceChar c = c == 32 || c == 10 || c == 9 || c == 13|]
        outCodeLine' [qc|skipHeader :: Int -> Int|]
        outCodeLine' [qc|skipHeader ofs|]
        outCodeLine' [qc|  | bs `BSU.unsafeIndex` ofs == openTagChar && bs `BSU.unsafeIndex` (ofs + 1) == questionChar = skipToCloseTag (ofs + 2) + 1|]
        outCodeLine' [qc|  | otherwise = ofs|]
        outCodeLine' [qc|slashChar    = 47 -- '<'|]
        outCodeLine' [qc|openTagChar  = 60 -- '<'|]
        outCodeLine' [qc|closeTagChar = 62 -- '>'|]
        outCodeLine' [qc|questionChar = 63 -- '?'|]
        outCodeLine' [qc|skipToCloseTag :: Int -> Int|]
        outCodeLine' [qc|skipToCloseTag ofs|]
        outCodeLine' [qc|  | bs `BSU.unsafeIndex` ofs == closeTagChar = ofs|]
        outCodeLine' [qc|  | otherwise = skipToCloseTag (ofs + 1)|]
        outCodeLine' [qc|skipToOpenTag :: Int -> Int|]
        outCodeLine' [qc|skipToOpenTag ofs|]
        outCodeLine' [qc|  | bs `BSU.unsafeIndex` ofs == openTagChar = ofs|]
        outCodeLine' [qc|  | otherwise = skipToOpenTag (ofs + 1)|]


generateParserExtractTopLevel :: Schema -> CG ()
generateParserExtractTopLevel Schema{..} = do
    forM_ tops $ \topEl -> do
        let rootName = eName topEl
        haskellRootName <- translate (ElementName, TargetTypeName) "" rootName -- TODO container?
        outCodeLine' [qc|extractTopLevel :: TopLevelInternal -> TopLevel|]
        outCodeLine' [qc|extractTopLevel (TopLevelInternal bs arr) = fst $ extract{haskellRootName}Content 0|]
    withIndent $ do
        outCodeLine' "where"
        let additionalTypes = extractAdditionalTypes tops -- TODO filter out repeated types
        withIndent $ do
            forM_ ((Map.toList types) ++ additionalTypes) $ \(typeName, ty) -> do
            -- forM_ (take 1 ((Map.toList types) ++ additionalTypes)) $ \(typeName, ty) -> do
                -- OK: haskellTypeName <- translate (SchemaType, TargetTypeName) typeName typeName -- TODO container?
                haskellTypeName <- translate (ElementName, TargetTypeName) typeName typeName -- TODO container?
                --pTraceM [qc|Extractor: typeName = <{typeName}>|]
                --pTraceM [qc|Extractor: ty = <{take 150 $ show ty}>|]
                --pTraceM [qc|Extractor: haskellTypeName = <{haskellTypeName}>|]
                outCodeLine' [qc|extract{haskellTypeName}Content ofs =|]
                withIndent $ generateContentParser typeName haskellTypeName ty
            generateAuxiliaryFunctions
  where
    generateContentParser typeName haskellTypeName ty = do
        case ty of
            Complex _ attrs (Seq elts) -> do
                withIndent $ do
                    let elements = map (\case (Elt e) -> e ; _ -> error [qc|Unsupported type: {take 100 $ show ty}|]) elts
                    -- Output attributes reader
                    forM_ attrs $ \attr -> do
                        let aname = aName attr
                        haskellAttrName <- translate (AttributeName, TargetFieldName) aname aname -- TODO container?
                        outCodeLine' [qc|let {haskellAttrName} = Nothing in|]
                    -- Output fields reader
                    forM_ (zip elements [(1::Int)..]) $ \(el, ofsIdx) -> do
                        extractorName <- getExtractorName (eType el) (eName el)
                        let ofs = if ofsIdx == 1 then ("ofs"::XMLString) else [qc|ofs{ofsIdx - 1}|]
                            -- TODO add extractExact support
                            (fieldQuantifier::(Maybe XMLString)) = case eltToRepeatedness el of
                                RepMaybe -> Just "extractMaybe"
                                RepOnce  -> Nothing
                                _        -> Just "extractMany"
                            (extractor::XMLString) = case fieldQuantifier of
                                     Nothing -> [qc|extract{extractorName}Content {ofs}|]
                                     Just qntf -> [qc|{qntf} {ofs} extract{extractorName}Content|]
                        let fieldName' = eName el
                        fieldName <- translate (ElementName, TargetFieldName) fieldName' fieldName' -- TODO container?
                        outCodeLine' [qc|let ({fieldName}, ofs{ofsIdx}) = {extractor} in|]
                    let ofs' = if null elements then "ofs" else [qc|ofs{length elements}|]::XMLString
                    haskellConsName <- translate (SchemaType, TargetConsName) typeName typeName -- TODO container?
                    outCodeLine' [qc|({haskellConsName}\{..}, {ofs'})|]
            r@(Ref {}) -> do
                rname <- getExtractorName r ""
                outCodeLine' [qc|extract{rname}Content ofs|]
            Restriction _ (Enum opts) -> do
                outCodeLine' [qc|first (\case|]
                withIndent $ do
                    forM_ (uniq opts) $ \opt -> do
                        tn <- translate (EnumIn typeName, TargetConsName) typeName opt -- TODO change 'typeName' to 'haskellTypeName' ?
                        outCodeLine' [qc|"{opt}" -> {tn}|]
                    outCodeLine' [qc|) $ extractStringContent ofs|]
            Restriction baseType _ -> do
                postProcess <- if baseType == "xs:string" || baseType == "xs:token"
                               then do
                                   haskellConsName <- translate (SchemaType, TargetConsName) typeName typeName -- TODO container?
                                   return [qc|first {haskellConsName} $ |]
                               else return (""::XMLString)
                outCodeLine' [qc|{postProcess}extractStringContent ofs|]
            e@(Extension _ _) -> do
                getExtendedType e >>= generateContentParser typeName haskellTypeName
            c@(Complex _ _attrs (Choice _elts)) -> do
                -- XXX
                outCodeLine' [qc|-- Complex / Choice: {c}|] -- XXX
            _ -> error [qc|Unsupported type: {show ty}|]
    getExtractorName :: Type -> XMLString -> CG B.Builder
    getExtractorName (Ref r) _                =
        case getParserForStandardXsd r of
            Nothing ->
                if "xs:" `BS.isPrefixOf` r
                then (error [qc|Standard type `{r}` is not supported|])
                else translate (ElementName, TargetTypeName) r r -- TODO container?
            Just rr -> return $ B.byteString rr
    getExtractorName (Complex {}) xname           = return $ B.byteString xname
    getExtractorName (Extension {}) xname         = return $ "??Extension??:" <> B.byteString xname
    getExtractorName t _                          = error [qc|Don't know how to generate {take 100 $ show t}|]
    extractAdditionalTypes :: [Element] -> [(XMLString, Type)]
    extractAdditionalTypes elts =
        let allElts = (universeBi elts :: [Element])
        in map (\(Element _ _ name typ _) -> (name, typ)) allElts
    generateAuxiliaryFunctions = do
        outCodeLine' [qc|extractStringContent :: Int -> (ByteString, Int)|]
        outCodeLine' [qc|extractStringContent ofs = (BSU.unsafeTake bslen (BSU.unsafeDrop bsofs bs), ofs + 2)|]
        outCodeLine' [qc|  where|]
        outCodeLine' [qc|    bsofs = arr `UV.unsafeIndex` ofs|]
        outCodeLine' [qc|    bslen = arr `UV.unsafeIndex` (ofs + 1)|]
        outCodeLine' [qc|extractMaybe ofs subextr|]
        outCodeLine' [qc|  | arr `UV.unsafeIndex` ofs == 0 = (Nothing, ofs + 1)|]
        outCodeLine' [qc|  | otherwise                     = first Just $ subextr (ofs + 1)|]
        outCodeLine' [qc|extractMany ofs subextr = extractMany' (ofs + 1) (arr `UV.unsafeIndex` ofs)|]
        outCodeLine' [qc|  where|]
        outCodeLine' [qc|    extractMany' ofs 0   = ([], ofs)|]
        outCodeLine' [qc|    extractMany' ofs len =|]
        outCodeLine' [qc|      let (v, ofs') = subextr ofs|]
        outCodeLine' [qc|      in first (v:) $ extractMany' ofs' (len - 1)|]
        outCodeLine' [qc|extractTokenContent = extractStringContent|]
        outCodeLine' [qc|extractDateTimeContent :: Int -> (ZonedTime, Int)|]
        outCodeLine' [qc|extractDateTimeContent = first zonedTimeStr . extractStringContent|]
        outCodeLine' [qc|extractDayContent :: Int -> (Day, Int)|]
        outCodeLine' [qc|extractDayContent = undefined|]
        outCodeLine' [qc|extractDurationContent :: Int -> (Duration, Int)|]
        outCodeLine' [qc|extractDurationContent = undefined|]
        outCodeLine' [qc|extractDecimalContent :: Int -> (Scientific, Int)|]
        outCodeLine' [qc|extractDecimalContent = first (read . BSC.unpack) . extractStringContent|]
        outCodeLine' [qc|extractIntegerContent :: Int -> (Integer, Int)|]
        outCodeLine' [qc|extractIntegerContent = first (read . BSC.unpack) . extractStringContent|]
        outCodeLine' [qc|extractIntContent :: Int -> (Int, Int)|]
        outCodeLine' [qc|extractIntContent = first (read . BSC.unpack) . extractStringContent|]
        outCodeLine' [qc|extractInt64Content :: Int -> (Int64, Int)|]
        outCodeLine' [qc|extractInt64Content = first (read . BSC.unpack) . extractStringContent|]
        outCodeLine' [qc|extractBooleanContent :: Int -> (Bool, Int)|]
        outCodeLine' [qc|extractBooleanContent ofs = first (\case|]
        outCodeLine' [qc|    "true" -> True|]
        outCodeLine' [qc|    "1"    -> True|]
        outCodeLine' [qc|    _      -> False|]
        outCodeLine' [qc|    ) $ extractStringContent ofs|]
        outCodeLine' [qc|first f (a,b) = (f a, b)|]


generateAuxiliaryFunctions :: Schema -> CG ()
generateAuxiliaryFunctions _schema = do
    outCodeLine' ""
    outCodeLine' ""
    outCodeLine' [qc|zonedTimeStr :: ByteString -> ZonedTime|]
    outCodeLine' [qc|zonedTimeStr = runIdentity . parseTimeM True defaultTimeLocale fmt . BSC.unpack|]
    outCodeLine' [qc|  where|]
    outCodeLine' [qc|    fmt = iso8601DateFormat (Just "%H:%M:%S")|]
    outCodeLine' "{-# INLINE zonedTimeStr #-}"


generateParserTop :: Schema -> CG ()
generateParserTop _schema = do
    outCodeLine "parser :: ByteString -> Either String TopLevel" -- TODO
    outCodeLine "parser = fmap extractTopLevel . parseTopLevelToArray"

