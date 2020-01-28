{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE ViewPatterns              #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
-- | Here we aim to analyze the schema.
module CodeGen(codegen, parserCodegen) where


import           Prelude                           hiding (id, lookup)

import           Control.Arrow
import           Control.Monad
import           Control.Monad                     (forM, forM_, when)
import qualified Data.ByteString.Builder           as B
import qualified Data.ByteString.Char8             as BS
import qualified Data.Map.Strict                   as Map
import           Data.Maybe
import qualified Data.Set                          as Set
import           Data.String
import qualified Language.Haskell.TH               as TH
import           Text.InterpolatedString.Perl6     (qc)

import           FromXML                           (XMLString)

import           BaseTypes
import           CodeGenMonad
import           Schema
import           TypeDecls

import           Data.Generics.Uniplate.Operations

import           Debug.Pretty.Simple
import           Text.Pretty.Simple


import           Identifiers


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

generateGroupType :: XMLString -- group name
                  -> CG TyField
generateGroupType groupName =
    (,) <$> (TyFieldName <$> trans TargetFieldName)
        <*> (TyType      <$> trans TargetTypeName )
  where
    trans tgt = translate (SchemaGroup, tgt) groupName groupName

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


-- | `escapeSpaces` envelop type declaration into braces if it contains spaces,
--   for example "Maybe Integer" trasforms to "(Maybe Integer)"
escapeSpaces :: TyType -> TyType
escapeSpaces ty@(TyType tyType@(builderString -> tyTypeStr))
    | BS.elem ' ' tyTypeStr = TyType $ "(" <> tyType <> ")"
    | otherwise             = ty


-- | Given a container with ComplexType details (attributes and children),
--   generate the type to hold them.
--   Or if it turns out these are referred types - just return their names.
--   That means that our container is likely 'SchemaType' namespace
generateContentType :: XMLString -- container name
                    -> Type -> CG B.Builder
generateContentType container (Ref (tyName)) = translate (SchemaType, TargetTypeName) container tyName
  -- TODO: check if the type was already translated (as it should, if it was generated)
generateContentType eName cpl@(Complex {attrs, inner}) = do
    myTypeName  <- translate (SchemaType, TargetTypeName) eName eName
    myConsName  <- translate (SchemaType, TargetConsName) eName eName
    attrFields  :: [TyField] <- tracer "attr fields"  <$> mapM makeAttrType attrs
    case inner of
        Seq ls -> do
            childFields <- seqInstance ls
            declareAlgebraicType (TyData myTypeName, [(TyCon myConsName, attrFields <> childFields)])
            return myTypeName
        All ls -> do
            -- Handling the same way as `Seq`
            generateContentType eName (cpl {inner = Seq ls})
        Choice ls -> do
            unless (null attrFields) $ warn [qc|Type {eName}: attributes in 'xs:choice' are unsupported!|]
            childFields <- choiceInstance ls
            declareSumType (TyData myTypeName, childFields)
            return myTypeName
        Elt e -> error  $ "Unexpected singular Elt inside content of ComplexType: " <> show e
        Group gName -> error $ "Did not yet implement complexType referring only to the xs:group " <> show gName
  where
    makeAttrType :: Attr -> CG TyField
    makeAttrType Attr {..} = second (\(TyType bs) -> TyType $ wrapAttr use bs) <$> makeFieldType aName aType
    makeFieldType :: XMLString -> Type -> CG TyField
    makeFieldType  aName aType = (,) <$> (TyFieldName <$> translate (AttributeName, TargetFieldName) eName aName)
                                     <*> (TyType      <$> generateContentType                        eName aType)
    seqInstance = mapM fun
      where
        fun (Elt (elt@(Element {}))) = do
          generateElementInstance eName elt
        fun (Group gName) =
          generateGroupType gName
        fun  x =
          error [qc|Type {eName}: not yet implemented nested sequence, all or choice: {x}|]
    choiceInstance :: [TyPart] -> CG [SumAlt]
    choiceInstance ls = do
        elts <- catMaybes <$> (forM ls $ \case -- TODO move to `forM`
            Elt e -> return $ Just e
            x     -> warn [qc|Type {eName}: nested types not supported yet // {x}|] >> return Nothing)
        forM elts $ \elt@Element{eName=tName} -> do
            tyName <- translate (ChoiceIn eName, TargetConsName) eName tName
            (_, tyType) <- generateElementInstance tName elt
            return (TyCon tyName, escapeSpaces tyType)
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
  superTyLabel <- builderString <$> translate (SchemaType,TargetFieldName) base "Super" -- should be: MetaKey instead of SchemaType
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
    -- pTraceShowM schema
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
                    let additionalTypes = [] -- extractAdditionalTypes tops -- TODO filter out repeated types
                    forM_ ((Map.toList types) ++ additionalTypes) $ \(typeName, ty) -> do
                        outCodeLine' [qc|parse{typeName}Content arrStart strStart = do|]
                        withIndent $ generateContentParserIA typeName ty
                    -- Generate auxiliary functions
                    generateAuxiliaryFunctionsIA
  where
    generateElementsOfComplexParser :: (XMLString, XMLString) -> [TyPart] -> CG (XMLString, XMLString)
    generateElementsOfComplexParser (arrStart, strStart) typarts = do
        let ofsNames' = ((arrStart, strStart) : [ ( [qc|arrOfs{i}|], [qc|strOfs{i}|]) | i <- [(1::Int)..]])
                        :: [(XMLString, XMLString)]
            ofsNames = zip ofsNames' (tail ofsNames')
            endNum = length typarts
        forM_ (zip ofsNames typarts) $ \(((arrOfs, strOfs), (arrOfs', strOfs')), typart) -> do
            case typart of
                Elt el -> do
                    let parserName = getParserName (eType el) (eName el)
                        (isUseArrOfs, tagQuantifier::XMLString) = case eltToRepeatedness el of
                            RepMaybe -> (True,  "inMaybeTag")
                            RepOnce  -> (False, "inOneTag")
                            _        -> (True,  "inManyTags")
                        (arrOfs1, arrOfs2)::(XMLString,XMLString) =
                            if isUseArrOfs then ([qc| {arrOfs}|],"") else ("", [qc| {arrOfs}|])
                    -- TODO parse with attributes!
                    outCodeLine' [qc|({arrOfs'}, {strOfs'}) <- {tagQuantifier} "{eName el}"{arrOfs1} {strOfs} $ parse{parserName}{arrOfs2}|]
                Group gName ->
                    outCodeLine' [qc|({arrOfs'}, {strOfs'}) <- parse{gName}Content {arrOfs} {strOfs}|]
                _ -> error [qc|Unsupported type: {take 100 $ show typart}|]
        return $ fst $ ofsNames !! endNum
    ofsToReturn :: (XMLString, XMLString) -> CG ()
    ofsToReturn (arrLastOfs, strLastOfs) = outCodeLine' [qc|return ({arrLastOfs}, {strLastOfs})|]
    generateContentParserIA typeName ty = do
        case ty of
            Complex _ _attrs (Seq elts) -> do
                generateElementsOfComplexParser ("arrStart", "strStart") elts >>= ofsToReturn
            c@(Complex _ _attrs (Choice choices)) -> do
                -- outCodeLine' [qc|-- Complex: {c}|]
                outCodeLine' [qc|let arrOfs' = arrStart + 1|]
                outCodeLine' [qc|case (getTagName strStart) of|]
                withIndent $ do
                    -- TODO what to do if there are two options with minOccurs="0" ?
                    forM_ (zip choices [0..]) $ \case
                        (Elt e, i) -> do
                            outCodeLine' [qc|"{eName e}" -> do|]
                            withIndent $ do
                                (a,o) <- generateElementsOfComplexParser ("arrOfs'", "strStart") [Elt e]
                                outCodeLine' [qc|UMV.unsafeWrite vec arrStart {i::Int}|]
                                outCodeLine' [qc|return ({a}, {o})|]
                        (x, _) -> warn [qc|Type {c}: nested types not supported yet // {x}|]
            r@(Ref {}) -> do
                let parserName = getParserName r ""
                outCodeLine' [qc|parse{parserName} arrStart strStart -- !! <{typeName}> / <{ty}>|]
            Restriction _ _ ->
                outCodeLine' [qc|parseString arrStart strStart|]
            Extension base (Complex {inner=Seq exFields}) -> do
                let baseParserName = fromMaybe [qc|{base}Content|] $ getParserForStandardXsd base
                outCodeLine' [qc|(arrOfs', strOfs') <- parse{baseParserName} arrStart strStart|]
                generateElementsOfComplexParser ("arrOfs'", "strOfs'") exFields >>= ofsToReturn
            _ -> error [qc|Unsupported type: {ty}|]
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
        outCodeLine' [qc|getTagName :: Int -> XMLString|]
        outCodeLine' [qc|getTagName strOfs = BSX.takeWhile (\c -> not (isSpaceChar c || c == closeTagChar || c == slashChar)) $ BS.drop (skipToOpenTag strOfs + 1) bs|]
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
        outCodeLine' [qc|skipToOpenTag ofs|] -- TODO with `takeWhile`
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
        let additionalTypes = [] -- extractAdditionalTypes tops -- TODO filter out repeated types
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
    getExtractorNameWithQuant :: XMLString -> Element -> CG XMLString -- ? Builder
    getExtractorNameWithQuant ofs el = do
        extractorName <- getExtractorName (eType el) (eName el)
        let (fieldQuantifier::(Maybe XMLString)) = case eltToRepeatedness el of
                RepMaybe -> Just "extractMaybe"
                RepOnce  -> Nothing
                _        -> Just "extractMany" -- TODO add extractExact support
        return $ case fieldQuantifier of
                 Nothing   -> [qc|extract{extractorName}Content {ofs}|]
                 Just qntf -> [qc|{qntf} {ofs} extract{extractorName}Content|]
    generateContentParser typeName haskellTypeName ty =
        case ty of
            Complex _ attrs (Seq elts) ->
                withIndent $ do
                    forM_ attrs $ \attr -> do
                        let aname = aName attr
                        haskellAttrName <- translate (AttributeName, TargetFieldName) aname aname -- TODO container?
                        outCodeLine' [qc|let {haskellAttrName} = Nothing in|]
                    forM_ (zip elts [1..]) $ \case
                        (Elt el, ofsIdx::Int) -> do
                            let ofs = if ofsIdx == 1 then ("ofs"::XMLString) else [qc|ofs{ofsIdx - 1}|]
                                fieldName' = eName el
                            extractor <- getExtractorNameWithQuant ofs el
                            fieldName <- translate (ElementName, TargetFieldName) fieldName' fieldName' -- TODO container?
                            outCodeLine' [qc|let ({fieldName}, ofs{ofsIdx}) = {extractor} in|]
                        (Group gName, ofsIdx) -> do
                            outCodeLine' [qc|-- Group: {gName}|]
                            extractor <- translate (SchemaGroup, TargetConsName) gName gName
                            fieldName <- translate (SchemaGroup, TargetFieldName) gName gName -- TODO container?
                            outCodeLine' [qc|let ({fieldName}, ofs{ofsIdx}) = extract{extractor}Content ofs{ofsIdx - 1} in|]
                        _ -> error [qc|Unsupported type: {take 100 $ show ty}|]
                    let ofs' = if null elts then "ofs" else [qc|ofs{length elts}|]::XMLString
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
            c@(Complex _ _attrs (Choice choices)) -> do
                outCodeLine' [qc|let ofs' = ofs + 1 in|]
                outCodeLine' [qc|case (arr `UV.unsafeIndex` ofs) of|]
                withIndent $ do
                    -- TODO what to do if there are two options with minOccurs="0" ?
                    forM_ (zip choices [0..]) $ \case
                        (Elt el, i) -> do
                            extractor <- getExtractorNameWithQuant "ofs'" el
                            tn <- translate (ChoiceIn typeName, TargetConsName) typeName (eName el) -- TODO change 'typeName' to 'haskellTypeName' ?
                            outCodeLine' [qc|{i::Int} -> first {tn} $ {extractor}|]
                        (x, _) -> warn [qc|Type {c}: nested types not supported yet // {x}|]
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
        outCodeLine' [qc|extractDayContent = first (read . BSC.unpack) . extractStringContent|]
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
    outCodeLine' [qc|    fmt = iso8601DateFormat (Just "%H:%M:%S%Q%Z")|]
    outCodeLine' "{-# INLINE zonedTimeStr #-}"


generateParserTop :: Schema -> CG ()
generateParserTop _schema = do
    outCodeLine "parser :: ByteString -> Either String TopLevel" -- TODO
    outCodeLine "parser = fmap extractTopLevel . parseTopLevelToArray"
