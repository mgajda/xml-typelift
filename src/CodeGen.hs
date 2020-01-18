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
    Extension {} -> do
      warn [qc|Did not implement elements with extension types yet {eType}|]
      return "Xeno.Node"
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
        fun  x = error $ "Not yet implemented nested sequence, all or choice:" <> show x
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
  superTyLabel <- translate (SchemaType,TargetFieldName) eName "Super" -- should be: MetaKey instead of SchemaType
  generateContentType eName $ cpl
                  `appendElt` Element {eName=builderString superTyLabel
                                      ,eType=Ref base
                                      ,maxOccurs=MaxOccurs 1
                                      ,minOccurs=1
                                      ,targetNamespace=""}
  -- TODO: Refactor for parser generation!
generateContentType eName (Extension   base  _otherType                  ) = do
  warn "Complex extensions are not implemented yet"
  tyName   <- translate (SchemaType,  TargetTypeName) base eName
  consName <- translate (ElementName, TargetConsName) base eName
  declareNewtype (TyData tyName) (TyCon consName) (TyType "Xeno.Node")
  return tyName

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
    outCodeLine [qc|data TopLevelInternal = TopLevelInternal !ByteString !(UV.Vector Int) deriving (Generic, NFData, Show)|]
    outCodeLine ""

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
                        withIndent $ case ty of
                            Complex _ _attrs (Seq elts) -> do
                                let elements = map (\case (Elt e) -> e ; _ -> error [qc|Unsupported type: {take 100 $ show ty}|]) elts
                                    ofsNames' = (("arrStart", "strStart") : [ ( [qc|arrOfs{i}|], [qc|strOfs{i}|]) | i <- [(1::Int)..]])
                                                :: [(XMLString, XMLString)]
                                    ofsNames = zip ofsNames' (tail ofsNames')
                                    endNum = length elements
                                forM_ (zip ofsNames elements) $ \(((arrOfs, strOfs), (arrOfs', strOfs')), el) -> do
                                    let parserName = getParserName (eType el) (eName el)
                                        (isUseArrOfs, tagQuantifier::XMLString) = case el of
                                                Element 0 (MaxOccurs 1) _ _ _ -> (True,  "inMaybeTag")
                                                Element 1 (MaxOccurs 1) _ _ _ -> (False, "inOneTag")
                                                Element 0 Unbounded     _ _ _ -> (True,  "inManyTags")
                                                Element mn mx _ _ _ -> (False, [qc|inSomeTag {mn} {mx}|]) -- XXX
                                                -- Element m n             _ _ _ -> error [qc|Unsupported element quantities: ({m}, {n})|]
                                        (arrOfs1, arrOfs2)::(XMLString,XMLString) =
                                            if isUseArrOfs then ([qc| {arrOfs}|],"") else ("", [qc| {arrOfs}|])
                                    -- TODO parse with attributes!
                                    outCodeLine' [qc|({arrOfs'}, {strOfs'}) <- {tagQuantifier} "{eName el}"{arrOfs1} {strOfs} $ parse{parserName}{arrOfs2}|]
                                outCodeLine' [qc|return (arrOfs{endNum}, strOfs{endNum})|]
                            c@(Complex _ _attrs (Choice _elts)) -> do
                                -- XXX
                                outCodeLine' [qc|-- Complex: {c}|]
                            r@(Ref {}) -> do
                                let parserName = getParserName r ""
                                outCodeLine' [qc|parse{parserName} arrStart strStart -- !!|]
                            r@(Restriction _ None) -> do
                                -- XXX
                                outCodeLine' [qc|-- Restriction: {r}|]
                            Restriction _ _ -> outCodeLine' [qc|parseString arrStart strStart|]
                            e@(Extension _base _mixin) ->
                                -- XXX
                                outCodeLine' [qc|-- Extension {e}|]
                            _ -> error [qc|Unsupported type: {ty}|]
                    -- Generate auxiliary functions
                    generateAuxiliaryFunctions
  where
    getParserName :: Type -> XMLString -> XMLString
    getParserName (Ref "xs:integer") _  = "Integer"
    getParserName (Ref "xs:positiveInteger") _  = "Integer" -- TODO add checking
    getParserName (Ref "xs:stringContent") _  = "String" -- TODO add checking
    getParserName (Ref "xs:decimal") _  = "Decimal"
    getParserName (Ref "xs:string") _   = "String"
    getParserName (Ref "xs:token") _    = "String"
    getParserName (Ref "xs:dateTime") _ = "DateTime"
    getParserName (Ref r) _             = [qc|{r}Content|]
    getParserName (Complex {}) xname    = [qc|{xname}Content|]
    getParserName t _                   = [qc|???{t}|]
    extractAdditionalTypes :: [Element] -> [(XMLString, Type)]
    extractAdditionalTypes elts =
        let allElts = (universeBi elts :: [Element])
        in map (\(Element _ _ name typ _) -> (name, typ)) allElts
    generateAuxiliaryFunctions = do
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
        outCodeLine' [qc|        Just ofs' -> do|]
        outCodeLine' [qc|            (arrOfs, strOfs) <- inParser ofs'|]
        outCodeLine' [qc|            let ofs'' = skipToOpenTag strOfs|]
        outCodeLine' [qc|            if bs `BSU.unsafeIndex` (ofs'' + 1) == slashChar then do|]
        outCodeLine' [qc|                case ensureTag False tag (ofs'' + 2) of|]
        outCodeLine' [qc|                    Nothing     -> return Nothing|]
        outCodeLine' [qc|                    Just ofs''' -> return $ Just (arrOfs, ofs''')|]
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
        --outCodeLine' [qc|inManyTags tag arrOfs strOfs inParser = inManyTags' True tag arrOfs strOfs inParser|] -- TODO add attributes processing
        --outCodeLine' [qc|inManyTagsWithAttrs tag arrOfs strOfs inParser = inManyTags' True tag arrOfs strOfs inParser|]
        --outCodeLine' [qc|inManyTags' hasAttrs tag arrOfs strOfs inParser = do|]
        --outCodeLine' [qc|    (cnt, endArrOfs, endStrOfs) <- flip fix (0, (arrOfs + 1), strOfs) $ \next (cnt, arrOfs', strOfs') ->|]
        --outCodeLine' [qc|        inOneTag' hasAttrs tag strOfs' (inParser arrOfs') >>= \case|]
        --outCodeLine' [qc|            Just (arrOfs'', strOfs'') -> next   (cnt + 1, arrOfs'', strOfs'')|]
        --outCodeLine' [qc|            Nothing                   -> return (cnt,     arrOfs', strOfs')|]
        --outCodeLine' [qc|    UMV.unsafeWrite vec arrOfs cnt|]
        --outCodeLine' [qc|    return (endArrOfs, endStrOfs)|]
        -- ~~~~~~~~
        outCodeLine' [qc|ensureTag True expectedTag ofs|]
        outCodeLine' [qc|  | expectedTag `BS.isPrefixOf` (BS.drop ofs bs) =|]
        outCodeLine' [qc|      if bs `BSU.unsafeIndex` ofsToEnd == closeTagChar|]
        outCodeLine' [qc|        then Just (ofsToEnd + 1)|]
        outCodeLine' [qc|      else if isSpaceChar (bs `BSU.unsafeIndex` ofsToEnd)|]
        outCodeLine' [qc|        then Just (skipToCloseTag (ofs + BS.length expectedTag) + 1)|]
        outCodeLine' [qc|      else|]
        outCodeLine' [qc|        Nothing|]
        outCodeLine' [qc|  | otherwise = Nothing|]
        outCodeLine' [qc|  where ofsToEnd = ofs + BS.length expectedTag|]
        outCodeLine' [qc|ensureTag False expectedTag ofs|]
        outCodeLine' [qc|  | expectedTag `BS.isPrefixOf` (BS.drop ofs bs) && (bs `BSU.unsafeIndex` ofsToEnd == closeTagChar)|]
        outCodeLine' [qc|        = Just (ofsToEnd + 1)|]
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
        outCodeLine' [qc|parseInteger = parseString|]
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
        let name = eName topEl
        outCodeLine' [qc|extractTopLevel :: TopLevelInternal -> TopLevel|]
        outCodeLine' [qc|extractTopLevel (TopLevelInternal bs arr) = fst $ extract{name}Content 0|]
    withIndent $ do
        outCodeLine' "where"
        let additionalTypes = extractAdditionalTypes tops -- TODO filter out repeated types
        withIndent $ do
            forM_ ((Map.toList types) ++ additionalTypes) $ \(typeName, ty) -> do
                case ty of
                    Complex _ attrs (Seq elts) -> do
                        haskellTypeName <- translate (ElementName, TargetTypeName) "" typeName -- TODO container?
                        outCodeLine' [qc|extract{typeName}Content ofs =|]
                        withIndent $ do
                            let elements = map (\case (Elt e) -> e ; _ -> error [qc|Unsupported type: {take 100 $ show ty}|]) elts
                            -- Output attributes reader
                            forM_ attrs $ \attr -> outCodeLine' [qc|let {aName attr} = Nothing in|]
                            -- Output fields reader
                            mapM_ (\(el, ofsIdx) -> do
                                let extractorName = getExtractorName (eType el) (eName el)
                                    ofs = if ofsIdx == 1 then ("ofs"::XMLString) else [qc|ofs{ofsIdx - 1}|]
                                    (fieldQuantifier::(Maybe XMLString)) = case el of
                                             Element 0 (MaxOccurs 1) _ _ _ -> (Just "extractMaybe")
                                             Element 1 (MaxOccurs 1) _ _ _ -> (Nothing)
                                             Element 0 Unbounded     _ _ _ -> (Just "extractMany")
                                             Element m n             _ _ _ -> (Just [qc|??extractSome?? {m} {n}|]) -- error [qc|Unsupported element quantities: ({m}, {n})|]
                                    (extractor::XMLString) = case fieldQuantifier of
                                             Nothing -> [qc|extract{extractorName}Content {ofs}|]
                                             Just qntf -> [qc|{qntf} {ofs} extract{extractorName}Content|]
                                outCodeLine' [qc|let ({normalizeFieldName $ eName el}, ofs{ofsIdx}) = {extractor} in|]
                                {-
                                    let (en, isPrimitive) = getExtractorNameAndOfs (eType el) (eName el)
                                        ofsSimpleStr = if simpleOfs == 0 then "" else [qc| + {simpleOfs}|]
                                        ofsCalcStr = mconcat (map (\co -> [qc| + {co}|]) calcOfs)
                                        ofsStr::XMLString =
                                            if BS.null ofsSimpleStr && BS.null ofsCalcStr
                                            then "ofs"
                                            else [qc|(ofs{ofsSimpleStr}{ofsCalcStr})|]
                                        (simpleOfs', calcOfs', addGetOffsAfter) =
                                            if isPrimitive
                                            then (simpleOfs + 2, calcOfs, False)
                                            else (simpleOfs, [qc|getOffsAfter{en} ofs|] : calcOfs, True)
                                        (fieldQuantifier::(Maybe XMLString), isSized) = case el of
                                                Element 0 (MaxOccurs 1) _ _ _ -> (Just "extractMaybe", False)
                                                Element 1 (MaxOccurs 1) _ _ _ -> (Nothing, False)
                                                Element 0 Unbounded     _ _ _ -> (Just "extractMany", True)
                                                Element m n             _ _ _ -> (Just [qc|??extractSome?? {m} {n}|], False) -- error [qc|Unsupported element quantities: ({m}, {n})|]
                                    size <- getElementSize el
                                    let sizeStr = if size > 0 && isSized then [qc| {size}|]::XMLString else ""
                                        getOffsetsAfter' =
                                            if addGetOffsAfter
                                            then [qc|getOffsAfter{en} ofs = ofs + 1 + (arr `UV.unsafeIndex` ofs) * {size}|]:getOffsetsAfter
                                            else getOffsetsAfter
                                        -- TODO `extractMany` reads **offset to the end of list**, and
                                        --      then reads all list until end is reached.
                                        --
                                        --      So it is need to save offset to end of list in `inManyTags` in previous
                                        --      parser: it skip one cell in array, then read list, then save offset it that
                                        --      skipped cell.
                                        --
                                        --      So `extractMany` can read this first cell and then use it.
                                        --
                                        --      Also `getOffsAfterXXX` can universally read it, so we do not need special
                                        --      `getOffsAffterXXX`, we just need `getEnd (getEnd (getEnd offs + K) + L) + M` which
                                        --      simple jumps to ends of arrays (and skip primitive offsets K, L, M).
                                        --
                                        --      BTW, so we need sequental list of this skipping and special tests for that.
                                        --
                                        --      But now we can suppose that arrays only at the end of struct and `error` other
                                        --      structures.
                                        --
                                        --      TODO
                                        --      No, problem is that structures can be variable... So we need to precalculate
                                        --      in generation time does it stable or variable structure. And then decide how
                                        --      to store array...
                                        --
                                        --      At first version we can just output pair `(parsed value, readed size)`.
                                        --      Then it is need to benchmark and make more simple version.
                                        --
                                    let extractor'::XMLString = [qc|extract{en}Content|]
                                        extractor::XMLString = maybe [qc|{extractor'} {ofsStr}|]
                                                                     (\fq -> [qc|{fq} {ofsStr}{sizeStr} $ {extractor'}|])
                                                                     fieldQuantifier
                                    outCodeLine' [qc|let {normalizeFieldName $ eName el} = {extractor}|]
                                    return (simpleOfs', calcOfs', getOffsetsAfter')
                                    -}
                                    )
                                (zip elements [(1::Int)..])
                            let lastCnt = length elements
                            outCodeLine' [qc|({haskellTypeName}\{..}, ofs{lastCnt})|]
                        -- when (not $ null ofsAfter) $
                        --    forM_ (tail ofsAfter) $ \ofsAft -> outCodeLine' [qc|{ofsAft}|]
                    r@(Ref {}) -> do
                        let rname = getExtractorName r ""
                        outCodeLine' [qc|extract{typeName}Content ofs = extract{rname}Content ofs -- {r}|] -- TODO remove comment
                    Restriction _ (Enum opts) -> do
                        haskellTypeName <- translate (ElementName, TargetTypeName) "" typeName -- TODO container?
                        outCodeLine' [qc|extract{haskellTypeName}Content ofs =|]
                        withIndent $ do
                            outCodeLine' [qc|first (\case|]
                            withIndent $ do
                                forM_ (uniq opts) $ \opt -> do
                                    tn <- translate (EnumIn opt, TargetConsName) typeName opt
                                    outCodeLine' [qc|"{opt}" -> {tn} ()|] -- TODO remove '()'
                                outCodeLine' [qc|) $ extractStringContent ofs|]
                    r@(Restriction _ _) ->
                        -- XXX
                        outCodeLine' [qc|-- {r}|]
                    e@(Extension _ _) ->
                        -- XXX
                        outCodeLine' [qc|-- Extension: {e}|] -- XXX
                    c@(Complex _ attrs (Choice elts)) -> do
                        -- XXX
                        outCodeLine' [qc|-- Complex / Choice: {c}|] -- XXX
                    _ -> error [qc|Unsupported type: {show ty}|]
            generateAuxiliaryFunctions
  where
    getExtractorName :: Type -> XMLString -> XMLString
    getExtractorName (Ref "xs:integer") _         = "Integer"
    getExtractorName (Ref "xs:positiveInteger") _ = "Integer" -- TODO
    getExtractorName (Ref "xs:decimal") _         = "Decimal"
    getExtractorName (Ref "xs:string") _          = "String"
    getExtractorName (Ref "xs:token") _           = "Token"
    getExtractorName (Ref "xs:dateTime") _        = "DateTime"
    getExtractorName (Ref r) _                    = r
    getExtractorName (Complex {}) xname           = xname
    getExtractorName (Extension {}) xname         = "??Extension??:" <> xname
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
        outCodeLine' [qc|extractDecimalContent :: Int -> (Scientific, Int)|]
        outCodeLine' [qc|extractDecimalContent = first (read . BSC.unpack) . extractStringContent|]
        outCodeLine' [qc|extractIntegerContent :: Int -> (Integer, Int)|]
        outCodeLine' [qc|extractIntegerContent = first (read . BSC.unpack) . extractStringContent|]
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

