{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE TemplateHaskell          #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
-- | Here we aim to analyze the schema.
module CodeGen(codegen, parserCodegen) where


import           Prelude hiding(lookup, id)

import           Control.Arrow
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

import Data.Proxy

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




class SchemaProcessor a where
    type ElementTraversableResult a :: *
    type TypeTraversableResult a :: *
    type TyPartTraversableResult a :: *

    start :: XMLString {-namespace-} -> a -> CG ()

    processRootTypes :: XMLString -> Type -> (CG (TypeTraversableResult a)) -> a -> CG ()

    processElement    :: a -> Int -> MaxOccurs -> ElementName -> NamespaceName -> CG (TypeTraversableResult a)
                      ->  CG (ElementTraversableResult a)

    processTypeRef     :: a -> XMLString -> CG (TypeTraversableResult a)
    processTypeComplex :: a -> Bool -> [Attr] -> CG (TyPartTraversableResult a) -> CG (TypeTraversableResult a)

    processTyPartSeq    :: a -> CG ([TyPartTraversableResult a])  -> CG (TyPartTraversableResult a)
    processTyPartChoice :: a -> CG ([TyPartTraversableResult a])  -> CG (TyPartTraversableResult a)
    processTyPartAll    :: a -> CG ([TyPartTraversableResult a])  -> CG (TyPartTraversableResult a)
    processTyPartElt    :: a -> CG (ElementTraversableResult a) -> CG (TyPartTraversableResult a)


    end :: a -> CG ()


-- ~~~

type ParserName = XMLString

data ParserST

data ParserSTTypeTraversableResultX = ParserSTTypeTraversableResultX XMLString deriving Show

data ParserSTTypeTraversableResult = PSTString
                                   | PSTDecimal
                                   | PSTInteger
                                   | PSTDateTime
                                   | PSTOther XMLString
                                   | PSTComplex [ParserName] -- names of underlaying parsers (just for draft)
                                   deriving Show

data ParserSTElementTraversableResult = PSE XMLString

data ParserSTTyTypeTraversableResult = PTESeq [ParserName] -- names of underlaying parsers
                                     | PTEElt XMLString
                                     | PTEOther XMLString
                                     deriving Show

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

instance SchemaProcessor ParserST where
    type ElementTraversableResult ParserST = ParserSTElementTraversableResult

    type TyPartTraversableResult ParserST = ParserSTTyTypeTraversableResult

    type TypeTraversableResult ParserST = ParserSTTypeTraversableResult

    start _namespace _ = do
        outCodeLine "-- START PARSER GEN --"

    processRootTypes schName _schType typeTraversor _ = do
        outCodeLine' [qc|--|]
        outCodeLine' [qc|--|]
        outCodeLine' [qc|-- processRootTypes {schName}|]
        outCodeLine' [qc|parse{schName} arrayBegin strBegin' = do|]
        outCodeLine' [qc|  let strBegin = skipSpaces bs strBegin'|]
        (r, parsersCode) <- cut $ withIndent typeTraversor
        withIndent $
            case r of
                PSTComplex [] -> error "Empty array"
                PSTComplex (firstParserName : parserNames) -> do
                    outCodeLine' [qc|strContentEnd'|]
                    outCodeLine' [qc|    <-  {firstParserName} arrayBegin strBegin|]
                    forM_ (zip parserNames [2::Int,4..]) $ \(parserName, arrayShift) ->
                        outCodeLine' [qc|    >>= {parserName} (arrayBegin + {arrayShift})|]
                    outCodeLine' [qc|return $ skpiSpaces bs strContentEnd'|]
                x -> error [qc|Don't know how to generate {x}|]
        outCodeLine' [qc|  where|]
        RWS.tell parsersCode
        -- outCodeLine [qc|processRootTypes : traversor result: {r}|]
    processTypeRef _st "xs:string"   = return PSTString
    processTypeRef _st "xs:integer"  = return PSTInteger
    processTypeRef _st "xs:dateTime" = return PSTDateTime
    processTypeRef _st "xs:decimal"  = return PSTDecimal
    processTypeRef _st ref = do
        outCodeLine' [qc|ref: {ref}|]
        return $ PSTOther ref
    processTypeComplex _st _mixed _attrs tyPartProcessor =
        (\(PTESeq names) -> PSTComplex names) <$> withIndent tyPartProcessor

    processElement _st minOccurs maxOccurs eName _targetNamespace typeProcessor = do
        let parserElementName = [qc|parse{eName}|]
        -- outCodeLine' [qc|-- processElement "{eName}, minOccurs = {minOccurs}, maxOccurs = {maxOccurs}" \{|]
        outCodeLine' [qc|{parserElementName} arrayBegin tagBegin' = do|]
        outCodeLine' [qc|    let tagBegin = skipSpaces bs tagBegin'|]
        outCodeLine' [qc|    ensure tagBegin "<{eName}>"|]
        outCodeLine' [qc|    let contentBegin = tagBegin + {BS.length eName}|]
        withIndent typeProcessor >>= \case
            PSTOther {} ->
                outCodeLine' [qc|    -- TODO don't know how to read complex type|]
            _           -> do
                outCodeLine' [qc|    let contentEnd = skipToOpenTag bs contentBegin|]
        outCodeLine' [qc|    ensure contentEnd "</{eName}>"|]
        outCodeLine' [qc|    UMV.unsafeWrite vec arrayBegin       contentBegin|]
        outCodeLine' [qc|    UMV.unsafeWrite vec (arrayBegin + 1) (contentEnd - contentBegin)|]
        outCodeLine' [qc|    return $ contentEnd + {BS.length eName + 1}|]
        return $ PSE parserElementName

    processTyPartSeq _st listTrav = do
        withIndent listTrav >>=
            return . PTESeq . map (\case
                    PTEElt elt -> elt
                    x -> error [qc|Don't know how to process {x}|])
    processTyPartChoice _st _listTrav = outCodeLine' [qc|processTyPartChoice|] >> return (PTEOther "<<<processTyPartChoice>>>")
    processTyPartAll _st _listTrav    = outCodeLine' [qc|processTyPartAll|]    >> return (PTEOther "<<<processTyPartChoice>>>")
    processTyPartElt _st elTrav = (\(PSE parserName) -> PTEElt parserName) <$> elTrav
    end _ = do
        outCodeLine "-- FINISH PARSER GEN --"



traverseSchema :: forall st . SchemaProcessor st => Schema -> st -> CG ()
traverseSchema Schema{..} st = do
    start namespace st
    mapM_ traverseSchemaType $ Map.toList types
    traverseTops tops
    end st
  where
    traverseSchemaType :: (XMLString, Type) -> CG ()
    traverseSchemaType (name, ty) =
        processRootTypes name ty (traverseType ty) st

    traverseElement :: Element -> CG (ElementTraversableResult st)
    traverseElement Element{..} = processElement st minOccurs maxOccurs eName targetNamespace (traverseType eType)

    traverseType :: Type -> CG (TypeTraversableResult st)
    traverseType (Ref str)          = processTypeRef st str
    traverseType (c@(Complex {..})) = processTypeComplex st mixed attrs (traverseTyPart inner)
    traverseType _ = error "traverseType : unknown"

    traverseTyPart :: TyPart -> CG (TyPartTraversableResult st)
    traverseTyPart (Seq parts)    = processTyPartSeq    st (traverseTyPartList parts)
    traverseTyPart (Choice parts) = processTyPartChoice st (traverseTyPartList parts)
    traverseTyPart (All parts)    = processTyPartAll    st (traverseTyPartList parts)
    traverseTyPart (Elt element)  = processTyPartElt    st (traverseElement element)

    traverseTyPartList :: [TyPart] -> CG [TyPartTraversableResult st]
    traverseTyPartList typarts = mapM traverseTyPart typarts

    traverseTops :: [Element] -> CG ()
    traverseTops _ = return ()


generateParser :: Schema -> CG ()
generateParser sch = traverseSchema sch (undefined :: ParserST)

