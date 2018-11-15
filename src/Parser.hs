{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
{-# GHC_OPTIONS -fno-warn-orphans #-}
module Parser where

import Prelude hiding (id)

import           Control.Monad
import qualified Data.ByteString.Char8 as BS hiding (elem)
import           Data.ByteString.Char8(ByteString(..))
import qualified Data.Map as Map
import           System.IO(stderr, hPutStrLn)
import           Text.Read(readMaybe)

import           Xeno.DOM   as Xeno
import           Xeno.Types as Xeno

import           Schema
import           Errors
import           FromXML

-- * These are internal types used only for convenient class definition.
--   Should not be exposed, but rather Schema types should be.
data TypeDesc =
  TypeDesc { tName :: !BS.ByteString
           , ty    :: !Type
           }



instance FromXML TypeDesc where
  fromXML' node = goTypeDesc init node
    where
      init :: TypeDesc
      init = TypeDesc "" $ Complex [] def
      goTypeDesc :: TypeDesc -> Node -> Result TypeDesc
      goTypeDesc = makeFromXML (typeAttr, typeElt)
      typeAttr tyd attr@(aName, aVal) =
        case stripNS aName of
          "name"     -> return $ tyd { tName = aVal }
          "abstract" -> return tyd -- ignore for now
          "final"    -> return tyd -- ignore for now
          "block"    -> return tyd -- ignore for now
          "type"     -> return $ tyd { ty = Ref aVal }
          "ref"      -> return $ tyd { ty = Ref aVal }
          _          -> unknownAttrHandler attr
      typeElt tyd node =
        case nodeName node of
          "annotation"  -> return tyd -- ignore annotations
          "attribute"   -> do
             attr <- fromXML' node
             return $ let ty@TypeDesc { ty = cpl@Complex { attrs, subs } } = tyd
                      in tyd { ty = cpl { attrs = attr:attrs } }
          "complexType"    -> nested
          "complexContent" -> nested
          "simpleContent"  -> nested
          otherwise     -> unknownChildHandler node
        where
          nested = do
             ComplexType ty <- fromXML' node
             return tyd { ty = ty }
  fromXML  node = case nodeName node of
                    "simpleType"  -> fromXML' node
                    "complexType" -> fromXML' node
                    otherName     -> ("Node expected to contain type descriptor is named '"
                                    <> otherName <> "'") `failHere` otherName

newtype ComplexType = ComplexType Type

instance FromXML Attr where
  fromXML' = makeFromXML (attrAttr, attrElt) def
    where
      attrElt  cpl nod = case nodeName nod of
        "annotation" -> return cpl
        "simpleType" -> do
          TypeDesc nam ty <- fromXML' nod
          return $ cpl { aType = ty }
        other        -> unknownChildHandler nod
      attrAttr cpl attr@(aName, aVal) = case stripNS aName of
        "id"      -> return cpl -- ignore ids for now
        "type"    -> return $ cpl { aType = Ref aVal }
        "use"     -> case aVal of
                       "prohibited" -> return cpl -- we can safely ignore, since we do not fully validate
                       "optional"   -> return cpl { use = Optional }
                       "required"   -> return cpl { use = Required }
                       _            -> ("Cannot parse attribute use qualifier: '" <> aVal <> "'")
                                           `failHere` aVal
        "default" -> return $ cpl { use = Default aVal }
        "fixed"   -> return $ cpl { use = Default aVal } -- TODO: treat fixed as default for now
        "form"    -> return   cpl -- ignore qualification check for now
        other     -> unknownAttrHandler attr

instance FromXML ComplexType where
  fromXML' node = goComplex (ComplexType def) node
    where
      goComplex = makeFromXML (cplxAttr, cplxElt)
      cplxAttr cpl (aName, aVal) = case stripNS aName of
          otherwise -> unknownChildHandler node
      cplxElt (ComplexType cpl) node = case nodeName node of
          "attribute" -> do
             attr <- fromXML' node
             return $ ComplexType $ cpl { attrs = attr:attrs cpl }
          "sequence"       -> handleContent Seq
          "choice"         -> handleContent Choice
          "complexContent" -> nested
          "simpleContent"  -> nested
          other            -> unknownChildHandler node
        where
          nested = goComplex (ComplexType cpl) node
          handleContent :: ([Element] -> Schema.Content) -> Result ComplexType
          handleContent cons = do
             contents :: [Element] <- mapM fromXML $ children node
             return $ ComplexType $ cpl { subs = cons contents } -- TODO: handle restricted better

-- | Find line number of the error from ByteString index.
lineNo :: Int -> BS.ByteString -> Int
lineNo index bs = BS.count '\n'
                $ BS.take index bs

parseSchema :: BS.ByteString -> IO (Maybe Schema)
parseSchema input = do
  case Xeno.parse input of
    Left  err -> do
      report $ show err
      return Nothing
    Right dom -> do
      putStrLn "DOM parsed"
      case fromXML dom of
        Left (Xeno.XenoParseError i msg) -> do
          BS.hPutStrLn stderr $
               "Decoding error in line " <> bshow (lineNo i input)
            <> " byte index "            <> bshow         i
            <> " at:\n"
            <> revTake 32 (BS.take i input)
            <> BS.takeWhile ('\n'/=) (BS.take 40 (BS.drop i input))
            <> ":\n"                     <> msg
          return Nothing
        Left  err -> do
          hPutStrLn stderr $ show err
          return Nothing
        Right schema -> do
          putStrLn "XML Schema extracted"
          return $ Just schema
  where
    report :: Show a => a -> IO ()
    report = hPutStrLn stderr . show

schemaAttr :: AttrHandler Schema
schemaAttr sch attr@(aName, aVal) =
  case splitNS aName of
    (_,       "targetNamespace"     ) -> Right sch
    (_,       "elementFormDefault"  ) -> Right sch
    (_,       "attributeFormDefault") -> Right sch
    (_,       "xmlns"               ) -> Right sch
    ("xmlns", _                     ) -> Right sch
    _                                 -> unknownAttrHandler attr

schemaElt :: ChildHandler Schema
schemaElt sch nod =
    case nodeName nod of
      "element" -> do
        elt :: Element <- fromXML' nod
        return $ sch { tops = elt:tops sch }
      "simpleType"  -> handleType
      "complexType" -> handleType
      _ -> return sch -- unknownChildHandler elt val
  where
    handleType = do
      TypeDesc name ty <- fromXML nod
      return $ addType name ty sch
      -- TODO: Find a way to add nested named elements into dictionary.


instance FromXML Element where
  fromXML  = fromXML'
  fromXML' = makeFromXML (eltAttrHandler, eltChildHandler) def

eltAttrHandler elt attr@(aName, aVal) =
  case stripNS aName of
    "name"      -> return $ elt { eName =     aVal }
    "type"      -> return $ elt { eType = Ref aVal }
    "minOccurs" ->
      case BS.readInt aVal of
        Nothing       -> ("Attribute minOccurs should be integer, but is '" <> aVal <> "'")
                             `failHere` aVal
        Just  (r, "") -> return $ elt { minOccurs = r }
    "maxOccurs" -> 
       case readMaxOccurs aVal of
         Left  err -> Left     err
         Right r   -> return $ elt { maxOccurs = r }
    otherwise   -> unknownAttrHandler attr

readMaxOccurs :: BS.ByteString -> Result MaxOccurs
readMaxOccurs  "unbounded"                 = return $ MaxOccurs maxBound
readMaxOccurs (BS.readInt -> Just (v, "")) = return $ MaxOccurs v
readMaxOccurs  other                       = ("Cannot decode '" <> other <> "' as maxoccurs value")
                                                 `failHere` other

eltChildHandler _ = unknownChildHandler

-- attrHandler  :: AttrHandler elt
--  attrHandler   = defaultAttrHandler
--  childHandler :: ChildHandler elt
--  childHandler  = defaultChildHandler

instance FromXML Schema where
  fromXML  n =
    case nodeName n of
      "schema"  -> fromXML' n
      otherName -> ("Top element should be schema, found element:" <> bshow otherName)
                       `failHere` otherName
  fromXML' = makeFromXML (schemaAttr, schemaElt) def

nodeName :: Node -> ByteString
nodeName = stripNS . Xeno.name

readAttr :: Read a => ByteString -> a
readAttr v = case readMaybe $ BS.unpack v of
               Nothing -> parseError v "Cannot read attribute value"
               Just x  -> x

-- | Add type if name is non-empty, to the toplevel schema dictionary.
addType :: XMLString -> Type -> Schema -> Schema
addType ""     _  s                = s
addType tyName ty s@Schema {types} = s { types = Map.insert tyName ty types }

{-
openTagE, endOpenTagE, closeTagE, textE, cDataE :: PState -> XMLString -> PState
openTagE s (stripNS -> tag) | stripNS tag `Prelude.notElem` handledTags = s
openTagE s@[BSchema _] (stripNS -> "schema")  = s
openTagE bs t@(stripNS -> "schema") = parseError t
                                    $ BS.unpack $ "Nested schema element:" <> bshow t
                                   <> "state:" <> bshow bs
openTagE bs (stripNS -> "element"       ) = BElement def:bs
openTagE bs (stripNS -> "simpleType"    ) = BType    "" (Complex [] def):bs
openTagE bs (stripNS -> "complexType"   ) = BType    "" (Complex [] def):bs
openTagE bs (stripNS -> "attribute"     ) = BAttr    def:bs
openTagE bs (stripNS -> "restriction"   ) = BRestriction "" None:bs
openTagE bs (stripNS -> "enumeration"   ) = BEnum "":bs
--openTagE bs (stripNS -> "complexContent") = BType    "" (Complex [] def)        :bs
openTagE bs (stripNS -> "sequence"      ) = BContent (Seq     []    ):bs
openTagE bs (stripNS -> "choice"        ) = BContent (Choice  []    ):bs
openTagE bs  _                            = bs

attrE :: PState -> XMLString -> XMLString -> PState
attrE (b                       :bs) (stripNS -> "name"     ) v = setName b v:bs
attrE (t@BType {}              :bs) (stripNS -> "type"     ) v = t { bType=Ref v }:bs
attrE (BElement e@(Element {}) :bs) (stripNS -> "type"     ) v = BElement (e { eType=Ref v }):bs
attrE (BElement e@(Element {}) :bs) (stripNS -> "minOccurs") v = BElement (e {minOccurs=readAttr v}):bs
attrE (BElement e@(Element {}) :bs) (stripNS -> "maxOccurs") v = BElement (e {maxOccurs=readAttr v}):bs
attrE (b:_)                         (stripNS -> "minOccurs") v = parseError v $ "minOccurs with TOS:" <> show b
attrE (BType n r@Restriction {}:bs) (stripNS -> "base"     ) v = BType n (r {base=v}):bs
attrE (BEnum _                 :bs) (stripNS -> "value"    ) v = BEnum v:bs
attrE  bs    _                                              _v = bs

setName :: Builder -> XMLString -> Builder
setName   (BElement   e) n = BElement $ e { name =n }
setName   (BAttr      a) n = BAttr    $ a { aName=n }
setName c@(BType     {}) n =            c { tName=n }
setName   (BContent   _) n = parseError n   "Attribute _name_ not allowed in content"
setName c                n = parseError n $ "Unexpected top element '" <> show c
                                         <> "' when trying to assign name."

stripNSElem :: [ByteString] -> ByteString -> Bool
stripNSElem elemList e = stripNS e `elem` elemList

endOpenTagE s _t = s-- here we can validate

closeTagE   s t | stripNS t `Prelude.notElem` handledTags = s
closeTagE   [BElement e, BSchema s]            (stripNS -> "element") =
  [BSchema $ s { tops = e:tops s }]
closeTagE   (BElement e:BContent c:s)          (stripNS -> "element") =
  (BContent $ contentAppend c e):s
closeTagE   (BEnum e:(BRestriction{rBase,restr}:bs)) (stripNS -> "enumeration") =
    BRestriction rBase (restriction restr):bs
  where
    restriction (Enum es)   = Enum (e:es)
    restriction  None       = Enum [e]
    restriction (Pattern _) = error "Regex pattern is not implemented yet!"
closeTagE   [BType tName ty,BSchema s@Schema {}]
                                      (stripNS -> "complexType") =
  addType tName ty [BSchema s]
closeTagE (BContent elts:ty@BType {bType}:bs) tag
    | stripNS tag `elem` ["sequence", "all", "choice"] =
    case bType of
      Complex ats _ty -> ty {bType=Complex ats elts}:bs
      _other          -> parseError tag $ "Expected different type to have content assigned: "
                                       <> show bType
closeTagE (BType innerName innerType:ty@BType {bType=outerType}:bs) tag
    | stripNS tag `elem` ["simpleContent", "complexContent"] =
  if outerType /= def
    then parseError tag $ "Double type assignment:" <> show innerType
                       <> " to: " <> show outerType
    else ty { bType=innerType}:addType innerName innerType bs
closeTagE   (BAttr a:BType {tName, bType=Complex {subs, attrs}}:bs) (stripNS -> "attribute") =
   BType {tName, bType=Complex { subs, attrs = a:attrs }}:bs
closeTagE   (BAttr _:bTy@BType {}:_bs) i@(stripNS -> "attribute") =
   parseError i $ "Attribute within non-complex type:" <> show bTy
{-closeTagE   (BElement e:bs) tag = error $ "Expected </element>, got: " <> BS.unpack tag
                                       <> "inside:" <> show bs-}
closeTagE   (_:b:bs) t | stripNS t `Prelude.elem` handledTags &&
                       stripNS t /= "schema" = b:bs
--closeTagE   [s] t = error $ BS.unpack $ "Closing the last tag:" <> t
closeTagE   s _ = s

-- | This we can freely ignore:
textE       s _   = s
cDataE      s _   = s
 -}

handledTags :: [ByteString]
handledTags  = ["schema"
               ,"element"
               ,"simpleType"
               ,"complexType"
               ,"attribute"
               ,"enumeration"
               ,"restriction"
               ,"sequence"
               ,"choice"        ]
