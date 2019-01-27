{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE Strict               #-}
{-# LANGUAGE ViewPatterns         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Parser where

import Prelude hiding (id)

import           Control.Monad(foldM)
import qualified Data.ByteString.Char8 as BS hiding (elem)
import           Data.ByteString.Char8(ByteString)
import           Data.List(find)
import           Data.Maybe(catMaybes, fromMaybe)
import qualified Data.Map as Map
import           System.IO(stderr)
import           Text.Read(readMaybe)

import           Xeno.DOM    as Xeno
import           Xeno.Errors as Xeno

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
  fromXML' = goTypeDesc $ TypeDesc "" $ Complex False [] def
    where
      goTypeDesc :: TypeDesc -> Node -> Result TypeDesc
      goTypeDesc = makeFromXML (typeAttr, typeElt)
      typeAttr :: AttrHandler TypeDesc
      typeAttr tyd attr@(aName, aVal) =
        case stripNS aName of
          "name"     -> return $ tyd { tName =     aVal }
          "abstract" -> return tyd -- ignore for now
          "final"    -> return tyd -- ignore for now
          "block"    -> return tyd -- ignore for now
          "mixed"    -> return $ tyd { ty = markMixed $ ty tyd } -- TODO: ignore for now
          "type"     -> return $ tyd { ty = Ref aVal }
          "ref"      -> return $ tyd { ty = Ref aVal }
          _          -> unknownAttrHandler "type description" attr
      typeElt :: ChildHandler TypeDesc
      typeElt tyd node =
        case nodeName node of
          "annotation"  -> return tyd -- ignore annotations
          "attribute"   -> do
             attr <- fromXML' node
             return $ let TypeDesc { ty = cpl@Complex { attrs } } = tyd
                      in  tyd      { ty = cpl { attrs = attr:attrs } }
          "complexType"    -> nested -- can it be nested?
          "simpleType"     -> nested -- can it be nested?
          "complexContent" -> nested
          "simpleContent"  -> nested
          "restriction"    -> do
             restr <- handleRestriction node
             return tyd { ty = restr }
          "extension"      -> do
             TypeDesc _ newTy <- foldM typeElt tyd $ Xeno.children     node
             return tyd { ty = Extension { base  = getBaseAttribute node
                                         , mixin = newTy } }
          "all"            -> handleTyPart consAll
          "sequence"       -> handleTyPart Seq
          "choice"         -> handleTyPart Choice
          _                -> unknownChildHandler "type description" node
        where
          -- Our parsing model does not take account of All vs Seq difference
          TypeDesc tName ty = tyd
          handleTyPart :: ([TyPart] -> TyPart) -> Result TypeDesc
          handleTyPart cons = do
            inner <- parseTyPart cons node
            return $ TypeDesc tName $ ty { inner } -- TODO: handle restricted better
          nested = goTypeDesc tyd node
  fromXML  node = case nodeName node of
                    "simpleType"  -> fromXML' node
                    "complexType" -> fromXML' node
                    otherName     -> ("Node expected to contain type descriptor is named '"
                                    <> otherName <> "'") `failHere` otherName

markMixed cpl@(Complex {..}) = cpl { mixed=True }
markMixed x = error $ "Cannot mark type as mixed: " <> show x

instance FromXML TyPart where
  fromXML' = fromXML
  fromXML node = case nodeName node of
      "choice"  ->  parseTyPart Choice  node
      "all"     ->  parseTyPart consAll node
      "seq"     ->  parseTyPart Seq     node
      "element" ->  Elt <$> fromXML     node
      other     -> ("Unknown type particle '" <> bshow other <> "'") `failHere` other

-- | Parse type particle, and fix missing attribute values in case of xs:all
parseTyPart :: ([TyPart] -> TyPart) -> Xeno.Node -> Result TyPart
parseTyPart cons node = cons <$> mapM fromXML (Xeno.children node)

-- | Fix missing minOccurs/maxOccurs in xs:all
consAll :: [TyPart] -> TyPart
consAll typas = Seq (fixOccurs <$> typas)
  where
    fixOccurs (Elt elt@(Schema.Element {..})) = Elt $ elt { minOccurs=1, maxOccurs=MaxOccurs 1 }
    fixOccurs other                           = other
postprocess  other      = other

handleRestriction :: Xeno.Node -> Result Type
handleRestriction node = do
    restriction <- case ((`elem` ["pattern", "enumeration", "list", "union"]) . nodeName)
                          `find` Xeno.children node of
        Nothing -> return None
        Just n -> let err = (`failHere` nodeName n)
                  in case nodeName n of
                      "length"      ->  err "Length restriction not yet implemented"
                      "list"        ->  err "List restriction not yet implemented"
                      "union"       ->  err "List restriction not yet implemented"
                      "pattern"     ->  Pattern <$> getValueAttr n
                      "enumeration" -> (Enum . catMaybes) <$> mapM getEnumValue (Xeno.children node)
                      _other        ->  err "Unexpected failure in parsing <restriction>"
    return Restricted { base = getBaseAttribute node, restriction }
  where
    getEnumValue :: Xeno.Node -> Result (Maybe XMLString)
    getEnumValue eNode@(nodeName -> "enumeration") = Just <$> getValueAttr eNode
    getEnumValue _                                 = return Nothing
    getValueAttr :: Xeno.Node -> Result XMLString
    getValueAttr (findAttributeValue "value" -> Just v) = return v
    getValueAttr  aNode                                 = "Missing value attribute" `failHere` nodeName aNode

findAttributeValue :: XMLString -> Xeno.Node -> Maybe XMLString
findAttributeValue attrName node = case find ((attrName==) . fst) $ Xeno.attributes node of
  Just (_, v) -> Just v
  Nothing     -> Nothing

getBaseAttribute :: Xeno.Node -> XMLString
getBaseAttribute  = fromMaybe "" . findAttributeValue "base"

--newtype ComplexType = ComplexType Type

instance FromXML Attr where
  fromXML' = makeFromXML (attrAttr, attrElt) def
    where
      attrElt  cpl nod = case nodeName nod of
        "annotation" -> return cpl
        "simpleType" -> do
          TypeDesc "" ty <- fromXML' nod
          return $ cpl { aType = ty }
        _other       -> unknownChildHandler "attribute" nod
      attrAttr cpl attr@(aName, aVal) = case stripNS aName of
        "id"      -> return cpl -- ignore ids for now
        "type"    -> return $ cpl { aType = Ref aVal }
        "name"    -> return $ cpl { aName = aName    }
        "use"     -> case aVal of
                       "prohibited" -> return cpl -- we can safely ignore, since we do not fully validate
                       "optional"   -> return cpl { use = Optional }
                       "required"   -> return cpl { use = Required }
                       _            -> ("Cannot parse attribute use qualifier: '" <> aVal <> "'")
                                           `failHere` aVal
        "default" -> return $ cpl { use = Default aVal }
        "fixed"   -> return $ cpl { use = Default aVal } -- TODO: treat fixed as default for now
        "form"    -> return   cpl -- ignore qualification check for now
        _other    -> unknownAttrHandler "attribute" attr

parseSchema :: BS.ByteString -> IO (Maybe Schema)
parseSchema input = do
  --print $ skipDoctype input
  case Xeno.parse $ skipDoctype input of
    Left  err -> do
      BS.hPutStrLn stderr $ displayException input err
      return Nothing
    Right dom -> do
      case fromXML dom of
        Left  msg    -> do
          BS.hPutStrLn stderr $ displayException input msg
          return   Nothing
        Right schema ->
          return $ Just schema

schemaAttr :: AttrHandler Schema
schemaAttr sch attr@(aName, aVal) =
  case splitNS aName of
    (_,       "targetNamespace"     ) -> return $ sch { namespace = aVal }
    (_,       "elementFormDefault"  ) -> return sch
    (_,       "attributeFormDefault") -> return sch
    (_,       "xmlns"               ) -> return sch
    ("xml",   "lang"                ) -> return sch -- ignore spoken language declaration
    (_,       "version"             ) -> return sch -- ignore spoken language declaration
    ("xmlns", _                     ) -> return sch
    _                                 -> unknownAttrHandler "schema" attr

schemaElt :: ChildHandler Schema
schemaElt sch nod =
    case nodeName nod of
      "element" -> do
        elt :: Element <- fromXML' nod
        return $ sch { tops = elt:tops sch }
      "simpleType"  -> handleType
      "complexType" -> handleType
      "key"         -> return sch
      "unique"      -> return sch
      "keyref"      -> return sch
      _ -> return sch -- unknownChildHandler elt val
  where
    handleType = do
      TypeDesc label ty <- fromXML nod
      return $ addType label ty sch
      -- TODO: Find a way to add nested named elements into dictionary.


instance FromXML Element where
  fromXML  n | nodeName n == "element" = fromXML' n
  fromXML (nodeName -> nam)            = ("Expected xs:element, got '" <> nam <> "'") `failHere` nam
  fromXML' = makeFromXML (eltAttrHandler, eltChildHandler) def

eltAttrHandler :: AttrHandler Element
eltAttrHandler elt attr@(aName, aVal) =
  case stripNS aName of
    "name"      -> return $ elt { eName =     aVal }
    "type"      -> return $ elt { eType = Ref aVal }
    "ref"       -> "Element references are not implemented yet" `failHere` aName
    "minOccurs" ->
      case BS.readInt aVal of
        Just  (r, "") -> return $ elt { minOccurs = r }
        _             -> ("Attribute minOccurs should be integer, but is '" <> aVal <> "'")
                             `failHere` aVal
    "maxOccurs" -> 
       case readMaxOccurs aVal of
         Left  err -> Left     err
         Right r   -> return $ elt { maxOccurs = r }
    _           -> unknownAttrHandler "element" attr

readMaxOccurs :: BS.ByteString -> Result MaxOccurs
readMaxOccurs  "unbounded"                 = return $ MaxOccurs maxBound
readMaxOccurs (BS.readInt -> Just (v, "")) = return $ MaxOccurs v
readMaxOccurs  other                       = ("Cannot decode '" <> other <> "' as maxoccurs value")
                                                 `failHere` other

eltChildHandler :: ChildHandler Element
eltChildHandler elt node = case nodeName node of
    "complexType" -> handleType
    "simpleType"  -> handleType
    "annotation"  -> return     elt -- ignore
    "key"         -> return     elt
    "unique"      -> return     elt
    "keyref"      -> return     elt
    _             -> unknownChildHandler "element" node
  where
    handleType = do
      TypeDesc _ ty <- fromXML node
      return $ elt { eType = ty }

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

