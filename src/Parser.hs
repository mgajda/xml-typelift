{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
module Parser where

import Prelude hiding (id)

import           Control.Monad
import qualified Data.ByteString.Char8 as BS hiding (elem)
import           Data.ByteString.Char8(ByteString)
import qualified Data.Map as Map
import           System.IO(stderr, hPutStrLn)
import           Text.Read(readMaybe)

import           Xeno.DOM as Xeno

import           Schema
import           Errors
import           FromXML

parseSchema :: BS.ByteString -> IO (Maybe Schema)
parseSchema input = do
  case Xeno.parse input of
    Left  err -> do
      report $ show err
      return Nothing
    Right dom -> do
      putStrLn "DOM parsed"
      case fromXML dom of
        Left  err    -> do
          report err
          return Nothing
        Right schema -> do
          putStrLn "XML Schema extracted"
          return $ Just schema
  where
    report :: Show a => a -> IO ()
    report = hPutStrLn stderr . show

schemaAttr :: AttrHandler Schema
schemaAttr sch attr = unknownAttrHandler  sch attr

schemaElt :: ChildHandler Schema
schemaElt  elt val  = unknownChildHandler elt val

-- attrHandler  :: AttrHandler elt
--  attrHandler   = defaultAttrHandler
--  childHandler :: ChildHandler elt
--  childHandler  = defaultChildHandler

instance FromXML Schema where
  fromXML  n =
    case nodeName n of
      "schema"  -> fromXML' n
      otherName -> Left $ DecodingError (getStartIndex otherName)
                                        ("Top element should be schema, found element:" <> bshow otherName)
  fromXML' = makeFromXML (def, schemaAttr, schemaElt)

bshow :: Show a => a -> BS.ByteString
bshow = BS.pack . show

nodeName :: Node -> ByteString
nodeName = stripNS . Xeno.name

splitNS :: ByteString -> (ByteString, ByteString)
splitNS = BS.breakEnd (==':')

stripNS :: ByteString -> ByteString
stripNS = snd . splitNS

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
