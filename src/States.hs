{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE NamedFieldPuns      #-}
module States where

import Prelude hiding (id)

import System.IO(stderr)
import Data.Char(ord)
import Data.Maybe(fromMaybe)
import Data.Monoid
import Data.ByteString.Char8 as BS
import qualified Data.Map as Map
import Data.Set
import Control.Monad.State.Strict as St
import Control.Monad.IO.Class
import GHC.Generics
import System.Exit(exitFailure)
import Data.ByteString.Internal(ByteString(..))

import Xeno.SAX as Xeno

import Schema

-- | Keep partly built structure to be merged upwards when the element is closed.
data Builder = BContent Content
             | BElement Element
             | BType    { tName :: XMLString, bType :: Type}
             | BAttr    Attr
             | BSchema  Schema
  deriving (Eq, Ord, Show, Generic)

type PState = [Builder]       -- ^ Builder

initialPState :: PState
initialPState = [BSchema def]

type Parser = St.StateT PState IO ()

parseSchema :: BS.ByteString -> IO Schema
parseSchema input = do
  let result =
         Xeno.fold openTagE attrE endOpenTagE textE closeTagE cDataE
                   initialPState input
  case result of
    Right [BSchema s] -> return s
    Left  exc         -> do BS.hPutStrLn stderr
                              $ "Xeno parser exception: " <> bshow exc
                            exitFailure
    Right unexpected  -> do BS.hPutStrLn stderr
                              $ "Unexpected XML Schema parsing result: " <> bshow unexpected
                            exitFailure

bshow :: Show a => a -> BS.ByteString
bshow = BS.pack . show

splitNS = breakEnd (==':')
stripNS = snd . splitNS

openTagE, endOpenTagE, closeTagE, textE, cDataE :: PState -> XMLString -> PState
openTagE s (stripNS -> tag) | stripNS tag `Prelude.notElem` handledTags = s
openTagE s@[BSchema _] (stripNS -> "schema")  = s
openTagE bs t@(stripNS -> "schema") = error $ BS.unpack $ "Nested schema element:" <> bshow t
                                           <> "state:" <> bshow bs
openTagE bs (stripNS -> "element"       ) = BElement def:bs
openTagE bs (stripNS -> "simpleType"    ) = BType    "" (Complex [] def):bs
openTagE bs (stripNS -> "complexType"   ) = BType    "" (Complex [] def):bs
openTagE bs (stripNS -> "attribute"     ) = BAttr    def:bs
openTagE bs (stripNS -> "simpleContent" ) = BType    "" (Complex [] def):bs
openTagE bs (stripNS -> "complexContent") = BType    "" (Complex [] def)        :bs
openTagE bs (stripNS -> "sequence"      ) = BContent (Seq     []    ):bs
openTagE bs (stripNS -> "choice"        ) = BContent (Choice  []    ):bs
openTagE bs  _                            = bs

attrE :: PState -> XMLString -> XMLString -> PState
attrE (b:bs) (stripNS -> "name") v = setName b v:bs
attrE    bs  _                   v = bs

setName :: Builder -> XMLString -> Builder
setName   (BElement   e) n = BElement $ e { name =n }
setName   (BAttr      a) n = BAttr    $ a { aName =n }
setName c@(BType     {}) n =            c { tName=n }
setName   (BContent   _) n = error $ "Attribute _name_ not allowed in content: " <> BS.unpack n

parseError (PS ptrInput iStart iEnd)
           (PS ptrErr start end) msg | ptrInput == ptrErr
                                    && iStart <= start
                                    && iEnd   >= end =
    error $ msg <> " in line " <> show lineCount
         <>        " at "      <> show start          <> ":\n"
         <> BS.unpack (PS ptrInput wrapStart wrapEnd) <> "\n"
         <> [' ' | _ <- [0..start-wrapStart]]         <> "^"
  where
    lineCount     = BS.count '\n' (PS ptrInput iStart start)
    wrapStart     = max iStart   lastLineStart
    wrapEnd       = min iEnd   $ end+40
    lastLineStart = (start-10) `fromMaybe` BS.elemIndexEnd '\n' tilError
    tilError      = PS ptrInput (max iStart $ start-40) start


endOpenTagE  s   t   = s-- here we can validate
closeTagE   s t | stripNS t `Prelude.notElem` handledTags = s
closeTagE   [BElement e, BSchema s]   (stripNS -> "element") =
  [BSchema $ s { tops = e:tops s }]
closeTagE   (BElement e:BContent c:s) (stripNS -> "element") =
  (BContent $ contentAppend c e):s
closeTagE   [BElement e, BSchema s]   (stripNS -> "element") =
  [BSchema $ s { tops = e:tops s }]
closeTagE   [BType tName ty,BSchema s@Schema {types}]
                                      (stripNS -> "complexType") =
  [BSchema $ s { types = Map.insert tName ty types}]
closeTagE   (BAttr a:BType {tName, bType=Complex {subs, attrs}}:bs) (stripNS -> "attribute") =
   BType {tName, bType=Complex { subs, attrs = a:attrs }}:bs
closeTagE   (BAttr a:bTy@BType {}:bs) (stripNS -> "attribute") =
   error $ "Attribute within non-complex type:" <> show bTy
{-closeTagE   (BElement e:bs) tag = error $ "Expected </element>, got: " <> BS.unpack tag
                                       <> "inside:" <> show bs-}
closeTagE   (_:b:bs) t | stripNS t `Prelude.elem` handledTags &&
                       stripNS t /= "schema" = b:bs
--closeTagE   [s] t = error $ BS.unpack $ "Closing the last tag:" <> t
closeTagE   s _ = s

-- | This we can freely ignore:
textE       s _   = s
cDataE      s _   = s

handledTags = ["schema"
              ,"element"
              ,"simpleType"
              ,"complexType"
              ,"attribute"
              ,"simpleContent"
              ,"complexContent"
              ,"sequence"
              ,"choice"        ]
