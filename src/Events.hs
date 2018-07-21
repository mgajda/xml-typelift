{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE NamedFieldPuns      #-}
module Events where

import Prelude hiding (id, putStrLn, putStr)

import System.IO(stderr)
import Data.Monoid
import Data.ByteString.Char8 as BS
import Data.Set
import Control.Monad.State.Strict as St
import Control.Monad.IO.Class
import GHC.Generics
import System.Exit(exitFailure)

import Xeno.SAX as Xeno


parseSchema :: BS.ByteString -> IO ()
parseSchema input =
  Xeno.process openTagE attrE endOpenTagE textE closeTagE cDataE
               input

bshow :: Show a => a -> BS.ByteString
bshow = BS.pack . show

berror = error . BS.unpack

splitNS = breakEnd (==':')
stripNS = snd . splitNS

--openTagE, endOpenTagE, closeTagE, textE, cDataE :: PState -> XMLString -> PState
openTagE, endOpenTagE, closeTagE, textE, cDataE :: BS.ByteString -> IO ()
openTagE (stripNS -> tag) | stripNS tag `Prelude.elem` handledTags =
  putStrLn $ "<" <> tag
openTagE _ = return ()

attrE :: BS.ByteString -> BS.ByteString -> IO ()
attrE       k v = putStrLn $ k <> "=" <> "\"" <> v <> "\""

endOpenTagE  t   = putStrLn ">"
closeTagE   t | stripNS t `Prelude.elem` handledTags = putStrLn $ "</" <> t <> ">"
closeTagE _ = return ()

-- | This we can freely ignore:
textE       _   = return ()
cDataE      _   = return ()

handledTags = ["schema"
              ,"element"
              ,"simpleType"
              ,"complexType"
              ,"attribute"
              ,"simpleContent"
              ,"complexContent"
              ,"sequence"
              ,"choice"        ]
