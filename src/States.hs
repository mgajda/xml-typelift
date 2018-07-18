{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module States where

import Prelude hiding (id)

import System.IO(stderr)
import Data.Monoid
import Data.ByteString.Char8 as BS
import Data.Set
import Control.Monad.State.Strict as St
import Control.Monad.IO.Class
import GHC.Generics
import System.Exit(exitFailure)

import Xeno.SAX

import Schema

-- | Keep partly built structure to be merged upwards when the element is closed.
data Builder = BContent Content
             | BElement Element
             | BType    Type
             | BAttr    Attr
             | BSchema  Schema
  deriving (Eq, Ord, Show, Generic)

type PState = [Builder]       -- ^ Builder

initialPState :: PState
initialPState = [BSchema def]

type Parser = St.StateT PState IO ()

report :: BS.ByteString -> Parser
report  = liftIO . BS.putStrLn 

parseSchema :: BS.ByteString -> IO Schema
parseSchema input = do
  result <- (execStateT :: Parser -> PState -> IO PState )
               (process openTagE attrE endOpenTagE textE closeTagE cDataE input)
                initialPState
  case result of
    [BSchema s] -> return s
    unexpected  -> do BS.hPutStrLn stderr
                        $ "Unexpected XML Schema parsing result: " <> bshow unexpected
                      exitFailure

bshow :: Show a => a -> BS.ByteString
bshow = BS.pack . show

openTagE    t   = return ()
attrE       k v = return ()
endOpenTagE t   = return ()-- here we can validate
closeTagE   t   = return ()

-- | This we can freely ignorei:
textE       _   = return ()
cDataE      _   = return ()

--process

