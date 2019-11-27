{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main(main) where

import qualified Data.ByteString.Char8 as BS
import           Weigh
import           Xeno.SAX(validate)

import           Parser
import           TestSet

main :: IO ()
main  = do
  Weigh.mainWith $ mapM memTest   testFiles

memTest :: FilePath -> Weigh ()
memTest filename = do
  io ("schema allocations for " <> filename) (\fn -> do
    input  <- BS.readFile fn
    parseSchema input) filename
  io ("xeno allocations for " <> filename) (\fn -> do
     input <- BS.readFile fn
     return $! validate input) filename

