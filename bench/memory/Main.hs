{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main(main) where

import           Control.Monad
import           Criterion
import           Criterion.Main
import qualified Data.ByteString.Char8 as BS
import           Data.Monoid
import           Weigh
import           Xeno.SAX(validate)

import           Parser
import           Schema
import           TestSet

main :: IO ()
main  = do
  Weigh.mainWith $ mapM memTest   testFiles

memTest :: FilePath -> Weigh ()
memTest filename = do
  io ("schema allocations for " <> filename) (\filename -> do
    input  <- BS.readFile filename
    parseSchema input) filename
  io ("xeno allocations for " <> filename) (\filename -> do
     input <- BS.readFile filename
     return $! validate input) filename

