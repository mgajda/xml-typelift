{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main(main) where

import           Control.DeepSeq
import           Control.Monad
import           Criterion
import           Criterion.Main
import qualified Data.ByteString.Char8 as BS
import           Data.Monoid
import           Weigh
import           Xeno.SAX(validate)
import           Xeno.DOM(parse)

import           Parser
import           Schema

import           TestSet(testFiles)

main :: IO ()
main  = do
  defaultMain    $ map  speedTest testFiles

speedTest :: FilePath -> Benchmark
speedTest filename = do
  env
    (BS.readFile filename)
    (\input ->
       bgroup
         filename
         [ bench "parseSchema" $ nfAppIO parseSchema input
         , bench "xeno"        $ nf      validate    input
         , bench "xeno dom"    $ nf      parse       input
         ])

