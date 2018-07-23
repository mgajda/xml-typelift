
module Main where

import Control.Monad
import qualified Data.ByteString.Char8 as BS
import System.Environment
import System.Exit(exitFailure)
import System.IO(stderr, hPutStrLn)

import Parser

main :: IO ()
main  = do
  args <- getArgs
  forM_ args $ \filename -> do
    input  <- BS.readFile filename
    schema <- parseSchema input
    print schema
  when (null args) $ do
    hPutStrLn stderr "Please give XML Schema argument at input!"
    exitFailure
