module Main(main, testExpr) where

import           Control.Monad
import qualified Data.ByteString.Char8 as BS
import           Data.Monoid
import           System.Environment
import           System.Exit(exitFailure)
import           System.IO(stderr, hPutStrLn)

import Parser

-- | For GHCid testing:
testExpr :: IO ()
testExpr = forM_ testFiles $ \filename -> do
    putStrLn $ "Starting to process " <> filename
    input <- BS.readFile filename
    schema <- parseSchema input
    putStrLn $ "Successfully parsed " <> filename
    print schema
  where
    testFiles = ["test/person.xsd"
                ,"test/simple.xsd"
                ,"test/test.xsd"
                ,"../tuxml/tuxml_schema-883.xsd"]

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
