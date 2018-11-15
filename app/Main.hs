module Main(main, testExpr) where

import           Control.Monad
import qualified Data.ByteString.Char8 as BS
import           System.Environment
import           System.Exit(exitFailure)
import           System.IO(stderr, hPutStrLn)

import Analyze
import Parser

whenJust (Just x) act = act x
whenJust  Nothing _   = return ()

-- | For GHCid testing:
testExpr :: IO ()
testExpr = forM_ testFiles $ \filename -> do
    putStrLn $ "Starting to process " <> filename
    input <- BS.readFile filename
    maybeSchema <- parseSchema input
    whenJust maybeSchema $ \schema -> do
      putStrLn $ "Successfully parsed " <> filename
      let (analyzed, schemaErrors) = analyze schema
      null schemaErrors `unless` mapM_ print schemaErrors
      print analyzed
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
