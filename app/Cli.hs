module Main(main) where
-- module Cli(main, testExpr) where

import           Control.Monad
import qualified Data.ByteString.Char8   as BS
import           System.Environment
import           System.Exit(exitFailure)
import           System.IO(stdout, stderr, hPutStrLn, hFlush)
import           Xeno.Errors(printExceptions)

import Analyze
import CodeGen
import Parser

-- import TestSet

whenJust :: Monad m => Maybe t -> (t -> m ()) -> m ()
whenJust (Just x) act = act x
whenJust  Nothing _   = return ()

---- | For GHCid testing:
--testExpr :: IO ()
--testExpr = forM_ testFiles processFile
--  [>where
--    testFiles = ["test/person.xsd"
--                ,"test/simple.xsd"
--                ,"test/test.xsd"
--                ,"../tuxml/tuxml_schema-883.xsd"
--                ]-}

processFile :: FilePath -> IO ()
processFile filename = do
    --putStrLn     $ "Starting to process " <> filename
    input       <- BS.readFile filename
    maybeSchema <- parseSchema input
    whenJust maybeSchema $ \schema -> do
      --putStrLn $ "Successfully parsed " <> filename <> ": " <> show schema
      let (analyzed, schemaErrors) = analyze schema
      null schemaErrors `unless` printExceptions input schemaErrors
      --putStrLn "Analysis:"
      printExceptions input $ check analyzed
      --putStrLn "\n===== Datatypes:"
      hFlush stdout
      generatedCode <- codegen analyzed
      putStrLn generatedCode
      hFlush stdout

main :: IO ()
main  = do
  args <- getArgs
  forM_ args processFile
  when (null args) $ do
    hPutStrLn stderr "Please give XML Schema argument at input!"
    exitFailure
