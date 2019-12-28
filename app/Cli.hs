module Main(main) where
-- module Cli(main, testExpr) where

import           Control.Monad
import qualified Data.ByteString.Char8   as BS
import           System.Environment
import           System.Exit(exitFailure)
import           System.IO(stdout, stderr, hPutStrLn, hFlush)
import           Xeno.Errors(printExceptions)
import           Text.Pretty.Simple

import Analyze
import CodeGen
import Parser
import Schema

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
      pPrint analyzed
      null schemaErrors `unless` printExceptions input schemaErrors
      --putStrLn "Analysis:"
      printExceptions input $ check analyzed
      --putStrLn "\n===== Datatypes:"
      hFlush stdout
      generatedCode <- codegen analyzed
      putStrLn generatedCode
      -- **************
      putStrLn "*** PARSER: ***"
      generatedParser <- parserCodegen1 analyzed
      -- putStrLn generatedParser
      writeFile "Result.hs" generatedCode
      appendFile "Result.hs" "\n\n\n\n-- *** PARSER *** --\n\n\n\n"
      appendFile "Result.hs" generatedParser
      appendFile "Result.hs" "\n\nmain :: IO ()\n"
      appendFile "Result.hs" "main = do\n"
      appendFile "Result.hs" "  (parseRootToArray <$> BS.readFile \"test/customersOrders.xml\") >>= \\case\n"
      appendFile "Result.hs" "    Left str -> Prelude.putStrLn (\"ERROR: \" ++ str)\n"
      appendFile "Result.hs" "    Right tl@(TopLevelInternal _ vec) -> do\n"
      appendFile "Result.hs" "        pPrint (UV.take 10 vec)\n"
      appendFile "Result.hs" "        let root = extractRoot tl\n"
      appendFile "Result.hs" "        Prelude.putStrLn \"~~~~~~~\"\n"
      appendFile "Result.hs" "        pPrint root\n"


main :: IO ()
main  = do
  args <- getArgs
  forM_ args processFile
  when (null args) $ do
    hPutStrLn stderr "Please give XML Schema argument at input!"
    exitFailure
