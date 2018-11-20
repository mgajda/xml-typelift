module Main(main, testExpr) where

import           Control.Monad
import qualified Data.ByteString.Char8   as BS
import qualified Data.ByteString.Builder as B
import           System.Environment
import           System.Exit(exitFailure)
import           System.IO(stdout, stderr, hPutStrLn)

import Analyze
import CodeGen
import Parser
import FromXML(printExceptions)

whenJust :: Monad m => Maybe t -> (t -> m ()) -> m ()
whenJust (Just x) act = act x
whenJust  Nothing _   = return ()

-- | For GHCid testing:
testExpr :: IO ()
testExpr = forM_ testFiles $ \filename -> do
    putStrLn     $ "Starting to process " <> filename
    input       <- BS.readFile filename
    maybeSchema <- parseSchema input
    whenJust maybeSchema $ \schema -> do
      putStrLn $ "Successfully parsed " <> filename <> ": " <> show schema
      let (analyzed, schemaErrors) = analyze schema
      null schemaErrors `unless` printExceptions input schemaErrors
      printExceptions input $ check analyzed
      putStrLn "Datatypes:"
      B.hPutBuilder stdout $ codegen schema
  where
    testFiles = ["test/person.xsd"
                ,"test/simple.xsd"
                ,"test/test.xsd"
                --,"../tuxml/tuxml_schema-883.xsd"
                ]

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
