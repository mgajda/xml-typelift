module Main(main, testExpr) where

import           Control.Monad
import qualified Data.ByteString.Char8   as BS
import qualified Data.ByteString.Builder as B
import           System.Environment
import           System.Exit(exitFailure)
import           System.IO(stdout, stderr, hPutStrLn, hFlush)
import           Xeno.Errors(printExceptions)

import CodeGen
import Parser

import TestSet

whenJust :: Monad m => Maybe t -> (t -> m ()) -> m ()
whenJust (Just x) act = act x
whenJust  Nothing _   = return ()

-- | For GHCid testing:
testExpr :: IO ()
testExpr = forM_ testFiles $ processFile

processFile :: FilePath -> IO ()
processFile filename = do
    putStrLn     $ "Starting to process " <> filename
    input       <- BS.readFile filename
    maybeSchema <- parseSchema input
    whenJust maybeSchema $ \schema -> do
      B.hPutBuilder stdout $ codegen schema
      hFlush stdout

main :: IO ()
main  = do
  args <- getArgs
  forM_ args processFile
  when (null args) $ do
    hPutStrLn stderr "Please give XML Schema argument at input!"
    exitFailure
