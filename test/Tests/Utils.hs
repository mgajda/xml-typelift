-- | Utilities for processing XML files
--
module Tests.Utils where


import           Control.DeepSeq
import           Control.Exception
import           Control.Monad
import           System.FilePath.Posix
import           System.IO.Temp
import qualified Data.ByteString.Char8 as BS

import Analyze
import Parser
import CodeGen


withGeneratedFile :: Bool -> FilePath -> (FilePath -> IO ()) -> IO ()
withGeneratedFile generateOnlyTypes xmlFilename action = do
    input <- BS.readFile xmlFilename
    (Just schema) <- parseSchema input
    let (analyzed, schemaErrors) = analyze schema
    unless (null schemaErrors) $ error "Schema has errors"
    types <- codegen analyzed
    result <- if generateOnlyTypes then
                  return types
              else do
                  parser <- parserCodegen analyzed
                  return (types <> "\n\n\n" <> parser)
    withSystemTempDirectory "xml-typelift" $ \dirname -> do
        let testfn = dirname </> "XMLSchema.hs"
        writeFile testfn result
        action testfn

