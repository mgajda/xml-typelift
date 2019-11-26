-- | Utilities for processing XML files
--
module Tests.Utils where


import           Control.DeepSeq
import           Control.Exception
import           Control.Monad
import           System.FilePath.Posix
import           System.IO.Temp
-- import           Text.InterpolatedString.Perl6 (qc)
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Char8   as BS
import qualified Data.ByteString.Lazy    as BSL

import Analyze
import Parser
import CodeGen


withGeneratedFile :: FilePath -> (FilePath -> IO ()) -> IO ()
withGeneratedFile xmlFilename action = do
    input <- BS.readFile xmlFilename
    (Just schema) <- parseSchema input
    let (analyzed, schemaErrors) = analyze schema
    unless (null schemaErrors) $ error "Schema has errors"
    result <- codegen analyzed >>= (evaluate . force)
    withSystemTempDirectory "xml-typelift" $ \dirname -> do
        let testfn = dirname </> "Result.hs"
        writeFile testfn result
        action testfn

