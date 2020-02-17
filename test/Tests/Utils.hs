-- | Utilities for processing XML files
--
module Tests.Utils where


import           Control.Monad
import           Data.Default
import           System.FilePath.Posix
import qualified Data.ByteString.Char8 as BS


import Analyze
import CodeGen
import Flatten
import Parser
import TestUtils


withGeneratedFile :: Bool -> FilePath -> (FilePath -> IO ()) -> IO ()
withGeneratedFile generateOnlyTypes xmlFilename action = do
    input <- BS.readFile xmlFilename
    (Just schema) <- parseSchema input
    let (flattened, msgs) = flatten schema
    unless (null msgs) $ error ("Flattened with errors: " ++ show msgs)
    let (analyzed, schemaErrors) = analyze flattened
    unless (null schemaErrors) $ error "Schema has errors"
    result <- (if generateOnlyTypes then codegen else parserCodegen (def { isGenerateMainFunction = True })) analyzed
    withPreservedSystemTempDirectory "xml-typelift" $ \dirname -> do
        let testfn = dirname </> "XMLSchema.hs"
        writeFile testfn result
        action testfn

