-- | Utilities for processing XML files
--
module Tests.Utils where


import           Control.Monad
import           System.Directory
import           System.FilePath.Posix
import           System.IO
import           System.IO.Temp
import qualified Control.Monad.Catch as MC
import qualified Data.ByteString.Char8 as BS

import Analyze
import CodeGen
import Flatten
import Parser


withGeneratedFile :: Bool -> FilePath -> (FilePath -> IO ()) -> IO ()
withGeneratedFile generateOnlyTypes xmlFilename action = do
    input <- BS.readFile xmlFilename
    (Just schema) <- parseSchema input
    let (flattened, msgs) = flatten schema
    unless (null msgs) $ error ("Flattened with errors: " ++ show msgs)
    let (analyzed, schemaErrors) = analyze flattened
    unless (null schemaErrors) $ error "Schema has errors"
    result <- (if generateOnlyTypes then codegen else parserCodegen) analyzed
    withPreservedSystemTempDirectory "xml-typelift" $ \dirname -> do
        let testfn = dirname </> "XMLSchema.hs"
        writeFile testfn result
        action testfn

-- * Adapted from `temporary` package

withPreservedSystemTempDirectory :: String   -- ^ Directory name template
                        -> (FilePath -> IO a) -- ^ Callback that can use the directory
                        -> IO a
withPreservedSystemTempDirectory template action =
    getCanonicalTemporaryDirectory >>= \tmpDir -> withPreservedTempDirectory tmpDir template action


withPreservedTempDirectory :: FilePath -- ^ Parent directory to create the directory in
                           -> String   -- ^ Directory name template
                           -> (FilePath -> IO a) -- ^ Callback that can use the directory
                           -> IO a
withPreservedTempDirectory targetDir template action = do
    dirName <- createTempDirectory targetDir template
    (do r <- action dirName
        ignoringIOErrors $ removeDirectoryRecursive dirName
        return r)
            `MC.onError`
            (hPutStrLn stderr $ "Test directory preserved in \"" ++ dirName ++ "\"")


ignoringIOErrors :: MC.MonadCatch m => m () -> m ()
ignoringIOErrors ioe = ioe `MC.catch` (\e -> const (return ()) (e :: IOError))
