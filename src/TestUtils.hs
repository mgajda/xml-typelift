-- | Functions for running generated modules
--
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
module TestUtils
    ( withTempSavedFile
    , withPreservedSystemTempDirectory
    , checkExitCode
    ) where


import qualified Control.Monad.Catch as MC
import           System.Directory
import           System.Exit
import           System.FilePath.Posix
import           System.IO
import           System.IO.Temp
import           Text.InterpolatedString.Perl6 (qc)


-- | Save data in temporary directory with specified filename.
--   If failed, don't erase this temporary directory and outputs path to it for further debugging.
--
withTempSavedFile :: String -> FilePath -> (FilePath -> IO a) -> IO a
withTempSavedFile content filename action =
    withPreservedSystemTempDirectory "xml-typelift" $ \dirname -> do
        let tmpfn = dirname </> filename
        writeFile tmpfn content
        action tmpfn


-- * Adapted from `temporary` package

withPreservedSystemTempDirectory :: String    -- ^ Directory name template
                        -> (FilePath -> IO a) -- ^ Callback that can use the directory
                        -> IO a
withPreservedSystemTempDirectory template action =
    getCanonicalTemporaryDirectory >>= \tmpDir -> withPreservedTempDirectory tmpDir template action


withPreservedTempDirectory :: FilePath           -- ^ Parent directory to create the directory in
                           -> String             -- ^ Directory name template
                           -> (FilePath -> IO a) -- ^ Callback that can use the directory
                           -> IO a
withPreservedTempDirectory targetDir template action = do
    dirName <- createTempDirectory targetDir template
    (flip MC.onError) (hPutStrLn stderr [qc|Test directory preserved in "{dirName}"|]) $ do
        r <- action dirName
        ignoringIOErrors $ removeDirectoryRecursive dirName
        return r


ignoringIOErrors :: MC.MonadCatch m => m () -> m ()
ignoringIOErrors ioe = ioe `MC.catch` (\e -> const (return ()) (e :: IOError))


checkExitCode :: String -> (IO ExitCode) -> IO ()
checkExitCode errMsg act =
    act >>= \case
        ExitSuccess -> return ()
        errCode     -> fail [qc|{errCode}\n{errMsg}|]


