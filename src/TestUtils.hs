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


import           Control.Exception
import           Control.Monad
import qualified Control.Monad.Catch as MC
import           Data.Default
import           System.Directory
import           System.Environment
import           System.Exit
import           System.FilePath.Posix
import           System.IO
import           System.IO.Temp
import           System.Process
import           Text.InterpolatedString.Perl6 (qc)

    {-

data RunOptions = RunOptions
        { verbose    :: Bool
        , showStdout :: Bool
        }


instance Default RunOptions where
    def = RunOptions { verbose = False
                     , showStdout = False
                     }


data GhcTool = Runner | Compiler


-- | Call specified process with args and print its output when it fails.
--
callProcess' :: RunOptions -> FilePath -> [String] -> IO ()
callProcess' RunOptions{..} cmd args = do
    (_, pstdout, pstderr, p) <- createProcess ((proc cmd args) { std_out = if showStdout then Inherit else CreatePipe, std_err = CreatePipe })
    waitForProcess p >>= \case
        ExitSuccess -> do
            -- whenMaybe hClose pstdout
            whenMaybe hClose pstderr
        ExitFailure r -> do
            whenMaybe (dumpHandle stdout) pstdout
            whenMaybe (dumpHandle stderr) pstderr
            fail [qc|Running "{cmd}" "{args}" has failed with "{r}"|]
  where
    dumpHandle outhndl inhnd = hGetContents inhnd >>= hPutStr outhndl
    whenMaybe a m = maybe (return ()) a m


findGhc :: RunOptions -> GhcTool -> IO (FilePath, [String])
findGhc RunOptions{..} ghcTool = do
    stack <- lookupEnv "STACK_EXE"
    cabal <- lookupEnv "CABAL_SANDBOX_CONFIG"
    when verbose $ putStrLn [qc|STACK_EXE={stack} ; CABAL_SANDBOX_CONFIG={cabal}|]
    let res@(exe, exeArgs') | Just stackExec <- stack = (stackExec, [tool, "--"])
                            | Just _         <- cabal = ("cabal", ["exec", tool, "--"])
                            | otherwise               = (tool, [])
        exeArgs = case ghcTool of
                    Compiler -> exeArgs' ++ ["-O0"]
                    Runner   -> exeArgs
    when verbose $ putStrLn [qc|Use exe "{exe}", and additional arguments: {exeArgs}|]
    return res
  where
    tool = case ghcTool of
               Runner   -> "runghc"
               Compiler ->  "ghc"


passModuleToGhc :: RunOptions -> GhcTool -> FilePath -> [String] -> IO ()
passModuleToGhc ro ghcTool moduleFilename args =
    handle (\(e::SomeException) -> do print e >> throw e) $ do
        (exe, exeArgs) <- findGhc ro ghcTool
        callProcess' ro exe (exeArgs ++ moduleFilename:args)


-- | Find ghc with cabal/stack and run it with specified arguments
--
compileHaskellModule :: FilePath -> [String] -> IO ()
compileHaskellModule moduleFilename args = passModuleToGhc def Compiler moduleFilename args


-- | Run Haskell module in specified file with arguments
--
runHaskellModule' :: RunOptions -> FilePath -> [String] -> IO ()
runHaskellModule' ro moduleFilename args = passModuleToGhc ro Runner moduleFilename args


runHaskellModule :: FilePath -> [String] -> IO ()
runHaskellModule moduleFilename args = runHaskellModule' def moduleFilename args

-}

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


