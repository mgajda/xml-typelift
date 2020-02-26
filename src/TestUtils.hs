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
    when verbose $ putStrLn [qc|Run "{cmd}" with args: {args}|]
    (_, pstdout, pstderr, p) <- createProcess ((proc cmd args) { std_out = if showStdout then Inherit else CreatePipe, std_err = CreatePipe })
    waitForProcess p >>= \case
        ExitSuccess -> do
            unless showStdout $ whenMaybe hClose pstdout
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
    when verbose $ do
        let showEnv env = lookupEnv env >>= (\e -> putStrLn [qc|>>> {env} = {e}|])
        showEnv "STACK_EXE"
        showEnv "CABAL_SANDBOX_CONFIG"
        showEnv "GHC_ENVIRONMENT"
        showEnv "GHC_PACKAGE_PATH"
        showEnv "HASKELL_DIST_DIR"
        showEnv "XML_TYPELIFT_STACK_FLAGS"
        -- putStrLn "Environment: -----------"
        -- getEnvironment >>= (mapM_ $ \(env,val) -> putStrLn [qc|{env} = "{val}"|])
        -- putStrLn "End of environment -----"
    stack    <- lookupEnv "STACK_EXE"
    oldCabal <- lookupEnv "CABAL_SANDBOX_CONFIG"
    newCabal <- lookupEnv "HASKELL_DIST_DIR"
    stackFlags <- (maybe [] (:[])) <$> lookupEnv "XML_TYPELIFT_STACK_FLAGS"
    let res@(exe, exeArgs') | Just stackExec <- stack    = (stackExec, stackFlags ++ [tool, "--"])
                            | Just _         <- oldCabal = ("cabal", ["exec", tool, "--"])
                            | Just _         <- newCabal = ("cabal", ["v2-exec", tool, "--"] ++ additionalPackagesArgs)
                            | otherwise                  = (tool, [])
        exeArgs = case ghcTool of
                    Compiler -> exeArgs' ++ ["-O0"]
                    Runner   -> exeArgs'
    when verbose $ putStrLn [qc|Use exe "{exe}", and additional arguments: {exeArgs}|]
    return res
  where
    tool = case ghcTool of
               Runner   -> "runghc"
               Compiler -> "ghc"
    -- New cabal can't find packages, so it is need to specify it explicit
    mkAdditionalPackagesArg arg = case ghcTool of
               Runner   -> "--ghc-arg=-package " ++ arg
               Compiler ->            [qc|-package {arg}|]
    additionalPackagesArgs =
        map mkAdditionalPackagesArg additionalPackages
    additionalPackages = ["iso8601-duration", "xeno", "xml-typelift"] -- TODO: try to get from package.yaml via Template Haskell


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


