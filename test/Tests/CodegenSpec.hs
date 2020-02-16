{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
module Tests.CodegenSpec where


import Control.Exception
import Control.Monad
import Language.Haskell.SourceMatch
import System.FilePath.Posix
import Language.Haskell.TH.Lib
import System.Process
import System.Exit
import System.IO
import Test.Hspec
import Text.InterpolatedString.Perl6 (qc)

import Tests.Utils

import FromXML


spec :: Spec
spec = describe "codegen" $ do
    describe "compiling" $
        forM_ ["person.xsd", "customersOrders.xsd", "shiporder.xsd", "choice.xsd", "extensions.xsd", "nested-extensions.xsd", "restriction.xsd"] $ \fn -> do
            it [qc|can compile generated types for "{fn}"|] $ example $
                tryCompile True  (inTestDir fn)
            it [qc|can compile generated parser for "{fn}"|] $ example $
                tryCompile False (inTestDir fn)
            it [qc|can parse XML generated parser for "{fn}"|] $ example $
                tryParse (inTestDir fn) (inTestDir fn -<.> "xml")
    describe "compiling types" $
        forM_ ["simple.xsd", "test.xsd", "contactExample.xsd"] $ \fn ->
            it [qc|can compile types for "{fn}"|] $ example $
                tryCompile True (inTestDir fn)
    describe "declarations presence" $ do
        it "decl.presence.1" $ example $ do
            withGeneratedFile True (inTestDir "person.xsd") $ \hsFilepath -> do
                hsFilepath `declShouldPresent`
                    [d|data Birthplace = Birthplace {
                                  city :: XMLString
                                , country :: XMLString } deriving Show|]
                hsFilepath `declShouldPresent`
                    [d|data Education = Education {
                                  degree :: XMLString
                                , yearobtained :: XMLString } deriving Show|]
                -- TODO:
                --hsFilepath `declShouldPresent`
                --    [d|data Person = Person {
                --                   name :: XMLString
                --                , age :: Integer
                --                , birthplace :: Birthplace
                --                , sex :: Integer
                --                , education :: Education }|]
        it "decl.presence.2" $ example $ do
            withGeneratedFile True (inTestDir "customersOrders.xsd") $ \hsFilepath -> do
                hsFilepath `declShouldPresent`
                    [d|data AddressType = AddressType { customerID :: Maybe XMLString,
                                                        address :: XMLString,
                                                        city :: XMLString,
                                                        region :: XMLString,
                                                        postalCode :: XMLString,
                                                        country :: XMLString} deriving Show|]


-- * --------------------------------------------------------------------------


callProcess' :: FilePath -> [String] -> IO ()
callProcess' cmd args = do
    (_, pstdout, pstderr, p) <- createProcess ((proc cmd args) { std_out = CreatePipe, std_err = CreatePipe })
    waitForProcess p >>= \case
        ExitSuccess -> do
            whenMaybe hClose pstdout
            whenMaybe hClose pstderr
        ExitFailure r -> do
            whenMaybe (dumpHandle stdout) pstdout
            whenMaybe (dumpHandle stderr) pstderr
            fail [qc|Running "{cmd}" "{args}" has failed with "{r}"|]
  where
    dumpHandle outhndl inhnd = hGetContents inhnd >>= hPutStr outhndl
    whenMaybe a m = maybe (return ()) a m


runGhc :: [String] -> IO ()
runGhc args = do
    handle (\(e::SomeException) -> do print e >> throw e) $ do
        -- TODO reimplement with 'cabal', see `runAutotype`
        -- TODO reimplement running as here: https://github.com/migamake/json-autotype/blob/master/json-autotype/src/Data/Aeson/AutoType/CodeGen/Haskell.hs
        callProcess' "stack" args


tryCompile :: Bool -> FilePath -> IO ()
tryCompile generateOnlyTypes xsdFileName =
    withGeneratedFile generateOnlyTypes xsdFileName $ \hsFilename ->
        runGhc $ ["exec", "--", "ghc", "-O0", hsFilename] ++ compileArgs
  where
    failOnWarns = False
    compileArgs | failOnWarns = ["-Wall", "-Werror"]
                | otherwise   = ["-Wno-all"]


tryParse :: FilePath -> FilePath -> IO ()
tryParse xsdFileName xmlFileName =
    withGeneratedFile False xsdFileName $ \hsFilename ->
        runGhc ["exec", "--", "runghc", hsFilename, xmlFileName]


declShouldPresent :: (HasCallStack) => FilePath -> DecsQ -> Expectation
declShouldPresent hsFilepath decl =
    isFileMatchedToDecl hsFilepath decl >>= \case
        Left err -> error (show err)
        Right _  -> return ()


inTestDir :: FilePath -> FilePath
inTestDir fn = "test" </> "data" </> fn

