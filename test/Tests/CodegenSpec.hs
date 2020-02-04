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
--import Text.InterpolatedString.Perl6 (qc)

import Tests.Utils

import FromXML


spec :: Spec
spec = describe "codegen" $ do
    describe "compiling" $
        forM_ ["person.xsd", "customersOrders.xsd", "shiporder.xsd"] $ \fn ->
        -- forM_ ["person.xsd", "customersOrders.xsd", "shiporder.xsd", "contactExample.xsd"] $ \fn ->
            forM_ [True, False] $ \isTestTypesGeneration -> do
                let genType = if isTestTypesGeneration then "types " else "parser"
                it ("can compile " ++ genType ++ " for \"" ++ fn ++ "\"") $ example $
                    tryCompile isTestTypesGeneration ("test" </> "data" </> fn)
    describe "declarations presence" $ do
        it "decl.presence.1" $ example $ do
            withGeneratedFile True ("test" </> "data" </> "person.xsd") $ \hsFilepath -> do
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
            withGeneratedFile True ("test" </> "data" </> "customersOrders.xsd") $ \hsFilepath -> do
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
            fail ("Running \"" ++ cmd ++ "\" " ++ show args ++ " has failed with " ++ show r)
  where
    dumpHandle outhndl inhnd = hGetContents inhnd >>= hPutStr outhndl
    whenMaybe a m = maybe (return ()) a m


tryCompile :: Bool -> FilePath -> IO ()
tryCompile generateOnlyTypes xmlFilename =
    withGeneratedFile generateOnlyTypes xmlFilename $ \hsFilename ->
        (callProcess' "stack" $ ["exec", "--", "ghc", "-O0", hsFilename] ++ compileArgs)
            `catch`
            (\(e::SomeException) -> do print e >> throw e)
        -- TODO reimplement with 'cabal', see `runAutotype`
        -- in https://gitlab.com/migamake/json-autotype/blob/master/json-autotype/test/TestExamples.hs
  where
    failOnWarns = False
    compileArgs | failOnWarns = ["-Wall", "-Werror"]
                | otherwise   = ["-Wno-all"]


declShouldPresent :: (HasCallStack) => FilePath -> DecsQ -> Expectation
declShouldPresent hsFilepath decl =
    isFileMatchedToDecl hsFilepath decl >>= \case
        Left err -> error (show err)
        Right _  -> return ()

