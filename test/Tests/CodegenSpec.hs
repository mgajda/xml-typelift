{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE LambdaCase  #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell #-}
module Tests.CodegenSpec where


import Control.Exception
import Language.Haskell.SourceMatch
import System.FilePath.Posix
import Language.Haskell.TH.Lib
import System.Process
import Test.Hspec
--import Text.InterpolatedString.Perl6 (qc)

import Tests.Utils

import FromXML


spec :: Spec
spec = describe "codegen" $ do
    describe "compiling" $ do
        it "can compile 1" $ example $ do
            tryCompile False ("test" </> "person.xsd")
        it "can compile 2" $ example $ do
            tryCompile False ("test" </> "customersOrders.xsd")
        it "can compile 3" $ example $ do
            tryCompile False ("test" </> "shiporder.xsd")
        it "can compile 4" $ example $ do
            tryCompile False ("test" </> "contactExample.xsd")
    describe "declarations presence" $ do
        it "decl.presence.1" $ example $ do
            withGeneratedFile ("test" </> "person.xsd") $ \hsFilepath -> do
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
            withGeneratedFile ("test" </> "customersOrders.xsd") $ \hsFilepath -> do
                hsFilepath `declShouldPresent`
                    [d|data AddressType = AddressType { name :: Maybe XMLString,
                                                        address :: XMLString,
                                                        city :: XMLString,
                                                        region :: XMLString,
                                                        postalCode :: XMLString,
                                                        country :: XMLString} deriving Show|]


-- * --------------------------------------------------------------------------


tryCompile :: Bool -> FilePath -> IO ()
tryCompile failOnWarns xmlFilename =
    withGeneratedFile xmlFilename $ \hsFilename ->
        (callProcess "stack" $ ["exec", "--", "ghc", "-O0", hsFilename] ++ compileArgs)
            `catch`
            (\(e::SomeException) -> do print e >> throw e)
        -- TODO reimplement with 'cabal', see `runAutotype`
        -- in https://gitlab.com/migamake/json-autotype/blob/master/json-autotype/test/TestExamples.hs
  where
    compileArgs | failOnWarns = ["-Wall", "-Werror"]
                | otherwise   = ["-Wno-all"]


declShouldPresent :: (HasCallStack) => FilePath -> DecsQ -> Expectation
declShouldPresent hsFilepath decl =
    isFileMatchedToDecl hsFilepath decl >>= \case
        Left err -> error (show err)
        Right _  -> return ()

