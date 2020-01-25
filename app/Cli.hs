{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Main(main) where
-- module Cli(main, testExpr) where

import           Control.Monad
import qualified Data.ByteString.Char8 as BS
import           Options.Applicative
-- import           Text.Pretty.Simple
import           Data.Version          (showVersion)
import           Development.GitRev    (gitHash)
import           Paths_xml_typelift    (version)
import           Xeno.Errors           (printExceptions)

import           Analyze
import           CodeGen
import           Flatten
import           Parser


data Opts = Opts
    { schemaFilename      :: FilePath
    , isGenerateTypesOnly :: Bool
    } deriving Show


processSchema :: Opts -> IO ()
processSchema Opts{..} = do
    input       <- BS.readFile schemaFilename
    parseSchema input >>= (maybe (return ()) $ \schema -> do
        -- putStrLn $ "Successfully parsed " <> filename <> ": " <> show schema
        let (flattened, msgs) = flatten schema
        forM_ msgs print
        let (analyzed, schemaErrors) = analyze flattened
        null schemaErrors `unless` printExceptions input schemaErrors
        if isGenerateTypesOnly then do
            generatedTypes <- codegen analyzed
            putStrLn generatedTypes
        else do
            generatedParser <- parserCodegen analyzed
            putStrLn generatedParser
        )


main :: IO ()
main = execParser optsParser >>= processSchema


optsParser :: ParserInfo Opts
optsParser =
    info (helper <*> versionOption <*> programOptions)
         (fullDesc <> progDesc "Generates types and parser for XML files by XML schema (.xsd) files" <>
             header "XML Typelift command line interface")
  where
    versionOption :: Parser (a -> a)
    versionOption = infoOption
        (concat [showVersion version, " ", $(gitHash)])
        (long "version" <> help "Show version")
    programOptions :: Parser Opts
    programOptions =
        Opts <$> filenameOption (long "schema" <> metavar "FILENAME"  <> help "Path to XML schema (.xsd file)")
             <*> switch         (long "types"  <>                        help "Generate types only")
    filenameOption = strOption
