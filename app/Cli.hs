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
import           System.IO
import           Xeno.Errors           (printExceptions)
#if !MIN_VERSION_base(4,11,0)
import           Data.Semigroup
#endif

import           Analyze
import           CodeGen
import           Flatten
import           Parser


data Opts = Opts
    { schemaFilename      :: FilePath
    , isGenerateTypesOnly :: Bool
    , generateOpts        :: GenerateOpts
    , outputToFile        :: Maybe FilePath
    } deriving Show


processSchema :: Opts -> IO ()
processSchema Opts{..} = do
    input       <- BS.readFile schemaFilename
    parseSchema input >>= (maybe (return ()) $ \schema -> do
        let (flattened, msgs) = flatten schema
        mapM_ (hPutStrLn stderr . show) msgs
        let (analyzed, schemaErrors) = analyze flattened
        null schemaErrors `unless` printExceptions input schemaErrors
        let generator | isGenerateTypesOnly = codegen
                      | otherwise           = parserCodegen generateOpts
        generatedFile <- generator analyzed
        (maybe putStrLn writeFile outputToFile) generatedFile
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
             <*> (GenerateOpts <$>
                 switch         (long "main"   <>                        help "Generate `main` function"))
             <*> (optional $
                 filenameOption (long "output" <> metavar "FILENAME"  <> help "Output generated parser to FILENAME"))
    filenameOption = strOption
