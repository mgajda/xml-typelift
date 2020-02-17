{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE QuasiQuotes         #-}
module Main(main) where
-- module Cli(main, testExpr) where

import           Control.Monad
import qualified Data.ByteString.Char8 as BS
import           Options.Applicative
import           Data.Version          (showVersion)
import           Development.GitRev    (gitHash)
import           Paths_xml_typelift    (version)
import           Text.InterpolatedString.Perl6 (qc)
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
import           TestUtils


data Opts = Opts
    { schemaFilename      :: FilePath
    , isGenerateTypesOnly :: Bool
    , generateOpts        :: GenerateOpts
    , testXmlFilename     :: Maybe FilePath
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
        let defoutputer = maybe putStrLn (\_ -> \_ -> return ()) testXmlFilename
        (maybe defoutputer writeFile outputToFile) generatedFile
        unless isGenerateTypesOnly $ do
            maybe putStrLn testGeneratedParser testXmlFilename generatedParser
        )


-- | Compile generated parser and run it with specified XML document
--
testGeneratedParser :: FilePath -- ^ XML document for test
                    -> String   -- ^ Generated Parser
                    -> IO ()
testGeneratedParser xmlFilename generatedParser =
    withTempSavedFile generatedParser "XMLSchema.hs" $ \parserFilename -> do
        runHaskellModule parserFilename [xmlFilename]
        putStrLn [qc|File {xmlFilename} processed successfully|]


main :: IO ()
main = execParser' optsParser >>= processSchema


execParser' :: ParserInfo Opts -> IO Opts
execParser' = fmap postProcessOpts . execParser
  where
    postProcessOpts opts@Opts{..}
      | isJust testXmlFilename = opts { generateOpts = generateOpts { isGenerateMainFunction = True }
                                      , isGenerateTypesOnly = False }
      | otherwise              = opts


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
        Opts <$> filenameOption (long "schema"        <> metavar "FILENAME"  <> help "Path to XML schema (.xsd file)")
             <*> switch         (long "types"         <>                        help "Generate types only")
             <*> (GenerateOpts <$>
                 switch         (long "main"          <>                        help "Generate `main` function"))
             <*> (optional $
                 filenameOption (long "test-document" <> metavar "FILENAME"  <> help "Path to test document (.xml file) \
                                                                                     \(turn on `--main` and turn off `--types`)"))
             <*> (optional $
                 filenameOption (long "output" <> metavar "FILENAME"         <> help "Output generated parser to FILENAME"))
    filenameOption = strOption
