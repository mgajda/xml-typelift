{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE QuasiQuotes         #-}
module Main(main) where
-- module Cli(main, testExpr) where

import           Control.Monad
import qualified Data.ByteString.Char8 as BS
import           Options.Applicative
import           Data.Default
import           Data.Maybe
import           Data.Version          (showVersion)
import           Development.GitRev    (gitHash)
import           Paths_xml_typelift    (version)
import           Text.InterpolatedString.Perl6 (qc)
import           System.Exit
import           System.IO
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
import           RunHaskellModule
import           TestUtils


data Opts = Opts
    { schemaFilename      :: FilePath
    , isGenerateTypesOnly :: Bool
    , generateOpts        :: GenerateOpts
    , testXmlFilename     :: Maybe FilePath
    , textXmlIsPrint      :: Bool
    , outputToFile        :: Maybe FilePath
    } deriving Show


processSchema :: Opts -> IO ()
processSchema Opts{..} = do
    input <- BS.readFile schemaFilename
    parseSchema input >>= (maybe (return ()) $ \schema -> do
        let (flattened, msgs) = flatten schema
        mapM_ (hPutStrLn stderr . show) msgs
        let (analyzed, schemaErrors) = analyze flattened
        null schemaErrors `unless` printExceptions input schemaErrors
        let generator | isGenerateTypesOnly = codegen
                      | otherwise           = parserCodegen generateOpts
        generatedFile <- generator analyzed
        let defoutputer = maybe putStrLn (\_ -> \_ -> return ()) testXmlFilename
        maybe defoutputer writeFile outputToFile generatedFile
        maybe (return ()) (testGeneratedParser generatedFile textXmlIsPrint) testXmlFilename
        )


-- | Compile generated parser and run it with specified XML document
--
testGeneratedParser :: String   -- ^ Generated Parser
                    -> Bool     -- ^ Print result of parsing
                    -> FilePath -- ^ XML document for test
                    -> IO ()
testGeneratedParser generatedParser isPrintParsingResult xmlFilename =
    withTempSavedFile generatedParser "XMLSchema.hs" $ \parserFilename ->
        if isPrintParsingResult then do
            checkExitCode "Fail to print out of generated parser result" $
                runHaskellModule' (def { showStdout = True }) parserFilename ["--print", xmlFilename]
        else do
            checkExitCode "Fail to run generated parser" $
                runHaskellModule' def parserFilename [xmlFilename]
            putStrLn [qc|File {xmlFilename} processed successfully|]


main :: IO ()
main = execParser' optsParser >>= processSchema


execParser' :: ParserInfo Opts -> IO Opts
execParser' = fmap postProcessOpts . execParser
  where
    postProcessOpts opts@Opts{..}
      | isGenerateTypesOnly && isJust testXmlFilename
      = error "`--types` don't compatable with `--test-document`"
      | textXmlIsPrint && isNothing testXmlFilename
      = error "Specify test XML document for parse and print"
      | isJust testXmlFilename
      = opts { generateOpts = generateOpts { isGenerateMainFunction = True }
             , isGenerateTypesOnly = False }
      | otherwise
      = opts


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
             <*> switch         (long "types"                                <> help "Generate types only")
             <*> (GenerateOpts <$>
                 switch         (long "main"                                 <> help "Generate `main` function"))
             <*> (optional $
                 filenameOption (long "test-document" <> metavar "FILENAME"  <> help "Path to test document (.xml file) \
                                                                                     \(turn on `--main` and turn off `--types`)"))
             <*> (switch        (long "print-result"                         <> help "Print result of test document parsing"))
             <*> (optional $
                 filenameOption (long "output"        <> metavar "FILENAME"  <> help "Output generated parser to FILENAME"))
    filenameOption = strOption
