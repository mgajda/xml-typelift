{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Main(main) where
-- module Cli(main, testExpr) where

import           Control.Monad
import qualified Data.ByteString.Char8   as BS
import           Options.Applicative
-- import           Text.Pretty.Simple
import           Xeno.Errors(printExceptions)
import           Data.Version (showVersion)
import           Development.GitRev (gitHash)
import           Paths_xml_typelift (version)

import           Analyze
import           CodeGen
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
        let (analyzed, schemaErrors) = analyze schema
        -- pPrint analyzed
        null schemaErrors `unless` printExceptions input schemaErrors
        -- putStrLn "Analysis:"
        -- printExceptions input $ check analyzed
        -- putStrLn "\n===== Datatypes:"
        generatedTypes <- codegen analyzed
        if isGenerateTypesOnly then do
            putStrLn generatedTypes
        else do
            -- **************
            generatedParser <- parserCodegen analyzed
            putStrLn generatedTypes
            putStrLn "\n-- Parser\n\n"
            putStrLn generatedParser
            ---- putStrLn generatedParser
            --writeFile "Result.hs" generatedTypes
            --appendFile "Result.hs" "\n\n\n\n-- *** PARSER *** --\n\n\n\n"
            --appendFile "Result.hs" generatedParser
            --appendFile "Result.hs" "\n\nmain :: IO ()\n"
            --appendFile "Result.hs" "main = do\n"
            --appendFile "Result.hs" "  (parseRootToArray <$> BS.readFile \"test/customersOrders.xml\") >>= \\case\n"
            --appendFile "Result.hs" "    Left str -> Prelude.putStrLn (\"ERROR: \" ++ str)\n"
            --appendFile "Result.hs" "    Right tl@(TopLevelInternal _ vec) -> do\n"
            --appendFile "Result.hs" "        pPrint (UV.take 10 vec)\n"
            --appendFile "Result.hs" "        let root = extractRoot tl\n"
            --appendFile "Result.hs" "        Prelude.putStrLn \"~~~~~~~\"\n"
            --appendFile "Result.hs" "        pPrint root\n"
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
        Opts <$> filenameOption (long "schema"              <> metavar "FILENAME"  <> help "Path to XML schema (.xsd file)")
             <*> switch         (long "generate-types-only" <>                        help "Generate types only")
    filenameOption = strOption

