-- | Utilities for processing XML files
--
{-# LANGUAGE RecordWildCards #-}
module Tests.Utils where


import           Control.Monad
import           Data.Default
import qualified Data.ByteString.Char8 as BS

import Analyze
import CodeGen
import Flatten
import Parser
import TestUtils


data TestGeneratedOpts = TestGeneratedOpts
    { generateOnlyTypes :: Bool
    , generateUnsafe    :: Bool
    }

instance Default TestGeneratedOpts where
    def = TestGeneratedOpts { generateOnlyTypes = True
                            , generateUnsafe    = False
                            }


withGeneratedFile :: TestGeneratedOpts -> FilePath -> (FilePath -> IO ()) -> IO ()
withGeneratedFile TestGeneratedOpts{..} xmlFilename action = do
    input <- BS.readFile xmlFilename
    (Just schema) <- parseSchema input
    let (flattened, msgs) = flatten schema
    unless (null msgs) $ error ("Flattened with errors: " ++ show msgs)
    let (analyzed, schemaErrors) = analyze flattened
    unless (null schemaErrors) $ error "Schema has errors"
    result <- (if generateOnlyTypes then codegen else parserCodegen) (def { isGenerateMainFunction = True, isUnsafe = generateUnsafe }) analyzed
    withTempSavedFile result "XMLSchema.hs" action

