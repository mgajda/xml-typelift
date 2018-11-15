-- | Here we aim to analyze the schema.
module Analyze(analyze) where

import qualified Data.ByteString as BS

import Schema

data SchemaError =
    SchemaError { site :: Int -- ByteString index to the relevant source
                , msg  :: BS.ByteString
                }
  deriving (Show)

-- | TODO: use common code to visualize errors on the source

analyze    :: Schema -> (Schema, [SchemaError])
analyze sch = (sch, [])
