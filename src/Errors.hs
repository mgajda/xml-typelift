{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE NamedFieldPuns      #-}
-- Reporting errors given site as ByteString
module Errors(parseError) where

import Prelude hiding (id)

--import Data.Monoid
import Data.ByteString.Char8 as BS
import Data.ByteString.Internal(ByteString(..))

-- | Since all ByteStrings have the pointer to initial input,
--   we can show the preceding data and count lines before error.
--   This hack looks dirty hack, but is still safe.
parseError :: ByteString -> String -> a
parseError (PS ptr start len) msg =
    error $ msg <> "\nIn line " <> show lineCount
         <>        " after:\n"
         <> BS.unpack (PS ptr lastLineStart (end-lastLineStart)) <> "\n"
  where
    end           = start+len
    lineCount     = BS.count '\n' $ PS ptr 0 start
    sndLastLineStart = do
      lastLineBeforeStart <- BS.elemIndexEnd '\n' $ til start
      BS.elemIndexEnd                        '\n' $ til lastLineBeforeStart
    lastLineStart = case sndLastLineStart of
                      Nothing              -> defaultStart
                      Just i | start-i>120 -> defaultStart
                      Just i               -> i+1
    defaultStart  = max    0 $ start-40
    til           = PS ptr 0

