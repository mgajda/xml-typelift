-- | Reporting errors given site as ByteString
--
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy         #-}
module Errors
    ( parseError
    , parseErrorBs
    ) where


import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.ByteString.Internal(ByteString(..))
#if !MIN_VERSION_base(4,11,0)
import           Data.Semigroup
#endif


-- | Show error with context
parseError :: Int -> ByteString -> String -> a
parseError ofs inputStr msg =
    error $ msg <> "\nIn line " <> show lineCount <> " (offset " <> show ofs <> "):"
         <> BSC.unpack inputStrPart <> "\n"
         <> replicate errPtrStickLen '-' <> "^"
  where
    lineCount           = succ $ BS.count nextLineChar $ BS.take ofs inputStr
    lastLineBeforeStart = maybe 0 id $ BS.elemIndexEnd nextLineChar $ til ofs
    sndLastLineStart    = maybe 0 id $ BS.elemIndexEnd nextLineChar $ til lastLineBeforeStart
    lastLineStart       = max 0 $ max (ofs - 120) sndLastLineStart
    lastLineLen         = min 40 $ maybe 40 id $ BS.elemIndex nextLineChar (BS.drop ofs inputStr)
    til len             = BS.take len inputStr
    errPtrStickLen      = max 0 (ofs - lastLineBeforeStart - 1)
    nextLineChar        = 10 -- '\n'
    inputStrPart        = BS.take (ofs - lastLineStart + lastLineLen) $ BS.drop lastLineStart inputStr


parseErrorBs :: ByteString -> String -> a
parseErrorBs inputStr@(PS _ start _) msg =
    parseError start inputStr msg
