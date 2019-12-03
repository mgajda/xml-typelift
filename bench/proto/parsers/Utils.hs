module Utils where


import Data.ByteString (ByteString)
import Data.Time.Format
import Data.Time.LocalTime(ZonedTime)
import FromXML
import Xeno.DOM as Xeno
import qualified Data.ByteString.Char8 as BSC


-- | Extract text from node (and all its subnodes)
--
allText :: Node -> ByteString
allText node = mconcat [txt | Text txt <- contents node]


-- | Get text from node and from all its subnodes, wrapped in `FromXML.Result`
--
allTextM :: Node -> Result ByteString
allTextM = return . allText


-- | Convert text presentation of datetime to ZonedTime
--
zonedTimeStrM :: (Monad m) => ByteString -> m ZonedTime
zonedTimeStrM = parseTimeM True defaultTimeLocale fmt . BSC.unpack
  where
    fmt = iso8601DateFormat (Just "%H:%M:%S")


-- | Extract all text from node and convert string presentation of datetime to ZonedTime
--
zonedTimeM :: (Monad m) => Node -> m ZonedTime
zonedTimeM = zonedTimeStrM . allText


-- | Extract all text from node and parse it
--
readM :: (Read a) => Node -> Result a
readM = fmap (read . BSC.unpack) . allTextM
