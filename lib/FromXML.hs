{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
{-# LANGUAGE ViewPatterns      #-}
module FromXML(FromXML(..),
               makeFromXML,
               DecodingError(..),
               Result,
               AttrHandler,
               ChildHandler,
               unknownAttrHandler,
               unknownChildHandler,
               getStartIndex,
               decodeXML,
               failHere,
               splitNS,
               stripNS,
               bshow,
               revTake,
               printExceptions,
               displayException
              ) where

import           Control.Monad(foldM)
import qualified Data.ByteString.Char8 as BS hiding (elem)
import           Data.ByteString.Internal(ByteString(..))
import           System.IO(stderr)
import           Xeno.Types as Xeno
import           Xeno.DOM as Xeno

type XenoAttribute = (ByteString, ByteString)

data DecodingError = DecodingError { errorIndex   :: {-# UNPACK #-} !Int
                                   , errorMessage :: {-# UNPACK #-} !BS.ByteString }
  deriving (Show, Eq)

type Result elt = Either XenoException elt

type AttrHandler  elt = elt -> XenoAttribute -> Either XenoException elt
type ChildHandler elt = elt -> Node          -> Either XenoException elt

{-# INLINE CONLIKE failHere #-}
failHere :: BS.ByteString -> BS.ByteString -> Either XenoException a
failHere msg here = Left $ XenoParseError (getStartIndex here) msg

-- Attribute handler for a given type of element
class FromXML elt where
  -- | Main entry to the parser
  fromXML  :: Xeno.Node -> Result elt
  fromXML' :: Xeno.Node -> Result elt
  -- | For faster parsing, `fromXML'` does not check the top element name
  --   just assumes that we checked it at level above in another element.
  fromXML = fromXML'

decodeXML      :: FromXML elt => BS.ByteString -> Either XenoException elt
decodeXML input = case Xeno.parse input of
  Right result -> fromXML result
  Left  e      -> Left    e

-- | There isn't much point in keeping this one uninlined!
{-# INLINE CONLIKE makeFromXML #-}
-- {-# CONLIKE   makeFromXML #-}
makeFromXML :: (AttrHandler e, ChildHandler e) -> e -> Xeno.Node -> Result e
makeFromXML (attrHandler, childHandler) pristine aNode = do
  withAttrs <- foldM attrHandler pristine               (Xeno.attributes aNode)
  foldM                          childHandler withAttrs (Xeno.children   aNode)

{-# INLINE CONLIKE getStartIndex #-}
getStartIndex :: BS.ByteString -> Int
getStartIndex (PS _ from _) = from

{-# NOINLINE unknownAttrHandler #-}
unknownAttrHandler :: BS.ByteString -> XenoAttribute -> Result elt
unknownAttrHandler desc (splitNS -> (ns, aName), aVal) =
  (  "Unhandled attribute of " <> desc
  <> " in namespace '" <> ns
  <> "' : '" <> aName <> "' = '" <> aVal <> "'")
        `failHere` aName

bshow :: Show a => a -> BS.ByteString
bshow = BS.pack . show

{-# INLINABLE splitNS #-}
splitNS :: ByteString -> (ByteString, ByteString)
splitNS eltName = (dropLastByte ns, elt)
  where
    (ns, elt) = BS.breakEnd (==':') eltName

-- | Drop last character
dropLastByte :: BS.ByteString -> BS.ByteString
dropLastByte (PS ptr start len) | len >= 1 = PS ptr start (len-1)
dropLastByte  bs                           = bs

-- | Take n last bytes.
revTake :: Int -> BS.ByteString -> BS.ByteString
revTake i (PS ptr from to) = PS ptr (end-len) len
  where
    end = from + to
    len = min to i

{-# INLINABLE stripNS #-}
stripNS :: ByteString -> ByteString
stripNS = snd . splitNS

{-# NOINLINE unknownChildHandler #-}
unknownChildHandler          :: BS.ByteString -> Xeno.Node -> Result elt
unknownChildHandler desc node = ("Unhandled child of " <> desc <> " '" <> eltName <> "'")
                                   `failHere` eltName
  where
    eltName = Xeno.name node

-- | Print schema errors with excerpts
printExceptions :: BS.ByteString -> [XenoException] -> IO ()
printExceptions i s = (BS.hPutStrLn stderr . displayException i) `mapM_` s

-- | Find line number of the error from ByteString index.
lineNo :: Int -> BS.ByteString -> Int
lineNo index bs = BS.count '\n'
                $ BS.take index bs

displayException :: BS.ByteString -> XenoException -> BS.ByteString
displayException input (Xeno.XenoParseError i msg) =
               "Decoding error in line " <> bshow (lineNo i input) <> ": "
            <> msg
            <> " at:\n"
            <> lineContentBeforeError
            <> lineContentAfterError
            <> "\n" <> pointer
  where
    lineContentBeforeError = snd $ BS.spanEnd   ('\n'/=) $ revTake 40 $ BS.take i input
    lineContentAfterError  =       BS.takeWhile ('\n'/=) $ BS.take 40 $ BS.drop i input
    pointer                = BS.replicate (BS.length lineContentBeforeError) ' ' <> "^"
displayException _      err                        = bshow err

