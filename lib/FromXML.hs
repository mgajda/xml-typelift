{-# LANGUAGE OverloadedStrings #-}
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
               revTake
              ) where

import           Control.Monad(foldM)
import qualified Data.ByteString.Char8 as BS hiding (elem)
import           Data.ByteString.Internal(ByteString(..))
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
  withAttrs <- foldM attrHandler pristine  (Xeno.attributes aNode)
  foldM                          childHandler  withAttrs (Xeno.children   aNode)

{-# INLINE CONLIKE catchAttrFailure #-}
catchAttrFailure :: (elt -> XenoAttribute -> Either BS.ByteString elt)
                 ->  AttrHandler                           elt
catchAttrFailure handler elt attr@(aName, _) =
  case handler elt attr of
    Left  errMsg -> errMsg `failHere` aName
    Right r      -> Right  r

{-# INLINE CONLIKE getStartIndex #-}
getStartIndex :: BS.ByteString -> Int
getStartIndex (PS _ from _) = from

{-# INLINE CONLIKE unknownAttrHandler #-}
unknownAttrHandler :: XenoAttribute -> Result elt
unknownAttrHandler (splitNS -> (ns, aName), aVal) = ("Unhandled attribute in namespace '" <> ns
                                                  <> "' : '" <> aName <> "' = '" <> aVal <> "'")
                                                       `failHere` aName

bshow :: Show a => a -> BS.ByteString
bshow = BS.pack . show

{-# INLINABLE splitNS #-}
splitNS :: ByteString -> (ByteString, ByteString)
splitNS name = (dropLastByte ns, elt)
  where
    (ns, elt) = BS.breakEnd (==':') name

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

{-# INLINE CONLIKE unknownChildHandler #-}
unknownChildHandler     :: Xeno.Node -> Result elt
unknownChildHandler node = ("Unhandled node '" <> eltName <> "'")
                              `failHere` eltName
  where
    eltName = Xeno.name node

