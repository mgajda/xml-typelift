{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
{-# LANGUAGE ViewPatterns      #-}
module FromXML(FromXML(..),
               makeFromXML,
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
               printExceptions,
               displayException
              ) where

import           Control.Monad(foldM)
import qualified Data.ByteString.Char8 as BS hiding (elem)
import           Data.ByteString.Internal(ByteString(..))
import           System.IO(stderr)
import           Xeno.Types as Xeno
import           Xeno.DOM as Xeno
import           Xeno.Errors

type XMLString = ByteString

type XenoAttribute = (ByteString, ByteString)

type Result elt = Either XenoException elt

type AttrHandler  elt = elt -> XenoAttribute -> Either XenoException elt
type ChildHandler elt = elt -> Node          -> Either XenoException elt

-- | Show for ByteStrings
bshow :: Show a => a -> BS.ByteString
bshow = BS.pack . show

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
makeFromXML         (attrHandler,
                     childHandler) pristine                    aNode = do
  withAttrs <- foldM attrHandler   pristine  $ Xeno.attributes aNode
  foldM              childHandler  withAttrs $ Xeno.children   aNode

{-# NOINLINE unknownAttrHandler #-}
unknownAttrHandler :: BS.ByteString -> XenoAttribute -> Result elt
unknownAttrHandler desc (splitNS -> (ns, aName), aVal) =
  (  "Unhandled attribute of " <> desc
  <> " in namespace '" <> ns
  <> "' : '" <> aName <> "' = '" <> aVal <> "'")
        `failHere` aName

{-# INLINABLE splitNS #-}
splitNS :: ByteString -> (ByteString, ByteString)
splitNS eltName = (dropLastByte ns, elt)
  where
    (ns, elt) = BS.breakEnd (==':') eltName

-- | Drop last character
dropLastByte :: BS.ByteString -> BS.ByteString
dropLastByte (PS ptr start len) | len >= 1 = PS ptr start (len-1)
dropLastByte  bs                           = bs

{-# INLINABLE stripNS #-}
stripNS :: ByteString -> ByteString
stripNS = snd . splitNS

{-# NOINLINE unknownChildHandler #-}
unknownChildHandler          :: BS.ByteString -> Xeno.Node -> Result elt
unknownChildHandler desc node = ("Unhandled child of " <> desc <> " '" <> eltName <> "'")
                                   `failHere` eltName
  where
    eltName = Xeno.name node

