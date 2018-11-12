{-# LANGUAGE OverloadedStrings #-}
module FromXML(FromXML(..),
               makeFromXML,
               DecodingError(..),
               Result,
               AttrHandler,
               ChildHandler,
               unknownAttrHandler,
               unknownChildHandler,
               getStartIndex,
               decodeXML
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

type Result elt = Either DecodingError elt

type AttrHandler  elt = elt -> XenoAttribute -> Either BS.ByteString elt
type ChildHandler elt = elt -> Node          -> Either BS.ByteString elt

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
  Right result -> case fromXML result of
                    Left  (DecodingError errIndex errMsg) -> Left $ XenoParseError errIndex errMsg
                    Right  r                              -> Right r
  Left  e      -> Left e

-- | There isn't much point in keeping this one uninlined!
{-# INLINE CONLIKE makeFromXML #-}
-- {-# CONLIKE   makeFromXML #-}
makeFromXML :: (e, AttrHandler e, ChildHandler e) -> Xeno.Node -> Result e
makeFromXML (pristine, attrHandler, childHandler) aNode = do
  withAttrs <- foldM (catchAttrFailure  attrHandler ) pristine  (Xeno.attributes aNode)
  foldM              (catchChildFailure childHandler) withAttrs (Xeno.children   aNode)

{-# INLINE CONLIKE catchAttrFailure #-}
catchAttrFailure :: AttrHandler elt -> elt -> XenoAttribute -> Result elt
catchAttrFailure handler elt attr@(aName, _) =
  case handler elt attr of
    Left  errMsg -> Left  (DecodingError (getStartIndex aName) errMsg)
    Right r      -> Right r

{-# INLINE CONLIKE catchChildFailure #-}
catchChildFailure :: ChildHandler elt -> elt -> Xeno.Node -> Result elt
catchChildFailure handler elt node =
  case handler elt node of
    Left  errMsg -> Left  (DecodingError (getStartIndex $ Xeno.name node) errMsg)
    Right r      -> Right r

{-# INLINE CONLIKE getStartIndex #-}
getStartIndex :: BS.ByteString -> Int
getStartIndex (PS _ from _) = from

{-# INLINE CONLIKE unknownAttrHandler #-}
unknownAttrHandler :: AttrHandler elt
unknownAttrHandler   _ (aName, aVal) = Left $ "Unhandled attribute " <> bshow aName <> "=" <> bshow aVal

{-# INLINE CONLIKE unknownChildHandler #-}
unknownChildHandler :: ChildHandler elt
unknownChildHandler  _  node         = Left $ "Unhandled node " <> bshow (Xeno.name node)

bshow :: ByteString -> ByteString
bshow = BS.pack . show

