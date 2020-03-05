{-# LANGUAGE CPP               #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module ToXML where


import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder)
import Data.Char
import Data.Word
#if !MIN_VERSION_base(4,11,0)
import           Data.Semigroup
#endif
import qualified Data.ByteString.Builder as B


data IndentInfo = IndentInfo
    { currentIndent :: Int
    , nextIndentIncrement :: Int
    , addCarrying :: Bool
    , plainDataInline :: Bool
    }


noIndent :: IndentInfo
noIndent = IndentInfo
    { currentIndent = 0
    , nextIndentIncrement = 0
    , addCarrying = False
    , plainDataInline = True
    }


defIndent :: IndentInfo
defIndent = IndentInfo
    { currentIndent = 0
    , nextIndentIncrement = 2
    , addCarrying = True
    , plainDataInline = True
    }

-- | Convert element to bytestring Builder
--
class ToXML a where
    toXML  :: a -> B.Builder
    toXML = toXML' noIndent
    toXML' :: IndentInfo  -- ^ Indent level
           -> a           -- ^ Object to convert to XML
           -> B.Builder


nextIndent :: IndentInfo -> IndentInfo
nextIndent ii@IndentInfo{..} = ii { currentIndent = currentIndent + nextIndentIncrement }


c2w :: Char -> Word8
c2w = fromIntegral . ord
{-# INLINE c2w #-}


bspaces :: Int -> Builder
bspaces cnt = mconcat $ replicate cnt (B.word8 $ c2w ' ')
{-# INLINE bspaces #-}


tag :: IndentInfo
    -> ByteString
    -> Builder
    -> Builder
tag ii tagName content = taga ii tagName [] content
{-# INLINE tag #-}


taga :: IndentInfo
     -> ByteString
     -> [(ByteString, ByteString)]
     -> Builder
     -> Builder
taga IndentInfo{currentIndent,addCarrying} tagName attributes content = mconcat
    [ spaces, "<",  tagName', attributes', closeTag
    ,             content
    , spaces, "</", tagName',              closeTag
    ]
  where
    spaces = bspaces currentIndent
    closeTag = if addCarrying then ">\n" else ">"
    tagName' = B.byteString tagName
    attributes' = mconcat $ map (\(n,v) -> " " <> B.byteString n <> "=\"" <> B.byteString v <> "\"") attributes
{-# INLINE taga #-}


-- TODO refactor
tags :: IndentInfo -> ByteString -> ByteString -> Builder
tags ii tagName bscontent
    | plainDataInline ii = mconcat
        [ spaces,  "<", tag', ">"
        ,             B.byteString bscontent
        ,          "</", tag', closeTag
        ]
    | otherwise = tag ii tagName content'
  where
    spaces = bspaces $ currentIndent ii
    closeTag = if addCarrying ii then ">\n" else ">"
    tag' = B.byteString tagName
    content' = spaces' <> B.byteString bscontent <> (if addCarrying ii then "\n" else mempty)
    spaces' = bspaces (currentIndent $ nextIndent ii)
{-# INLINE tags #-}
