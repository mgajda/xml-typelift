{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
module Identifiers(normalizeTypeName,
                   normalizeFieldName,
                   rejectInvalidTypeName
                  ) where

import           Prelude               hiding(lookup)

import qualified Data.ByteString.Char8 as BS
import qualified Data.Set              as Set

import qualified Data.Char as Char

--import Debug.Trace

-- * Similar to JSON Autotype...
-- Making valid Haskell identifier out of text
normalizeFieldName :: BS.ByteString -> BS.ByteString
normalizeFieldName = escapeKeywords                 .
                     uncapitalize                   .
                     normalizeTypeName

keywords :: Set.Set BS.ByteString
keywords  = Set.fromList ["type", "data", "module", "class", "where", "let", "do"]

escapeKeywords ::  BS.ByteString -> BS.ByteString
escapeKeywords k | k `Set.member` keywords = k <> "_"
escapeKeywords k                           = k

-- | Normalize type name by:
-- 1. Treating all characters that are not acceptable in Haskell variable name as end of word.
-- 2. Capitalizing each word, but a first (camelCase).
-- 3. Adding underscore if first character is non-alphabetic.
-- 4. Escaping Haskell keywords if the whole identifier is such keyword.
-- 5. If identifier is empty, then substituting "JsonEmptyKey" for its name.
normalizeTypeName :: BS.ByteString -> BS.ByteString
normalizeTypeName = escapeFirstNonAlpha                     .
                    mconcat                                 .
                    map    capitalize                       .
                    filter (""/=)                           .
                    BS.splitWith (not . acceptableInVariable)
  where
    acceptableInVariable c = isAcceptableFirstChar c || Char.isAlpha c || Char.isDigit c
    escapeFirstNonAlpha ""                                          =        ""
    escapeFirstNonAlpha cs@(BS.head -> c) | isAcceptableFirstChar c =        cs
    escapeFirstNonAlpha cs                                          = "_" <> cs
    isAcceptableFirstChar '_' = True
    isAcceptableFirstChar c   = Char.isAlpha c

rejectInvalidTypeName   :: BS.ByteString -> Bool
rejectInvalidTypeName bs = BS.head bs == '_'

toUpper, toLower :: BS.ByteString -> BS.ByteString
toUpper = BS.map Char.toUpper

toLower = BS.map Char.toLower

-- | Make the first letter of a BS.ByteString upper case.
capitalize     :: BS.ByteString -> BS.ByteString
capitalize ""   = ""
capitalize word@(BS.head -> c) | c == '_' = BS.cons '_' $ capitalize (BS.tail word)
capitalize word = toUpper first <> rest
  where
    (first, rest) = BS.splitAt 1 word

-- | Make the first letter of a BS.ByteString lower case.
uncapitalize :: BS.ByteString -> BS.ByteString
uncapitalize "" = ""
uncapitalize word@(BS.head -> c) | c == '_' = BS.cons '_' $ uncapitalize (BS.tail word)
uncapitalize word = toLower first <> rest
  where
    (first, rest) = BS.splitAt 1 word

