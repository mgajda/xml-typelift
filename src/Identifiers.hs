{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
module Identifiers(normalizeTypeName,
                   normalizeFieldName
                  ) where

import           Prelude               hiding(lookup)

import qualified Data.ByteString.Char8 as BS
import qualified Data.Set              as Set

import qualified Data.Char as Char

-- * Similar to JSON Autotype...
-- Making valid Haskell identifier out of text
normalizeFieldName ::  BS.ByteString -> BS.ByteString -> BS.ByteString
normalizeFieldName identifier = escapeKeywords                 .
                                uncapitalize                   .
                               (normalizeTypeName identifier <>) .
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
normalizeTypeName = escapeKeywords                          .
                    escapeFirstNonAlpha                     .
                    mconcat                                 .
                    map    capitalize                       .
                    filter (""==)                           .
                    BS.splitWith (not . acceptableInVariable)
  where
    acceptableInVariable c = Char.isAlpha c || Char.isDigit c
    escapeFirstNonAlpha ""                                 =        ""
    escapeFirstNonAlpha cs@(BS.head -> c) | Char.isAlpha c =        cs
    escapeFirstNonAlpha cs                                 = "_" <> cs

toUpper, toLower :: BS.ByteString -> BS.ByteString
toUpper = BS.map Char.toUpper

toLower = BS.map Char.toLower

-- | Make the first letter of a Text upper case.
capitalize     :: BS.ByteString -> BS.ByteString
capitalize word = toUpper first <> rest
  where
    (first, rest) = BS.splitAt 1 word

-- | Make the first letter of a BS.ByteString lower case.
uncapitalize :: BS.ByteString -> BS.ByteString
uncapitalize word = toLower first <> rest
  where
    (first, rest) = BS.splitAt 1 word

