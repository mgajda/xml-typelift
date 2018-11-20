{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE ViewPatterns        #-}
-- | Translating base types
--   Checking if a given type is
--   predefined Haskell type,
module BaseTypes(fromBaseXMLType
                ,baseHaskellType
                ,baseHaskellTypes
                ,predefinedTypes
                ,isSimple
                ) where

import           Prelude hiding(lookup)

import qualified Data.Set                   as Set
import           Data.String

import           Schema

-- | Translating base XML types.
fromBaseXMLType :: (Eq a, IsString a, IsString b) => a -> b
fromBaseXMLType s = case s of
  "any"                -> "Xeno.Node"
  "string"             -> "String"
  "token"              -> "String"
  "integer"            -> "Int" -- or Integer
  "positiveInteger"    -> "Int" -- or Integer
  "float"              -> "Float"
  "date"               -> "Date"
  "decimal"            -> "Int"
  "double"             -> "Double"
  _                    -> "Xeno.Node" -- or error?\

-- | Check if builder makes Haskell base type
baseHaskellType :: XMLString -> Bool
baseHaskellType = (`Set.member` baseHaskellTypes)

-- | List of predefined Haskell types that we use.
baseHaskellTypes :: Set.Set XMLString
baseHaskellTypes  = Set.fromList $ map fromBaseXMLType
                                 $ Set.toList predefinedTypes

predefinedTypes :: Set.Set XMLString
predefinedTypes = Set.fromList [
    "any"
  , "string"
  , "token"
  , "integer"
  , "positiveInteger"
  , "float"
  , "date"
  ]

isSimple :: Type -> Maybe Bool
isSimple (Ref x)
        | x    `Set.member` predefinedTypes = Just True
isSimple Restriction { base }
        | base `Set.member` predefinedTypes = Just True -- not always!!!
isSimple  Extension {}                      = Just False
isSimple  Complex   {}                      = Just False
isSimple  _                                 = Nothing -- no idea, need dictionary

