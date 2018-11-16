{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE FlexibleContexts  #-}
-- | Here we aim to analyze the schema.
module Analyze(analyze, check) where

import           Data.Maybe(catMaybes)
import qualified Data.ByteString as BS

import FromXML(getStartIndex)
import Schema
import Data.Generics.Uniplate.Operations

data SchemaError =
    SchemaError { site :: Int -- ByteString index to the relevant source
                , msg  :: BS.ByteString
                }
  deriving (Show)

-- | TODO: use common code to visualize errors on the source

analyze    :: Schema -> (Schema, [SchemaError])
analyze sch = (sch, [])

-- | Check desired properties that should be kept after flattening
check    :: Schema -> [SchemaError]
check sch = mconcat [
    test isRestriction          "Restrictions present"
  , test isExtension            "Extensions present"
  , test referenceToNonBaseType "Reference to non-base type"
  ]
  where
    -- | Test predicate on entire Schema, and return index of first violation, if present.
    test pred msg = case catMaybes $ map pred $ universeBi sch of
                      (i:_) -> [SchemaError i msg]
                      []    -> []

type Test t = Biplate Schema t => t -> Maybe Int

-- | Tests to be performed:
isRestriction (Restriction {base}) = Just $ getStartIndex base
isRestriction  _                   = Nothing

isExtension   (Extension   {base}) = Just $ getStartIndex base
isExtension    _                   = Nothing

referenceToNonBaseType :: Type -> Maybe Int
referenceToNonBaseType (Ref aType) | aType `elem` predefinedTypes = Nothing
referenceToNonBaseType (Ref aType)                                = Just $ getStartIndex aType
referenceToNonBaseType  _                                         = Nothing
