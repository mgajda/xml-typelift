{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MonoLocalBinds    #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE ViewPatterns      #-}
-- | Here we aim to analyze the schema.
module Analyze(analyze, check) where

import           Data.Maybe(catMaybes)
--import qualified Data.ByteString as BS

import FromXML(getStartIndex, stripNS)
import Xeno.Types(XenoException(..))
import Data.Generics.Uniplate.Operations

import BaseTypes
import Schema

-- * Analysis
type SchemaError = XenoException
-- | TODO: use common code to visualize errors on the source

analyze    :: Schema -> (Schema, [SchemaError])
analyze sch = (sch, [])

-- * Quality check after analysis
-- | Check desired properties that should be kept after flattening
check    :: Schema -> [SchemaError]
check sch = mconcat [
  --test isRestriction          "Restrictions present"
    test isExtension            "Extensions present"
  , test referenceToNonBaseType "Reference to non-base type"
  ]
  where
    -- | Test predicate on entire Schema, and return index of first violation, if present.
    test aTest msg = case catMaybes $ map aTest $ universeBi sch of
                      (i:_) -> [XenoParseError i msg]
                      []    -> []

type Test t = Biplate Schema t => t -> Maybe Int



{-
-- | Tests to be performed:
isRestriction :: Test Type
isRestriction (Restriction {base}) = Just $ getStartIndex base
isRestriction  _                   = Nothing
 -}

-- | Check if there are unexpanded extensions.
isExtension :: Test Type
isExtension   (Extension   {base}) = Just $ getStartIndex base
isExtension    _                   = Nothing

-- | Check if there are unexpanded references to user-defined types.
referenceToNonBaseType :: Test Type
referenceToNonBaseType (Ref (isBaseHaskellType . stripNS -> True)) = Nothing
referenceToNonBaseType (Ref  aType)                                = Just $ getStartIndex aType
referenceToNonBaseType  _                                          = Nothing

