{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns         #-}
-- | Generating type declarations in code generation monad.
--
--   We have types for partial type declarations,
--   and assemble them accordingly, while having minimum wrappers.
module Types(Field   (..)
            ,HTyFrag (..)
            ,HType   (..)
            ,NamedRec(..)
            ,Rec
            ,wrapList
            ,wrapMaybe
            ,anyXML
            ) where

import           GHC.Generics
import           Prelude hiding (lookup)

import           Control.Monad  (forM_)
import qualified Data.ByteString.Char8   as BS
import qualified Data.ByteString.Builder as B
import           Data.Function(on)
import           Data.Semigroup(Semigroup(..))
import           Data.Monoid hiding (Sum)
import           Control.Applicative
import           Control.Monad

import           Code(ToCode(..), SeedId(..), Code, TargetId(..), identifierLength)

wrapList, wrapMaybe :: HType -> HType
wrapList  ty = TyExpr $ "[" <> toCode ty <> "]"

wrapMaybe ty = TyExpr $ "Maybe " <> toCode ty

-- * Here we model Haskell types and their fragments,
--   without consideration to declaration syntax,
--   but just to syntax of their use instances.
--
--   Thus `newtype` and single-constructor `data` will be unified,
--   and the main distinction is whether type fragment can be used as-is,
--   or needs to be wrapped in suitable declaration.
-- | Type fragments during generation
data HTyFrag =
      Rec    Rec       -- ^ Record with fields
    | Sum   [NamedRec] -- ^ Sum type with constructor identifier
    | Whole  HType     -- ^ Any type that can be used _standalone_, without declaring.
  deriving (Show)

anyXML = Named $ TargetId "Xeno.Node"

-- | Standalone Haskell types
data HType =
      TyExpr Code      -- ^ Type expression that is not an identifier of standalone type
    | Named  TargetId  -- ^ Type name that refers to unique `data` or `newtype` declaration
  deriving (Show)

instance ToCode HType where
  toCode (Named  tid ) = toCode tid
  toCode (TyExpr code) = code

type FieldName = TargetId
-- | Unnamed record is just a set of fields
data Field = Field { name :: FieldName
                   , fTy  :: HType
                   }
  deriving (Show)
type Rec   = [Field]

-- | Single record with a constructor assigned
data NamedRec = NamedRec {
    cons   ::   TargetId
  , fields ::   Rec
  }
  deriving (Show)

instance SeedId HType where
  seed (TyExpr c  ) = seed c
  seed (Named  tid) = seed tid

instance SeedId Rec where
  seed = mconcat . map (seed . name)

instance SeedId NamedRec where
  seed (NamedRec name _) = seed name

instance SeedId HTyFrag where
  seed (Sum   s) = mconcat $ map seed s
  seed (Rec   r) = seed r
  seed (Whole t) = seed t

