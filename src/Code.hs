{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonoLocalBinds             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE ViewPatterns               #-}
-- | Monad for code generation:
--   Mostly deals with keeping track of all
--   generated code as "Builder",
--   keeping track of unique translation
--   for each XML identifier into Haskell
--   type or field name.
module Code(-- Code generation monad
            Code    (..)
           ,ToCode  (..)
           ,TargetId(..)
           ,identifierLength

           -- Utilities
           ,builderUnlines
           ,builderString
           ,builderLength
           ,bshow
           ) where

import           Prelude hiding(lookup)

import           Control.Lens as Lens
import qualified Control.Monad.RWS.Strict   as RWS
import           Data.Monoid
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy       as BSL(toStrict, length)
import qualified Data.ByteString.Builder    as B
import qualified Data.Map.Strict            as Map
import qualified Data.Set                   as Set

import           FromXML(XMLString)
import           Data.String
import           Identifiers
import           BaseTypes


-- | Wrap the generated strings in here to avoid confusion.
newtype Code = Code { unCode :: B.Builder }

instance Semigroup Code where
  Code a <> Code b = Code (a <> b)

instance Monoid Code where
  mempty = Code mempty
  Code a `mappend` Code b = Code (a `mappend` b)

newtype TargetId = TargetId XMLString
  deriving (Eq, Show)

identifierLength (TargetId t) = BS.length t

class ToCode a where
  toCode :: a -> Code

instance ToCode String where
  toCode = Code . B.string7

instance ToCode XMLString where
  toCode = Code . B.byteString

instance IsString Code where
  fromString = Code . B.string7

instance ToCode TargetId where
  toCode (TargetId s) = toCode s

-- TODO: add keywords to prevent mapping of these
bshow :: Show a => a -> BS.ByteString
bshow = BS.pack . show

builderUnlines :: [B.Builder] -> B.Builder
builderUnlines []     = ""
builderUnlines (l:ls) = l <> mconcat (("\n" <>) <$> ls)

-- | Convert builder back to String, if you need to examine the content.
builderString :: B.Builder -> BS.ByteString
builderString  = BSL.toStrict . B.toLazyByteString

instance Show Code where
  show = show . builderString . unCode

builderLength :: B.Builder -> Int
builderLength  = fromIntegral . BSL.length . B.toLazyByteString

