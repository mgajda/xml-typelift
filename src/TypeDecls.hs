{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DeriveDataTypeable  #-}
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
-- | Generating type declarations in code generation monad.
--   
--   This just extracts data from `Types` and produces correct syntax,
--   without any consideration for consistency of inputs.
module TypeDecls(declareAlgebraicType
                ,declareNewtype
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

import           FromXML(XMLString)
import           Code(ToCode(..), Code, TargetId, identifierLength)
import           CodeGenMonad
import           Types

-- * Low level generation of type declarations
declareAlgebraicType :: (TargetId, [NamedRec]) -> CG ()
declareAlgebraicType (_,          []                      ) = error "Empty list of records"
declareAlgebraicType (myTypeName, (firstEntry:nextEntries)) = do
    gen ["\ndata ", toCode myTypeName, " ="]
    gen ["\n    ", formatRecord firstEntry, "\n"]
    forM_ nextEntries $ \nextEntry ->
      gen ["  | ", formatRecord nextEntry]


-- * Type declarations
formatRecord :: NamedRec -> Code
formatRecord NamedRec { cons=name, fields=f:fields } =
    mconcat
      ( formatHeading f
      :(formatFollowing <$> fields))
    <> trailer
  where
    formatHeading   fld = header   <> formatField fld
    formatFollowing fld = follower <> formatField fld
    header, follower, leftPad, trailer :: Code
    header   = toCode  name    <> " {\n" <> leftPad <> "   "
    follower = "\n" <> leftPad <> " , "
    trailer  = "\n" <> leftPad <> " }"
    leftPad  = toCode
             $ BS.replicate (identifierLength name) ' '
formatRecord NamedRec { cons=name, fields=[] } = toCode name -- empty record

formatField           :: Field -> Code
formatField Field {..} = toCode name <> " :: " <> toCode fTy

declareNewtype :: TargetId -> TargetId -> Code -> CG ()
declareNewtype tyName consName baseTy = 
  gen ["\nnewtype ", toCode tyName, " = ", toCode consName, " ", baseTy, "\n"]

