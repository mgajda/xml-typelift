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
module TypeDecls(Field
                ,Record
                ,declareAlgebraicType
                ,declareSumType
                ,formatRecord
                ,formatField
                ,wrapList
                ,wrapMaybe
                ) where

import           Prelude hiding(lookup)

import           Control.Monad(forM_)
import qualified Control.Monad.RWS.Strict   as RWS
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Builder    as B

import           CodeGenMonad

formatField :: (B.Builder, B.Builder) -> B.Builder
formatField (fName, fTypeName) = fName <> " :: " <> fTypeName

wrapList, wrapMaybe :: B.Builder -> B.Builder
wrapList  x = "["      <> x <> "]"
wrapMaybe x = "Maybe " <> x

-- * Type declarations
type Field = (B.Builder, B.Builder)
type Record = (B.Builder, [Field])

declareAlgebraicType :: [Record] -> CG ()
declareAlgebraicType []                       = error "Empty list of records"
declareAlgebraicType (firstEntry:nextEntries) = do
    RWS.tell   $ "\n    " <> formatRecord firstEntry <> "\n"
    forM_ nextEntries $ \nextEntry ->
      RWS.tell $ "  | " <> formatRecord nextEntry

formatRecord :: Record -> B.Builder
formatRecord (name, (f:fields)) =
    mconcat
      ( formatHeading f
      :(formatFollowing <$> fields))
    <> trailer
  where
    formatHeading   fld = header   <> formatField fld
    formatFollowing fld = follower <> formatField fld
    header, follower, leftPad, trailer :: B.Builder
    header   =         name    <> " {\n" <> leftPad <> "   "
    follower = "\n" <> leftPad <> " , "
    trailer  = "\n" <> leftPad <> " }"
    leftPad  = B.byteString
             $ BS.replicate (builderLength name) ' '
formatRecord (name,  []       ) =
  error $ "Cannot format empty record syntax for " <> BS.unpack (builderString name)

-- | Sum type without single record field for each constructor.
type SumType = (B.Builder -- ^ Type name
               ,[SumAlt]
               )

type SumAlt = (B.Builder -- ^ Constructor name
              ,B.Builder -- ^ Type under the constructor
              )

-- | Declare sum type *without* field names.
declareSumType :: SumType
               -> CG ()
declareSumType (tyName, (firstAlt:otherAlts)) =
    RWS.tell $ "data " <> tyName <> " =\n"
            <>          genFirstAlt    firstAlt
            <> mconcat (genNextAlt <$> otherAlts)
  where
    genFirstAlt, genNextAlt, genAlt :: SumAlt -> B.Builder
    genFirstAlt alt = "\n    " <> genAlt alt
    genNextAlt  alt = "\n  | " <> genAlt alt
    genAlt (consName, typeName) = consName <> " " <> typeName
declareSumType (tyName, []) = error $ "Cannot declare sum type with no constructors: "
                                   <> BS.unpack (builderString tyName)

