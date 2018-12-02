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
                ,declareNewtype
                ,formatRecord
                ,formatField
                ,wrapList
                ,wrapMaybe
                ) where

import           Prelude hiding(lookup)

import           Control.Monad(forM_)
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Builder    as B

import           CodeGenMonad

formatField :: (B.Builder, B.Builder) -> B.Builder
formatField (fName, fTypeName) = fName <> " :: " <> fTypeName

wrapList, wrapMaybe :: B.Builder -> B.Builder
wrapList  x = "["      <> x <> "]"
wrapMaybe x = "Maybe " <> x

-- TODO: type alias these for safety
-- * Type declarations
type TyCon     = B.Builder
type TyName    = B.Builder
type FieldName = B.Builder

type Field  = (FieldName, -- field name
               TyName)    -- field type
type Record = (TyCon,     -- Constructor name
               [Field])

type TAlt = (TyCon,           -- ^ Constructor name
             Either TyName    -- ^ Lone type under constructor
                    [Field]   -- ^ Record under constructor
            )

declareAlgebraicType :: (B.Builder, [Record]) -> CG ()
declareAlgebraicType (_,          []                      ) = error "Empty list of records"
declareAlgebraicType (myTypeName, (firstEntry:nextEntries)) = do
    gen ["\ndata ", myTypeName, " ="]
    gen ["\n    ", formatRecord firstEntry, "\n"]
    forM_ nextEntries $ \nextEntry ->
      gen ["  | ", formatRecord nextEntry]

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
formatRecord (name,  []       ) = name -- empty record

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
    gen ["\ndata ", tyName, " ="
        ,         genFirstAlt    firstAlt
        ,mconcat (genNextAlt <$> otherAlts)
        ,"\n"]
  where
    genFirstAlt, genNextAlt, genAlt :: SumAlt -> B.Builder
    genFirstAlt alt = "\n    " <> genAlt alt
    genNextAlt  alt = "\n  | " <> genAlt alt
    genAlt (consName, typeName) = consName <> " " <> typeName
declareSumType (tyName, []) = gen ["data ", tyName, " = ", tyName]

declareNewtype :: TyName -> TyCon -> TyName -> CG ()
declareNewtype tyName consName baseTy = 
  gen ["\nnewtype ", tyName, " = ", consName, " ", baseTy, "\n"]

