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
--   We have types for partial type declarations,
--   and assemble them accordingly, while having minimum wrappers.
module TypeDecls(Field
                ,Record
                ,declare
                ,declareAlgebraicType
                --,declareSumType
                ,declareNewtype
                ,formatRecord
                ,formatField
                ,wrapList
                ,wrapMaybe
                ) where

import           GHC.Generics
import           Prelude hiding (lookup)

import           Control.Monad  (forM_)
import qualified Data.ByteString.Char8   as BS
import qualified Data.ByteString.Builder as B
import           Data.Function(on)
import           Data.Semigroup(Semigroup(..))

import           FromXML(XMLString)
import           CodeGenMonad

--wrapList, wrapMaybe :: B.Builder -> B.Builder
wrapList  x = "["      <> x <> "]"
wrapMaybe x = "Maybe " <> x

{-
instance Show B.Builder where
  show = BS.unpack . builderString

instance Eq B.Builder where
  (==) = (==) `on` show

builderShow = show . BS.unpack . builderString
 -}
-- * Here we model Haskell types and their fragments,
--   without consideration to declaration syntax,
--   but just to syntax of their use instances.
--
--   Thus `newtype` and single-constructor `data` will be unified,
--   and the main distinction is whether type fragment can be used as-is,
--   or needs to be wrapped in suitable declaration.
-- | Type fragments during generation
data HTyFrag =
      Rec    Rec               -- ^ Record with fields
    | Sum   [NamedRec]         -- ^ Sum type with constructor identifier
    | Whole  HType             -- ^ Any type that can be used _standalone_, without declaring.
  deriving (Show)

-- | Standalone Haskell types
data HType = 
      TyExpr Code              -- ^ Type expression that is not an identifier of standalone type
    | Named  TargetId          -- ^ Type name that refers to unique `data` or `newtype` declaration
  deriving (Show)

instance ToCode HType where
  toCode (Named  tid ) = toCode tid
  toCode (TyExpr code) = code

-- | Type fragment, with all context necessary to correctly allocate a name for it.
data TyCtx = TyCtx {
    schemaType  :: XMLIdNS
  , containerId :: XMLString
  , ctxName     :: XMLString
  }

allocateTypeName, allocateConsName :: TyCtx -> CG TargetId
(allocateTypeName, allocateConsName) =
    (alloc TargetTypeName, alloc TargetConsName)
  where
    alloc haskellNamespace TyCtx {..} = 
      translate (schemaType, haskellNamespace)
                 containerId ctxName

-- | Unnamed record is just a set of fields
type Rec = [(FieldName, HType)]

-- | Single record with a constructor assigned
data NamedRec = NamedRec {
    cons   ::   TargetId
  , fields ::   Rec
  }
  deriving (Show)



-- | Get HTyFrag, and declare it as a named type.
declare :: TyCtx -> HTyFrag -> CG HType
declare tyCtx (Whole hType) = do
  ty   <- allocateTypeName tyCtx
  cons <- allocateConsName tyCtx
  declareNewtype ty cons $ "(" <> toCode hType <> ")"
  return $ Named ty
declare tyCtx (Rec rec ) = do
  ty   <- allocateTypeName tyCtx
  cons <- allocateConsName tyCtx
  declareAlgebraicType (ty, [NamedRec cons rec])
  return $ Named ty
declare tyCtx (Sum []  ) = error "Empty list of records"
declare tyCtx (Sum recs) = do
  ty <- allocateTypeName tyCtx
  declareAlgebraicType (ty, recs)
  return $ Named ty

--declareAlgebraicType :: (B.Builder, [Record]) -> CG ()
declareAlgebraicType :: (TargetId, [NamedRec]) -> CG ()
declareAlgebraicType (_,          []                      ) = error "Empty list of records"
declareAlgebraicType (myTypeName, (firstEntry:nextEntries)) = do
    gen ["\ndata ", toCode myTypeName, " ="]
    gen ["\n    ", formatRecord firstEntry, "\n"]
    forM_ nextEntries $ \nextEntry ->
      gen ["  | ", formatRecord nextEntry]


-- TODO: type alias these for safety
-- * Type declarations
type TyCon     = Code
type TyName    = Code
type FieldName = Code

type Field  = (FieldName, -- field name
               TyName)    -- field type
type Record = (TyCon,     -- Constructor name
               [Field])

type TAlt = (TyCon,           -- ^ Constructor name
             Either TyName    -- ^ Lone type under constructor
                    [Field]   -- ^ Record under constructor
            )

--formatRecord :: Record -> B.Builder
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

formatField                    :: (FieldName, HType) -> Code
formatField  (fName, fTypeName) = fName <> " :: " <> toCode fTypeName

-- | Sum type without single record field for each constructor.
type SumType = (B.Builder -- ^ Type name
               ,[SumAlt]
               )

type SumAlt = (B.Builder -- ^ Constructor name
              ,B.Builder -- ^ Type under the constructor
              )

{-
-- | Declare sum type *without* field names.
declareSumType :: SumType
               -> CG ()
declareSumType (tyName, (firstAlt:otherAlts)) =
    gen ["\ndata ", toCode tyName, " ="
        ,         genFirstAlt    firstAlt
        ,mconcat (genNextAlt <$> otherAlts)
        ,"\n"]
  where
    genFirstAlt, genNextAlt, genAlt :: SumAlt -> B.Builder
    genFirstAlt alt = "\n    " <> genAlt alt
    genNextAlt  alt = "\n  | " <> genAlt alt
    genAlt (consName, typeName) = consName <> " " <> typeName
declareSumType (tyName, []) = gen ["data ", toCode tyName, " = ", toCode tyName]
 -}
declareNewtype :: TargetId -> TargetId -> Code -> CG ()
declareNewtype tyName consName baseTy = 
  gen ["\nnewtype ", toCode tyName, " = ", toCode consName, " ", baseTy, "\n"]

