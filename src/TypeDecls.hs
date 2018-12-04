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
                ,HTyFrag(..)
                ,HType(..)
                ,Rec
                ,declare
                ,declareAlgebraicType
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
import           Data.Monoid hiding (Sum)
import           Control.Applicative

import           FromXML(XMLString)
import           Code(ToCode(..), Code, TargetId, identifierLength)
import           CodeGenMonad

--wrapList, wrapMaybe :: B.Builder -> B.Builder
wrapList  x = "["      <> x <> "]"
wrapMaybe x = "Maybe " <> x

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
  , ty          :: HTyFrag
  }

-- withFields
-- choice   :: [TyCtx] -> TyCtx
-- sequence :: [TyCtx] -> TyCtx

fragType :: TyCtx -> CG HType
fragType       TyCtx { ty=Whole ty } = return ty
fragType tyCtx@TyCtx { ty=Sum   _  } = declare tyCtx
fragType _                           = error "fragType should not be applied to Rec constructor!"

ctx1@TyCtx { ty=Sum _ } `seq` ctx2@TyCtx { ty=Sum _ } = do
  field1 <- (,) <$> allocateFieldName ctx1 <*> declare ctx1
  field2 <- (,) <$> allocateFieldName ctx2 <*> declare ctx2
  return $ ctx1 { ty=Rec [field1, field2] }
ctx1@TyCtx { ty=Rec r1 } `seq` ctx2@TyCtx { ty=Rec r2 } =
  return $ ctx1 { ty=Rec (r1 <> r2) }
ctx1@TyCtx { ty=Rec r1 } `seq` ctx2@TyCtx { ty=other } = do
  field <- allocateFieldName $ ctx2 { ctxName="content", containerId=ctxName ctx1 }
  f     <- fragType ctx2
  return $ ctx1 { ty=Rec ((field, f):r1) }
allocateTypeName,
  allocateConsName,
    allocateFieldName :: TyCtx -> CG TargetId
(allocateTypeName,
 allocateConsName,
 allocateFieldName) =
    (alloc TargetTypeName
    ,alloc TargetConsName
    ,alloc TargetFieldName)
  where
    alloc haskellNamespace TyCtx {..} = 
      translate (schemaType, haskellNamespace)
                 containerId ctxName

type FieldName = TargetId
-- | Unnamed record is just a set of fields
type Field = (FieldName, HType)
type Rec   = [Field]

-- | Single record with a constructor assigned
data NamedRec = NamedRec {
    cons   ::   TargetId
  , fields ::   Rec
  }
  deriving (Show)

-- | Get HTyFrag, and declare it as a named type.
declare :: TyCtx -> CG HType
declare tyCtx@TyCtx { ty=Whole hType } = do
  ty   <- allocateTypeName tyCtx
  cons <- allocateConsName tyCtx
  declareNewtype ty cons $ "(" <> toCode hType <> ")"
  return $ Named ty
declare tyCtx@TyCtx { ty=Rec rec     } = do
  ty   <- allocateTypeName tyCtx
  cons <- allocateConsName tyCtx
  declareAlgebraicType (ty, [NamedRec cons rec])
  return $ Named ty
declare tyCtx@TyCtx { ty=Sum []      } = error "Empty list of records"
declare tyCtx@TyCtx { ty=Sum recs    } = do
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

formatField                    :: Field -> Code
formatField  (fName, fTypeName) = toCode fName <> " :: " <> toCode fTypeName

declareNewtype :: TargetId -> TargetId -> Code -> CG ()
declareNewtype tyName consName baseTy = 
  gen ["\nnewtype ", toCode tyName, " = ", toCode consName, " ", baseTy, "\n"]

