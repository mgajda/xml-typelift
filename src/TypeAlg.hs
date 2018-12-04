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
-- | Assembling together type declarations using high level operators.
--
--   We have types for partial type declarations,
--   and assemble them accordingly, while having minimum wrappers.
module TypeAlg(TyCtx(..)
              ,parents
              ,tyChoice
              ,tySequence
              ,fragType
              ,wrapList
              ,wrapMaybe
              ,referType
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
import           TypeDecls

-- | Type fragment, with all context necessary to correctly allocate a name for it.
data TyCtx = TyCtx {
    schemaType  :: XMLIdNS
  , containerId :: XMLString
  , ctxName     :: XMLString
  , ty          :: HTyFrag
  }

-- | Child context with a new name, and XML namespace of this name.
parents :: TyCtx -> (XMLIdNS, XMLString) -> TyCtx
tyCtx `parents` (schTy, name) = tyCtx { containerId=ctxName tyCtx, ctxName=name, schemaType=schTy }

-- | Take type context, and return a legal Haskell type.
--   Declares new datatype if type is too complex
--   to be expressed as in-place Haskell type.
fragType :: TyCtx -> CG HType
fragType       TyCtx { ty=Whole ty } = return ty
fragType tyCtx@TyCtx { ty=Sum   _  } = declare tyCtx
fragType tyCtx@TyCtx { ty=Rec   _  } = declare tyCtx

-- | tySequence merges sequences or records of types.
tySequence, tyChoice :: [TyCtx] -> CG TyCtx
tySequence [rep]      = return      rep
tySequence (rep:reps) = foldM inSeq rep reps

-- | tyChoice merges alternatives types.
tyChoice   [alt]      = return         alt
tyChoice   (alt:alts) = foldM inChoice alt alts

ctx1@TyCtx { ty=Sum s1 } `inChoice` ctx2@TyCtx { ty=Sum s2 } = do
  return $ ctx1 { ty=Sum (s1 <> s2) }
ctx1@TyCtx { ty=Sum s  } `inChoice` ctx2@TyCtx { ty=other } = do
  alt <- NamedRec <$>  allocateConsName ctx2
                  <*> (singleField <$> allocateFieldName ctx2
                                   <*> fragType          ctx2)
  return $ ctx1 { ty=Sum (alt:s) }
  where
    singleField x y = [Field x y]

-- | `inSeq` and `inChoice` are joins in the TyCtx lattice
--    of types embedded in the context that allows
--    to name on demand.
inSeq, inChoice :: TyCtx -> TyCtx -> CG TyCtx
ctx1@TyCtx { ty=Sum _  } `inSeq` ctx2@TyCtx { ty=Sum _  } = do
  field1 <- Field <$> allocateFieldName ctx1 <*> declare ctx1
  field2 <- Field <$> allocateFieldName ctx2 <*> declare ctx2
  return $ ctx1 { ty=Rec [field1, field2] }
ctx1@TyCtx { ty=Rec r1 } `inSeq` ctx2@TyCtx { ty=Rec r2 } =
  return $ ctx1 { ty=Rec (r1 <> r2) }
ctx1@TyCtx { ty=Rec r1 } `inSeq` ctx2@TyCtx { ty=other  } = do
  name  <- allocateFieldName $ ctx2 { ctxName="content", containerId=ctxName ctx1 }
  fTy   <- fragType ctx2
  return $ ctx1 { ty=Rec (Field name fTy:r1) }

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

referType :: TyCtx -> CG HType
referType  = fmap Named . allocateTypeName

-- * These get use new identifier names.
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

