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
-- | Assembling together type declarations using high level operators.
--
--   We have types for partial type declarations,
--   and assemble them accordingly, while having minimum wrappers.
--
--   Type context contains either a descriptor for standalone
--   Haskell type declaration, or part of, *and* also context
--   information that allows to generate name(s) for its
--   type name, fields etc.
--   
--   We need to keep the context, so that we can name field
--   or type on demand, and reduce number of unnecessary wrappers.
--
--   For simplicity we do not distinguish instances where we use only a context,
--   versus those that we pass type with a context.
--
--   TODO: Make it a type class that both on standalone context, and context with type.
module TypeCtx(--TyCtx(..)
              --,parents
               declare
              ,declareIfAbsent
              ,tyChoice
              ,tySequence
              --,fragType
              ,wrapList
              ,wrapMaybe
              --,referType
              --,freshInnerCtx
              ,enumCons
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
import           Code(ToCode(..), Code, TargetId(..), identifierLength, SeedId(..))
import           CodeGenMonad
import           Types
import           TypeDecls
import           Schema(TyPart(..))

import           Debug.Trace(trace)

{-
-- | Type fragment, with all context necessary to correctly allocate a name for it.
data TyCtx = TyCtx {
    ty    :: HTyFrag
  , scope :: Scope
  } deriving (Show)
 -}

-- | Take type context, and return a legal Haskell type.
--   Declares new datatype if type is too complex
--   to be expressed as in-place Haskell type.
{-fragType :: TyCtx -> CG HType
fragType       TyCtx { ty=Whole ty } = return  ty
fragType tyCtx@TyCtx { ty=Sum   _  } = declare tyCtx
fragType tyCtx@TyCtx { ty=Rec   _  } = declare tyCtx
 -}

-- | tyChoice merges alternatives types.
tyChoice :: [HTyFrag] -> CG HTyFrag
tyChoice [   ] = error "tyChoice applied to empty list of arguments"
tyChoice [alt] = return alt
tyChoice  alts = Sum <$> concatMapM getAlt alts
  where
    getAlt :: HTyFrag -> CG [NamedRec]
    getAlt (Sum  subs     ) = return subs
    getAlt (Rec [Field nam t]) =
      (:[]) <$> enumCons [Field nam t] (seed nam)
    getAlt (Rec  fields   ) =
      (:[]) <$> enumCons  fields "alt" -- TODO: find constructor name
    getAlt (Whole  t      ) = do
      fName <- innerScope "content" $ translate TargetFieldName
      (:[]) <$> enumCons [Field fName t] (seed t) -- TODO: find constructor and field name

concatMapM f = fmap concat . mapM f

{-
asAlt :: TyCtx -> CG NamedRec
asAlt ctx = NamedRec <$>  allocateConsName ctx
                     <*> (singleField <$> allocateFieldName ctx
                                      <*> fragType          ctx)
  where
    singleField x y = [Field x y]
 -}

-- | Constructor without any record data.
enumCons :: Rec -> XMLString -> CG NamedRec
enumCons rec consName = do
  inEnum consName $ NamedRec <$> translate TargetConsName <*> pure rec

-- TODO: handle empty types better!
-- Discard empty types
 {-
inChoice :: TyCtx -> TyCtx -> CG TyCtx
TyCtx     { ty=Rec []  } `inChoice`  ctx@TyCtx { ty=Whole t } = return $ ctx { ty=Whole $ wrapMaybe t }
TyCtx     { ty=Rec []  } `inChoice`  ctx                      = return   ctx
TyCtx     { ty=Sum []  } `inChoice`  ctx                      = return   ctx
TyCtx     { ty=Whole t } `inChoice`  ctx@TyCtx { ty=Rec []  } = return $ ctx { ty=Whole $ wrapMaybe t }
ctx                      `inChoice`      TyCtx { ty=Rec []  } = return   ctx -- TODO: mark this as empty option!
ctx                      `inChoice`      TyCtx { ty=Sum []  } = return   ctx
ctx1@TyCtx { ty=Sum s1 } `inChoice` ctx2@TyCtx { ty=Sum s2  } = do
  return $ ctx1 { ty=Sum (s1 <> s2) }
ctx1@TyCtx { ty=Sum s  } `inChoice` ctx2@TyCtx { ty=other   } = do
    alt      <- asAlt ctx2
    innerCtx <- freshInnerCtx ctx1 "choice"
    return    $ innerCtx { ty=Sum (alt:s) }
ctx1@TyCtx { ty=_      } `inChoice` ctx2@TyCtx { ty=_       } = do
    alt1 <- asAlt ctx1
    alt2 <- asAlt ctx2
    innerCtx <- freshInnerCtx ctx1 "choice"
    return    $ innerCtx { ty=Sum [alt1, alt2] }
  where
    singleField x y = [Field x y]

field :: TyCtx -> XMLString -> HTyFrag -> CG TyCtx
field tyCtx fieldName frag =
    singleField <$> allocateFieldName myCtx
                <*> fragType         (myCtx { ty=frag })
  where
    singleField name ty = myCtx { ty=Rec [Field name ty] }
    myCtx               = tyCtx `parents` (AttributeName, fieldName)
 -}

-- | tySequence merges sequences or records of types.
tySequence :: [HTyFrag] -> CG HTyFrag
tySequence []         = error "tySequence applied to empty list of arguments"
tySequence [rep]      = return rep
tySequence reps@(r:_) = do
    frags <- concat <$> mapM mkRep reps
    case frags of
      [               ] -> return r
      --[Field fName fTy] -> TyCtx fName fTy
      other             -> return $ Rec other
  where
    mkRep :: HTyFrag -> CG [Field]
    mkRep (Rec  [ ]) = return   [ ]
    mkRep (Rec   r ) = return    r
    --mkRep  inner     = do
    --mkRep (Sum   s ) = fragType $ Sum s
    mkRep inner      = do
      inTy  <- declareIfCompound inner
      fName <- innerScope "inner" $ translate TargetFieldName
      return [Field fName inTy]

declareIfCompound (Whole aTy) = return aTy
declareIfCompound  other      = declare other

{-tySequence (rep:reps) = do
  seqs <- foldM inSeq rep reps
  freshInnerCtx seqs "sequence"
 -}


-- | `inSeq` and `inChoice` are joins in the TyCtx lattice
--    of types embedded in the context that allows
--    to name on demand.
{-
inSeq :: TyCtx -> TyCtx -> CG TyCtx
TyCtx      { ty=Rec [] } `inSeq` ctx                      = return ctx
TyCtx      { ty=Sum [] } `inSeq` ctx                      = return ctx
ctx                      `inSeq`      TyCtx { ty=Rec [] } = return ctx
ctx                      `inSeq`      TyCtx { ty=Sum [] } = return ctx
ctx1@TyCtx { ty=Rec r1 } `inSeq` ctx2@TyCtx { ty=Rec r2 } =
  return $ ctx1 { ty=Rec (r1 <> r2) }
ctx1@TyCtx { ty=Rec r1 } `inSeq` ctx2@TyCtx { ty=other  } = trace ("Other is " <> show other) $ do
  fTy   <- fragType ctx2
  newCtx <- freshInnerCtx ctx1 "content"
  name  <- allocateFieldName newCtx
  return $ ctx1 { ty=Rec (Field name fTy:r1) }
ctx1@TyCtx { ty=other } `inSeq` ctx2@TyCtx { ty=Rec r2  } = do
  fTy    <- fragType      ctx1
  newCtx <- freshInnerCtx ctx2 "after"
  name   <- allocateFieldName newCtx
  return  $ ctx2 { ty=Rec (Field name fTy:r2) }
-- | In this case, it is probably special of attribute + simple content type
--   No inner context name seems needed.
ctx1@TyCtx { ty=_  } `inSeq` ctx2@TyCtx { ty=_ } = do
  field1 <- Field <$> allocateFieldName ctx1 <*> declare ctx1
  field2 <- Field <$> allocateFieldName ctx2 <*> declare ctx2
  return  $ ctx1 { ty=Rec [field1, field2] }
 -}

{-
-- | Get HTyFrag, and declare it as a named type.
declare :: HType -> CG HType
declare (Whole hType) = do
  ty   <- translate 
  case hType of
    Named t | t == ty -> do
       return   hType
    Named t           -> do
       cons <- allocateConsName tyCtx
       declareNewtype ty cons $ toCode hType
       return $ Named ty
    _                 -> do
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
 -}

--declare 
declare :: HTyFrag -> CG HType
declare (Rec fields) = do
  cons <- translate TargetConsName
  ty   <- translate TargetTypeName
  declareAlgebraicType (ty, [NamedRec { cons, fields }])
  return $ Named ty
declare (Whole aTy ) = return aTy
declare (Sum   []  ) = error "Empty list of records"
declare (Sum   recs) = do
  ty <- translate TargetTypeName
  declareAlgebraicType (ty, recs)
  return $ Named ty

declareIfAbsent tyFrag@(Whole (Named n)) = do
  ty <- translate TargetTypeName
  if n == ty
     then return $ Named n
     else declare tyFrag
declareIfAbsent tyFrag = declare tyFrag

{-
declareIfAbsent tyCtx@TyCtx { ty=Whole (Named n) } = do
  ty <- allocateTypeName tyCtx
  if n == ty
     then return (Named n)
     else declare tyCtx
declareIfAbsent tyCtx = declare tyCtx
 -}

--referType = undefined
--referType :: TyCtx -> CG HType
--referType  = fmap Named . allocateTypeName

{-
-- * These get use new identifier names.
allocateTypeName,
  allocateConsName,
    allocateFieldName :: HTyFrag -> CG TargetId
(allocateTypeName ,
 allocateConsName ,
 allocateFieldName) =
    (alloc TargetTypeName
    ,alloc TargetConsName
    ,alloc TargetFieldName)
  where
    alloc haskellNamespace = CG $ translate haskellNamespace
 -}

