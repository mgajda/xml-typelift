-- | Generating type declarations in code generation monad.
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
module TypeDecls( Record
                , TyData(..)
                , TyCon(..)
                , TyType(..)
                , TyFieldName(..)
                , TyField
                , SumAlt
                , declareAlgebraicType
                , declareSumType
                , declareNewtype
                ) where


import           Control.Monad
import qualified Data.ByteString.Builder    as B
import           Language.Haskell.TH.Syntax as TH hiding (SumAlt)

import           CodeGenMonad


-- TODO: type alias these for safety
-- * Type declarations
newtype TyData      = TyData B.Builder
newtype TyCon       = TyCon B.Builder
newtype TyType      = TyType B.Builder
newtype TyFieldName = TyFieldName B.Builder

type TyField  = (TyFieldName, -- field name
                 TyType)    -- field type
type Record = (TyCon,     -- Constructor name
               [TyField])

-- | Sum type without single record field for each constructor.
type SumType = (TyData -- ^ Type name
               ,[SumAlt]
               )


type SumAlt = (TyCon -- ^ Constructor name
              ,TyType -- ^ Type under the constructor
              )


newName'' :: B.Builder -> Q Name
newName'' bsn = return $ mkName $ bToS bsn


newConstrName :: TyCon -> Q Name
newConstrName (TyCon bsn) = newName'' bsn


newDataName :: TyData -> Q Name
newDataName (TyData bsn) = newName'' bsn


newTypeName :: TyType -> Q Name
newTypeName (TyType bsn) = newName'' bsn


newFieldName :: TyFieldName -> Q Name
newFieldName (TyFieldName bsn) = newName'' bsn


-- | Creates 'deriving Show' clause
#if MIN_VERSION_template_haskell(2,12,0)
makeShowDc :: Q DerivClause
makeShowDc = do
    showDc <- newName'' (B.byteString "Show")
    return $ DerivClause Nothing [ConT showDc]
#else
makeShowDc :: Q Pred
makeShowDc = do
    showDc <- newName'' (B.byteString "Show")
    return $ ConT showDc
#endif


declareAlgebraicType :: (TyData, [Record]) -> CG ()
declareAlgebraicType    (_,          []) = error "Empty list of records"
declareAlgebraicType    (tyDataName,   records) =
    out $ do dataName <- newDataName tyDataName
             recs     <- mapM formatRecord records
             showDc   <- makeShowDc
             return $ DataD [] dataName [] Nothing recs [showDc]


formatRecord :: Record -> Q Con
formatRecord (name, fields) = do
    recName <- newConstrName name
    recFields <- forM fields $ \(fieldName, fieldType) -> do
        thFieldName <- newFieldName fieldName
        -- TODO: try to restore type with `reify :: Name -> Q Info`, which can get write type name
        --       `reify` can't work in IO, so we can use prebuilded dicts
        thFieldType <- ConT <$> newTypeName fieldType
        return  (thFieldName, noBang, thFieldType)
    return (RecC recName recFields)


-- | Declare sum type *without* field names.
declareSumType :: SumType
               -> CG ()
declareSumType (tyName, []) =
    out $ do dataName <- newDataName tyName
             showDc   <- makeShowDc
             return $ DataD [] dataName [] Nothing [NormalC dataName []] [showDc]
declareSumType (tyDataName, sumTypes) =
    out $ do dataName <- newDataName tyDataName
             constrs  <- mapM (uncurry mkNormalC) sumTypes
             showDc   <- makeShowDc
             return $ DataD [] dataName [] Nothing constrs [showDc]


declareNewtype :: TyData -> TyCon -> TyType -> CG ()
declareNewtype tyDataName tyConstr baseTy =
    out $ do dataName <- newDataName tyDataName
             constr   <- mkNormalC tyConstr baseTy
             showDc   <- makeShowDc
             return $ NewtypeD [] dataName [] Nothing constr [showDc]


mkNormalC :: TyCon -> TyType -> Q TH.Con
mkNormalC tyConstr tyName@(TyType bsn) = do
    constrName   <- newConstrName tyConstr
    if builderIsNull bsn then do
        return $ NormalC constrName []
    else do
        baseTypeName <- newTypeName tyName
        return $ NormalC constrName [(noBang, ConT baseTypeName)]


noBang :: Bang
noBang = Bang NoSourceUnpackedness NoSourceStrictness

