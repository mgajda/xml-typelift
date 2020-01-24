{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonoLocalBinds             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE ViewPatterns               #-}
-- | Here we aim to analyze the schema.
module Flatten(flatten) where

import           Control.Monad.RWS.Strict
import qualified Data.Map.Strict                   as Map
import           Data.Maybe                        (catMaybes)

import           Data.Functor.Identity
import           Data.Generics.Uniplate.Operations
import           FromXML                           (XMLString, getStartIndex)
import           Xeno.Types                        (XenoException (..))

import           BaseTypes
import           Schema

data Message = Message {
                 inType  :: XMLString
               , content :: String
               }

data FScope = ScopeElement XMLString
            | ScopeType    XMLString
            | ScopeGlobal

-- | Flattening monad
newtype Flattener a =
  Flattener {
    unFlattener :: RWS  FScope   -- Processed type
                       [Message] -- Messages
                        TypeDict -- Dictionary of types
                        a
  } deriving (Functor, Applicative, Monad)

runFlattener ::  Schema                  -- input schema
             -> (Type -> Flattener Type) -- transform function
             -> (Schema, [Message])      -- resulting schema and messages
runFlattener Schema { types, tops, namespace } act =
    evalRWS loop ScopeGlobal types
  where
    loop = do
      flattenType `mapM_` Map.keys types
      tops'      <- flattenElt `mapM` tops
      finalTypes <- get
      return Schema { types=finalTypes, tops=tops', namespace}
      -- TODO: flatten element types
    flattenType key = local (const $ ScopeType key) $ do
      typ    <- gets (Map.! key)
      typ'   <- unFlattener $ act typ
      modify' $ Map.adjust (const typ') key
    flattenElt elt@Element { eName, eType } = local (const $ ScopeElement eName) $ do
      eType' <- unFlattener $ act eType
      return  $ elt { eType=eType' }

deriving instance MonadReader FScope                    Flattener
deriving instance MonadWriter        [Message]          Flattener
deriving instance MonadState                   TypeDict Flattener
deriving instance MonadRWS    FScope [Message] TypeDict Flattener

-- * Analysis
type SchemaError = XenoException
-- | TODO: use common code to visualize errors on the source

analyze    :: Schema -> (Schema, [SchemaError])
analyze sch = (sch, [])

-- * Quality check after analysis
-- | Check desired properties that should be kept after flattening
check    :: Schema -> [SchemaError]
check sch = mconcat [
  --test isRestriction          "Restrictions present"
    test isExtension            "Extensions present"
  , test referenceToNonBaseType "Reference to non-base type"
  ]
  where
    -- | Test predicate on entire Schema, and return index of first violation, if present.
    test aTest msg = case catMaybes $ map aTest $ universeBi sch of
                      (i:_) -> [XenoParseError i msg]
                      []    -> []

type Test t = Biplate Schema t => t -> Maybe Int



{-
-- | Tests to be performed:
isRestriction :: Test Type
isRestriction (Restriction {base}) = Just $ getStartIndex base
isRestriction  _                   = Nothing
 -}

-- | Check if there are unexpanded extensions.
isExtension :: Test Type
isExtension   (Extension   {base}) = Just $ getStartIndex base
isExtension    _                   = Nothing

-- | Check if there are unexpanded references to user-defined types.
referenceToNonBaseType :: Test Type
referenceToNonBaseType (Ref (isBaseHaskellType -> True)) = Nothing
--referenceToNonBaseType (Ref  aType)                      = Just $ getStartIndex aType
referenceToNonBaseType  _                                = Nothing
