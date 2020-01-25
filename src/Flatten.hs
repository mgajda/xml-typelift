{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonoLocalBinds             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE ViewPatterns               #-}
-- | Here we aim to analyze the schema.
module Flatten(flatten) where

import           Control.Monad.Reader
import           Control.Monad.RWS     (MonadRWS (..), MonadReader (..),
                                        MonadState (..), MonadWriter (..), RWS,
                                        evalRWS, gets, modify')
import qualified Data.Map.Strict       as Map
import           Data.Maybe            (catMaybes, isJust)

import           Data.Functor.Identity
import           FromXML               (XMLString, getStartIndex)

import           BaseTypes             (isSimple, isXSDBaseType)
import           Schema

data Message = Message {
                 inType  :: FScope
               , content :: String
               }

instance Show Message where
  show Message {..} = concat ["In ", show inType, " ", content]

data FScope = ScopeElement XMLString
            | ScopeType    XMLString
            | ScopeGlobal

instance Show FScope where
  show  ScopeGlobal           = "top level"
  show (ScopeType    ty     ) = "complex type " <> show ty
  show (ScopeElement eltName) = "element "      <> show eltName

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
      (unFlattener . flattenType) `mapM_` Map.keys types
      tops'      <- (unFlattener . flattenElt) `mapM` tops
      finalTypes <- get
      return Schema { types=finalTypes, tops=tops', namespace}
      -- TODO: flatten element types
    flattenType :: XMLString -> Flattener ()
    flattenType key =
      Flattener $
        local (const $ ScopeType key) $ do
          typ    <- gets (Map.! key)
          typ'   <- unFlattener $ act typ
          modify' $ Map.adjust (const typ') key
    flattenElt :: Element -> Flattener Element
    flattenElt elt@Element { eName, eType } =
      Flattener $
        local (const $ ScopeElement eName) $ do
          eType' <- unFlattener $ act eType
          return  $ elt { eType=eType' }

deriving instance MonadReader FScope                    Flattener
deriving instance MonadWriter        [Message]          Flattener
deriving instance MonadState                   TypeDict Flattener
deriving instance MonadRWS    FScope [Message] TypeDict Flattener

report :: String -> Flattener ()
report issue = Flattener $ do
  scope <- ask
  tell [Message scope issue]

{-
  | Restriction {
        base       :: !XMLString
      , restricted :: !Restriction
      }
  | Extension {
        base  :: !XMLString
      , mixin :: !Type
      } -- ^ Extension of complexType
  | Complex {
        mixed :: !Bool
      , attrs :: ![Attr]
      , inner :: !TyPart
      }
 -}

{-
allFlatElts = all isFlatElt
  where
    -- Is it a straight reference, or a more complex type?
    isFlatElt (Ref _) = True
    isFlatElt other   = isJust $ isSimple other
  -}

allFlatElts = all isFlatElt

isFlatElt (Elt   e) = True
isFlatElt (Group g) = True
isFlatElt  _        = False

-- | Is it a single-level structure, or does it need splitting?
isFlat (Ref _) = return True
isFlat (Restriction {base}) | isXSDBaseType base =
  return True
isFlat (Restriction {base}) = do
  report $ "Restriction of non-base type: " <> show base
  return False
isFlat (Complex { inner=Seq (allFlatElts -> True) }) =
  return True
-- TODO: accept seq extensions of
isFlat (Complex {inner=Seq s}) = undefined
isFlat (Complex { attrs=[], inner=Choice (allFlatElts -> True) }) =
  return True
isFlat (Complex { attrs, inner=Choice []}) =
  return True
isFlat (Extension {base,
                   mixin=Complex {inner=Seq (allFlatElts -> True)}}) = return False
isFlat (Extension {base}) = return False

tyFlatten :: Type -> Flattener Type
tyFlatten ty@Complex { mixed
                     , attrs
                     , inner } =
  case inner of
    Seq {} -> do
      report $ "Seq in Complex type: " <> show  ty
      return ty
    Choice {} -> do
      report $ "Choice in Complex type: " <> show  ty
      return ty
    All {} -> do
      report $ "No special treatment for xs:all yet: " <> show ty
      return ty
    -- | Named elements, and groups are already flat
    other -> return ty
--tyFlatten Extension {base, mixin} = undefined
tyFlatten ty = return ty

flatten :: Schema -> (Schema, [Message])
flatten schema = runFlattener schema tyFlatten
