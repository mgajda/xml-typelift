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

runFlattener ::  Schema                  -- ^ input schema
             -> (Type -> Flattener Type) -- ^ transform function
             -> (Schema, [Message])      -- ^ resulting schema and messages
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

allFlatElts :: [TyPart] -> Bool
allFlatElts = all isFlatElt

isFlatElt :: TyPart -> Bool
isFlatElt (Elt   e) = True
isFlatElt (Group g) = True
isFlatElt  _        = False

-- | Is it a single-level structure, or does it need splitting?
isFlat :: Type -> Flattener Bool
isFlat (Ref _) = return True
isFlat (Restriction {base}) = do
  report $ "Restriction of non-base type: " <> show base
  return False
isFlat (Complex { inner=Seq (allFlatElts -> True) }) =
  return True
-- TODO: accept seq extensions of
isFlat (Complex { attrs=[], inner=Elt _}) =
  return True
isFlat (Complex {inner=Choice (allFlatElts -> True)}) =
  return True
isFlat (Complex { attrs=[], inner=Choice (allFlatElts -> True) }) =
  return True
isFlat (Complex { attrs, inner=Choice []}) =
  return True
isFlat (Extension { base
                  , mixin=Complex {inner=Seq (allFlatElts -> True)
                                  }
                  }) =
  return False
isFlat (Extension { base
                  , mixin=Complex {inner=Choice cs
                                  }
                  }) = do
  return False
isFlat e@(Extension {}) = do
  report $ "Extension not  yet handled: " <> show e
  return False
isFlat (Restriction {}) = do
  return True
isFlat  other =
  error $ "isFlat? " <> show other

scopeId  ScopeGlobal      = "Global"
scopeId (ScopeType    ty) = ty <> "T"
scopeId (ScopeElement e ) = e  <> "E"

-- | TODO: check that the name is unique
nestedGroupName :: XMLString -> Flattener XMLString
nestedGroupName hint = do
  scope <- ask
  return $ scopeId scope <> hint

tyFlatten, goFlatten :: Type -> Flattener Type
tyFlatten ty = do
  terminate <- isFlat ty
  if terminate
    then return       ty
    else goFlatten    ty

goFlatten ty@Complex { mixed
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
goFlatten e@(Extension { base
                       , mixin=c@Complex {mixed, inner}}) = do
  mixed <- findIsMixed base
  case inner of
    Seq s | allFlatElts s -> do
      return e
    Seq s -> do
      s' <- splitTyPart mixed `mapM` s
      return e { mixin = c { inner = Seq s' }
               }
    Choice cs -> do
      newGroup <- splitTyPart mixed inner
      return e { mixin = c { inner = newGroup } }
goFlatten e@(Extension { mixin = other
                       }) = do
  report $ "Do not know how to flatten extension of "
        <> show other
  return e
goFlatten other = do
  report $ "Do not know how to flatten "
        <> show other
  return other

-- | Check if given type is mixed type.
findIsMixed typeName | isXSDBaseType typeName =
  return True -- TODO: check with the spec
findIsMixed typeName = do
  ty <- gets $ Map.lookup typeName
  case ty of
    Nothing -> do
      report $ "Cannot find base type "
            <> show typeName
      return False -- TODO: check spec?
    Just  Complex     {mixed} -> return mixed
    Just  Extension   {base}  ->
      findIsMixed base
    Just  Restriction {base}  ->
      findIsMixed base
    Just (Ref          name ) ->
      findIsMixed name

-- | Given `TyPart`, check if it is flat enough to process,
--   and split it into newly named `Group` if not.
splitTyPart :: Bool -> TyPart -> Flattener TyPart
splitTyPart mixed tyPart = do
    flat <- tyFlatten Complex { mixed
                              , attrs = []
                              , inner = tyPart
                              }
    name <- nestedGroupName $ nestedGroupNameHint tyPart
    modify' $ Map.insert name flat
    return  $ Group name

nestedGroupNameHint (Choice cs) = "Alt"
nestedGroupNameHint (Seq    cs) = "Rec"
nestedGroupNameHint  _          = "Inner"

flatten :: Schema -> (Schema, [Message])
flatten schema = runFlattener schema tyFlatten
