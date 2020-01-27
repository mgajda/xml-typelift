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
-- | Here we aim to analyze the schema.
module Flatten(flatten) where

import           Control.Monad.Reader
import           Control.Monad.RWS     (MonadRWS (..), MonadReader (..),
                                        MonadState (..), MonadWriter (..), RWS,
                                        evalRWS, gets, modify')
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as BS
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

class ComplexityLevel t where
  complexityLevel :: t -> Flattener Int

instance ComplexityLevel Type where
-- | Is it a single-level structure, or does it need splitting?
--   Types of level 0 are just references to other declarations
--   Types of level 1 need a single declaration
--   Types of higher level need splitting to eliminate nesting
  complexityLevel (Ref _) = return 0
  complexityLevel (Restriction {base}) = do
    when (not $ isXSDBaseType base) $
      report $ "Restriction of non-base type: " <> show base
    return 1
  complexityLevel (Complex { inner=Seq s }) =
    complexityLevel s
  -- TODO: accept seq extensions of
  complexityLevel (Complex { attrs=[], inner=Elt _ }) =
    return 1
  complexityLevel (Complex { attrs=[], inner=Choice cs }) =
    complexityLevel cs
  complexityLevel (Complex { attrs, inner=Choice []}) =
    return 1
  complexityLevel (Complex { attrs=(_a:_as), inner=Choice cs }) =
    complexityLevel cs
  complexityLevel (Extension {
                      base
                    , mixin=Complex { inner=Seq s
                                    }
                    }) =
    -- Extension on top of sequence adds only single level of complexity (like Seq does),
    -- since base type will be mentioned as reference.
    complexityLevel s
  complexityLevel (Extension { base
                    , mixin=Complex {inner=Choice cs
                                    }
                    }) =
    -- Extension on top of choice adds only single level of complexity,
    -- since we need to declare sum type anyway
    complexityLevel cs
  complexityLevel e@(Extension {mixin}) = do
    report $ "Extension not  yet handled: " <> show e
    (1+) <$> complexityLevel mixin
  complexityLevel (Restriction {}) = do
    return 1
  complexityLevel  other =
    error $ "complexityLevel? " <> show other

instance ComplexityLevel [TyPart] where
  complexityLevel s =
    ((+1) . maximum . (0:)) <$> mapM complexityLevel s

instance ComplexityLevel TyPart where
  complexityLevel (Elt    e ) =
    return 0
  complexityLevel (Group  g ) =
    return 0
  complexityLevel (Seq    s ) =
    complexityLevel s
  complexityLevel (Choice cs) =
    complexityLevel cs
  complexityLevel (All    s ) =
    complexityLevel s

scopeId :: FScope -> XMLString
scopeId  ScopeGlobal      = "Global"
scopeId (ScopeType    ty) = ty <> "T"
scopeId (ScopeElement e ) = e  <> "E"

nestedGroupName :: XMLString -> Flattener XMLString
nestedGroupName hint = do
  scope <- ask
  tryNames scope ("": map (BS.pack . show) [(1::Integer)..])
  where
    tryNames scope (n:ns) = do
      let name = scopeId scope <> n <> hint
      gets (Map.member name) >>= bool (return name) (tryNames scope ns)
    tryNames _ [] = undefined

tyFlatten, goFlatten :: Type -> Flattener Type
tyFlatten ty = do
  level <- complexityLevel ty
  case level of
    0 -> return    ty -- Should be `newtype` alias
    1 -> return    ty -- Will be a single new declaration
    _ -> goFlatten ty -- Needs to be splitted

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
                       , mixin=c@Complex {attrs, inner}}) = do
  mixed <- findIsMixed base
  case inner of
    Seq s -> do
      level <- complexityLevel s
      case level of
        0 -> return e
        _ -> do
          s' <- trySplitTyPart mixed `mapM` s
          return e { mixin = c { inner = Seq s' }
                   }
    Choice cs -> do
      baseType <- gets $ Map.lookup base
      -- base type should be choice, or flat
      {- TODO: gather bases, and merge
        case base of
        Complex {attrs = baseAttrs,
                ,mixed
                ,inner = baseInner} ->
        other ->
       -}
      newGroup <- splitTyPart mixed inner
      return e { mixin = c { inner = newGroup } }
    x -> error $ "Unhandled " <> show x
goFlatten e@(Extension { mixin = other
                       }) = do
  report $ "Do not know how to flatten extension of "
        <> show other
  return e
goFlatten other = do
  report $ "Do not know how to flatten "
        <> show other
  return other

-- | Split `TyPart` if it has complexity >0
trySplitTyPart mixed ty = do
  level <- complexityLevel ty
  case level of
    0 -> return ty
    _ -> splitTyPart mixed ty

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
