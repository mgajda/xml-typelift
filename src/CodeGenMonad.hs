{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonoLocalBinds             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE ViewPatterns               #-}
-- | Monad for code generation:
--   Mostly deals with keeping track of all
--   generated code as "Builder",
--   keeping track of unique translation
--   for each XML identifier into Haskell
--   type or field name.
module CodeGenMonad(-- Code generation monad
                    CG
                   ,runCodeGen
                   ,gen
                   ,warn

                   -- Translating identifiers
                   ,TargetIdNS(..)
                   ,freshInnerId
                   ,XMLIdNS   (..)
                   ,translate
                   ,isTypeDefinedYet

                   -- Scopes
                   , Scope(..)
                   , inScope
                   , inEnum
                   , innerScope
                   ) where

import           Prelude hiding(lookup)

import           Control.Lens as Lens
import qualified Control.Monad.RWS.Strict   as RWS
import           Data.Monoid
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy       as BSL(toStrict, length)
import qualified Data.ByteString.Builder    as B
import qualified Data.Map.Strict            as Map
import qualified Data.Set                   as Set

import           FromXML(XMLString)
import           Data.String
import           Identifiers
import           Schema
import           BaseTypes
import           Code

-- | To enable tracing import Debug.Trace(trace) instead:
--import Debug.Trace(trace)
trace _ x = x

-- * Translation name spaces
-- | Which of the XML Schema identifier namespaces do we use here:
data XMLIdNS = SchemaType
             | ElementName
             | AttributeName
             | Inner  Int       -- ^ Anonymous inner content inside element
             | EnumIn XMLString -- ^ Enumeration entry inside type/element of given name (should be path)
                                --   Since different enumeration can have the same entry,
                                --   we treat them as separate namespaces.
  deriving (Eq, Ord, Show)

-- | Which of the target language identifier namespaces do we use here
data TargetIdNS = TargetTypeName
                | TargetConsName
                | TargetFieldName
  deriving (Eq, Ord, Show, Enum, Bounded)

type IdClass = (XMLIdNS, TargetIdNS)

-- | State of code generator
data CGState =
  CGState {
    -- | Translation of XML Schema identifiers to Haskell identifiers.
    _translations         :: Map.Map (IdClass,    XMLString) XMLString
    -- | Set of translation target names that were used before (and are thus unavailable.)
  , _allocatedIdentifiers :: Set.Set (TargetIdNS, XMLString)
    -- | Manage supply of fresh identifiers
  , _freshId              :: Int
  }
makeLenses ''CGState

-- * Scopes and naming
-- | All information necessary to assign a new name.
data Scope = Scope {
    containerId
  , contextId   :: XMLString
  , schemaType  :: XMLIdNS
  } deriving (Show, Eq)

-- | Descend one level down with the context.
inScope :: XMLIdNS -> XMLString -> CG a -> CG a
inScope schemaType contextId = do
    CG . RWS.local descend . unCG
  where
    descend Scope { contextId = containerId } =
            Scope { contextId,  containerId, schemaType }

-- | Generate fresh inner context.
innerScope        :: XMLString -> CG a -> CG a
innerScope stem action = do
  innerId  <- freshInnerId stem
  inScope (Inner innerId) stem action

-- | Code generation monad
newtype CG a = CG { unCG :: RWS.RWS Scope B.Builder CGState a }
  deriving (Functor, Applicative, Monad)

instance RWS.MonadState CGState CG where
  get   = CG   RWS.get
  put   = CG . RWS.put
  state = CG . RWS.state

instance RWS.MonadWriter B.Builder CG where
  tell   = CG . RWS.tell
  listen = CG . RWS.listen . unCG
  pass   = CG . RWS.pass   . unCG

-- | Fresh context with a given name stem.
--   Used for anonymous elements.
freshInnerId :: XMLString -> CG Int
freshInnerId stem = do
    result  <- Lens.use freshId
    freshId %= (+1)
    return     result

-- | Scope with enum constructor id.
inEnum :: XMLString -> CG a -> CG a
inEnum consName action = do
  ctxName <- CG $ RWS.asks contextId
  inScope (EnumIn ctxName) consName action

-- | Toplevel scope from which we start.
initialScope :: Scope
initialScope  = Scope "Top" "TopLevel" (Inner (-1))

-- | Initial state of translation dictionary.
initialState :: CGState
initialState  = CGState
               (Map.fromList [(((SchemaType, TargetTypeName), schemaType), haskellType)
                             | (schemaType, haskellType) <- baseTranslations ])
               (Set.fromList $ map trans baseTranslations)
                1
  where
    trans = (TargetTypeName,) . snd

gen     :: [Code] -> CG ()
gen args = RWS.tell $ mconcat $ map unCode args

warn     :: [String] -> CG ()
warn args = gen ["{- WARNING ", toCode $ mconcat args, " -}\n"]

-- | PlaceHolder depending of class of the source and target ids
--   Names selected to provide maximum clarity.
placeholder :: XMLIdNS -> TargetIdNS -> XMLString
placeholder xmlIdClass targetIdClass = classNormalizer targetIdClass $ name xmlIdClass
  where
    name  SchemaType    = "UnnamedSchemaType"
    name  ElementName   = "UnnamedElement"    -- is not allowed by schema
    name  AttributeName = "UnnamedAttribute"
    name (Inner  i)     = "Inner"  <> bshow i
    name (EnumIn x)     = "EnumIn" <> x

-- | Normalize name to fit in a given target namespace
classNormalizer                :: TargetIdNS -> XMLString -> XMLString
classNormalizer TargetTypeName  = normalizeTypeName
classNormalizer TargetConsName  = normalizeTypeName -- same normalization as type names, but separate space
classNormalizer TargetFieldName = normalizeFieldName

-- | Translate XML Schema identifier into Haskell identifier,
--   maintaining dictionary to assure uniqueness of Haskell identifier.
translate :: TargetIdNS
          -> CG TargetId
translate targetIdClass = do
  Scope {..} <- CG RWS.ask
  let normalizer            = classNormalizer targetIdClass
      name                  = normalizer  contextId
      normName | name==""   = placeholder schemaType targetIdClass
               | otherwise  = name
      proposedTranslations :: [(TargetIdNS, XMLString)]
      proposedTranslations  = ((targetIdClass,) . normalizer) <$>
          ([BS.take i containerId <> normName | i :: Int <- [0..BS.length containerId]] <>
           [normName <> bshow i | i :: Int <- [1..]])
  tr     <- Lens.use translations
  allocs <- Lens.use allocatedIdentifiers
  case Map.lookup ((schemaType, targetIdClass), contextId) tr of
    Just r  -> return $ TargetId r
    Nothing -> do
      let isValid (_, x) | rejectInvalidTypeName x = False
          isValid     x  | x `Set.member` allocs   = False
          isValid  _                               = True
          proposals = isValid `filter` proposedTranslations
      case proposals of
        (goodProposal:_) ->
         trace ("translate " <> show schemaType  <>
                " "          <> show containerId <>
                " "          <> show contextId   <>
                " -> "       <> show goodProposal) $ do
          translations         %= Map.insert ((schemaType, targetIdClass), contextId) (snd goodProposal)
          allocatedIdentifiers %= Set.insert                                               goodProposal
          return $ TargetId $ snd goodProposal
        [] -> error "Impossible happened when trying to find a new identifier - file a bug!"

-- | Check if given type has been already defined
isTypeDefinedYet :: XMLString -> CG Bool
isTypeDefinedYet xmlName = do
    tr    <- Lens.use translations
    return $ isJust $ Map.lookup ((SchemaType, TargetTypeName), xmlName) tr
  where
    isJust (Just _) = True
    isJust Nothing  = False

-- | Make builder to generate schema code.
runCodeGen         :: CG () -> B.Builder
runCodeGen (CG rws) = case RWS.runRWS rws initialScope initialState of
                            ((), _state, builder) -> builder

