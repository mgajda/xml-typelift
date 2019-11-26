{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonoLocalBinds             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
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
                   ,CGOutput
                   ,CGOutputEntity(..)
                   ,runCodeGen
                   ,out
                   ,outCodeLine
                   ,warn

                   -- Translating identifiers
                   ,TargetIdNS(..)
                   ,XMLIdNS   (..)
                   ,translate

                   -- Utilities
                   ,builderUnlines
                   ,builderString
                   ,builderLength
                   ,bshow
                   ,bToS
                   ) where

import           Prelude hiding(lookup)

import           Control.Lens as Lens
-- import           Text.InterpolatedString.Perl6 (qc)
import qualified Control.Monad.RWS.Strict   as RWS
import qualified Data.ByteString.Builder    as B
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy       as BSL(toStrict, length)
import qualified Data.Map.Strict            as Map
import qualified Data.Set                   as Set
import qualified Language.Haskell.TH        as TH

import           BaseTypes
import           FromXML(XMLString)
import           Identifiers
import           Schema


-- | To enable tracing import Debug.Trace(trace) instead:
--import Debug.Trace(trace)
trace :: String -> a -> a
trace _ x = x

-- | Which of the XML Schema identifier namespaces do we use here
data XMLIdNS = SchemaType
             | ElementName
             | AttributeName
             | EnumIn XMLString -- enumeration inside type/element of given name (should be path)
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
  }
makeLenses ''CGState


data CGOutputEntity = CGDec (TH.Q TH.Dec)
                    | CGCodeLine String

type CGOutput = [CGOutputEntity]


newtype CG a = CG { unCG :: (RWS.RWS Schema CGOutput CGState a) }
  deriving (Functor, Applicative, Monad) -- , RWS.MonadReader, RWS.MonadWriter, RWS.MonadIO)

instance RWS.MonadState CGState CG where
  get       = CG   RWS.get
  put   x   = CG $ RWS.put x
  state mdf = CG $ RWS.state mdf

instance RWS.MonadWriter CGOutput CG where
  tell   = CG . RWS.tell
  listen = CG . RWS.listen . unCG
  pass   = CG . RWS.pass   . unCG

initialState :: CGState
initialState  = CGState
               (Map.fromList [(((SchemaType, TargetTypeName), schemaType), haskellType)
                             | (schemaType, haskellType) <- baseTranslations ])
               (Set.fromList $ map trans baseTranslations)
  where
    trans = (TargetTypeName,) . snd

out :: (TH.Q TH.Dec) -> CG ()
out dec = RWS.tell [CGDec dec]

outCodeLine :: String -> CG ()
outCodeLine cmnt = RWS.tell [CGCodeLine cmnt]

warn :: String -> CG ()
warn wrn = outCodeLine $ concat ["{- WARNING ", wrn, " -}"]

-- TODO: add keywords to prevent mapping of these

bshow :: Show a => a -> BS.ByteString
bshow = BS.pack . show

builderUnlines :: [B.Builder] -> B.Builder
builderUnlines []     = ""
builderUnlines (l:ls) = l <> mconcat (("\n" <>) <$> ls)

-- | PlaceHolder depending of class of the source and target ids
--   Names selected to provide maximum clarity.
placeholder :: XMLIdNS -> TargetIdNS -> XMLString
placeholder xmlIdClass targetIdClass = classNormalizer targetIdClass $ name xmlIdClass
  where
    name  SchemaType    = "UnnamedSchemaType"
    name  ElementName   = "UnnamedElement"    -- is not allowed by schema
    name  AttributeName = "UnnamedAttribute"
    name (EnumIn x)     = "EnumIn" <> x

classNormalizer :: TargetIdNS -> XMLString -> XMLString
classNormalizer TargetTypeName  = normalizeTypeName
classNormalizer TargetConsName  = normalizeTypeName -- same normalization as type names, but separate space
classNormalizer TargetFieldName = normalizeFieldName

-- | Translate XML Schema identifier into Haskell identifier,
--   maintaining dictionary to assure uniqueness of Haskell identifier.
translate :: IdClass
          -> XMLString               -- input container name
          -> XMLString               -- input name
          -> CG B.Builder
translate idClass@(schemaIdClass, haskellIdClass) container xmlName = do
    tr     <- Lens.use translations
    allocs <- Lens.use allocatedIdentifiers
    case Map.lookup (idClass, xmlName) tr of
      Just r  -> return $ B.byteString r
      Nothing -> do
        let isValid (_, x) | rejectInvalidTypeName x = False
            isValid     x  | x `Set.member` allocs   = False
            isValid  _                               = True
            proposals = isValid `filter` proposeTranslations xmlName
        case proposals of
          (goodProposal:_) ->
           trace ("translate " <> show idClass <> " " <> show container <>
                  " "          <> show xmlName <> " -> " <> show goodProposal) $ do
            _ <- translations         %= Map.insert (idClass, xmlName) (snd goodProposal)
            _ <- allocatedIdentifiers %= Set.insert                         goodProposal
            return $ B.byteString $ snd goodProposal
          [] -> error "Impossible happened when trying to find a new identifier - file a bug!"
  where
    normalizer = classNormalizer haskellIdClass
    proposeTranslations                     :: XMLString -> [(TargetIdNS, XMLString)]
    proposeTranslations (normalizer -> name) = ((haskellIdClass,) . normalizer) <$>
        ([BS.take i container <> normName | i :: Int <- [0..BS.length container]] <>
         [normName <> bshow i | i :: Int <- [1..]])
      where
        normName | name==""  = placeholder schemaIdClass haskellIdClass
                 | otherwise = name

-- | Make builder to generate schema code.
runCodeGen :: Schema -> CG () -> CGOutput
runCodeGen sch (CG rws) = case RWS.runRWS rws sch initialState of
                            ((), _state, output) -> output

-- | Convert builder back to String, if you need to examine the content.
builderString :: B.Builder -> BS.ByteString
builderString  = BSL.toStrict . B.toLazyByteString

builderLength :: B.Builder -> Int
builderLength  = fromIntegral . BSL.length . B.toLazyByteString


bToS :: B.Builder -> String
bToS = BS.unpack . BSL.toStrict . B.toLazyByteString


