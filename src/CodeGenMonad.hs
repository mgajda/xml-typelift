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
-- | Monad for code generation:
--   Mostly deals with keeping track of all
--   generated code as "Builder",
--   keeping track of unique translation
--   for each XML identifier into Haskell
--   type or field name.
module CodeGenMonad(-- Code generation monad
                    CG
                   ,CGState
                   ,runCodeGen

                   -- Translating identifiers
                   ,translations
                   ,translateType
                   ,translateField

                   -- Utilities
                   ,builderUnlines
                   ,builderString
                   ,bshow
                   ) where

import           Prelude hiding(lookup)

import           Control.Lens as Lens
import qualified Control.Monad.RWS.Strict   as RWS
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy       as BSL(toStrict)
import qualified Data.ByteString.Builder    as B
import qualified Data.Map.Strict            as Map
import qualified Data.Set                   as Set

import           Identifiers
import           Schema
import           BaseTypes

-- | State of code generator
data CGState =
  CGState {
    -- Translation of XML Schema identifiers to Haskell identifiers
    _translations :: Map.Map XMLString XMLString
  }
makeLenses ''CGState

type CG a = RWS.RWS Schema B.Builder CGState a

initialState :: CGState
initialState  = CGState
              $ Map.fromList [(bt, fromBaseXMLType bt)
                             | bt <- Set.toList predefinedTypes ]

bshow :: Show a => a -> BS.ByteString
bshow = BS.pack . show

builderUnlines :: [B.Builder] -> B.Builder
builderUnlines []     = ""
builderUnlines (l:ls) = l <> mconcat (("\n" <>) <$> ls)

-- | Translate type name from XML identifier.
translateType :: XMLString -> XMLString -> CG B.Builder
translateType  = translate' "UnnamedElementType" normalizeTypeName

-- | Translate field name from XML identifier.
translateField :: XMLString -> XMLString -> CG B.Builder
translateField = translate' "unnamedFieldName"   normalizeFieldName

-- | Translate XML Schema identifier into Haskell identifier,
--   maintaining dictionary to assure uniqueness of Haskell identifier.
translate' ::  XMLString               -- placeholder for empty inputs
           -> (XMLString -> XMLString) -- normalizer
           ->  XMLString               -- input container name
           ->  XMLString               -- input name
           -> CG B.Builder
translate' placeholder normalizer container xmlName = do
    tr <- Lens.use translations
    case Map.lookup xmlName tr of
      Just r  -> return $ B.byteString r
      Nothing ->
        let proposals = proposeTranslations xmlName
        in do
          case filter (`Map.notMember` tr) proposals of
            (goodProposal:_) -> do
              _ <- translations %= Map.insert xmlName goodProposal
              return $ B.byteString goodProposal
  where
    proposeTranslations     :: XMLString -> [XMLString]
    proposeTranslations (normalizer -> name) =
        [BS.take i container <> normName | i :: Int <- [0..BS.length container]] <>
        [normName <> bshow i | i :: Int <- [1..]]
      where
        normName | name==""  = placeholder
                 | otherwise = name

-- | Make builder to generate schema code.
runCodeGen        :: Schema -> CG () -> B.Builder
runCodeGen sch rws = case RWS.runRWS rws sch initialState of
                       ((), _state, builder) -> builder

-- | Convert builder back to String, if you need to examine the content.
builderString :: B.Builder -> BS.ByteString
builderString  = BSL.toStrict . B.toLazyByteString
