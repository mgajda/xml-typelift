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
-- | Generating type declarations in code generation monad.
module TypeDecls(Field
                ,Record
                ,declareAlgebraicType
                ,formatRecord
                ,formatField
                ,wrapList
                ,wrapMaybe
                ) where

import           Prelude hiding(lookup)

import           Control.Lens as Lens
import           Control.Monad(forM, forM_)
import qualified Control.Monad.RWS.Strict   as RWS
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy       as BSL(length, toStrict)
import qualified Data.ByteString.Builder    as B
import           Data.Generics.Uniplate.Operations
import qualified Data.Map.Strict            as Map
import           Data.Maybe(catMaybes)
import qualified Data.Set                   as Set
import           Data.String

import           Xeno.Types(XenoException(..))

import           CodeGenMonad
import           Identifiers
import           Schema

formatField (fName, fTypeName) = fName <> " :: " <> fTypeName

wrapList  x = "["      <> x <> "]"
wrapMaybe x = "Maybe " <> x

-- | Translate type name from XML identifier.
translateType  = translate' "UnnamedElementType" normalizeTypeName

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
      Just r  -> --return $ "Translation for " <> B.byteString xmlName <> " is " <> B.byteString r <> " normalizer gave " <>
                 return $ B.byteString $ normalizer xmlName
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

-- * Type declarations
type Field = (B.Builder, B.Builder)
type Record = (B.Builder, [Field])

declareAlgebraicType :: [Record] -> CG ()
declareAlgebraicType []                       = error "Empty list of records"
declareAlgebraicType (firstEntry:nextEntries) = do
    RWS.tell   $ "    " <> formatRecord firstEntry <> "\n"
    forM_ nextEntries $ \nextEntry ->
      RWS.tell $ "  | " <> formatRecord nextEntry


formatRecord :: Record -> B.Builder
formatRecord (name, (f:fields)) =
    builderUnlines
      ( formatHeading f
      :(formatFollowing <$> fields))
    <> trailer
  where
    formatHeading   f = header   <> formatField f
    formatFollowing f = follower <> formatField f
    header, follower, leftPad, trailer :: B.Builder
    header   =         name    <> " { "
    follower =         leftPad <> " , "
    trailer  = "\n" <> leftPad <> " }"
    leftPad  = B.byteString
             $ BS.replicate (fromIntegral $ BSL.length $ B.toLazyByteString name) ' '

