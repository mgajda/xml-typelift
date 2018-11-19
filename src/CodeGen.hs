{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MonoLocalBinds    #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}
-- | Here we aim to analyze the schema.
module CodeGen(codegen) where

import           Prelude hiding(lookup)

import           Control.Lens as Lens
import qualified Control.Monad.State   as St
import qualified Data.ByteString.Char8 as BS
import           Data.Generics.Uniplate.Operations
import qualified Data.Map.Strict       as Map
import           Data.Maybe(catMaybes)
import qualified Data.Set              as Set

import           Xeno.Types(XenoException(..))

import           FromXML(getStartIndex, stripNS)
import           Identifiers
import           Schema

-- | State of code generator
data CGState =
  CGState {
    -- Translation of XML Schema identifiers to Haskell identifiers
    _translations :: Map.Map XMLString XMLString
  , _schema       :: Schema
  }
makeLenses ''CGState

type CG a = St.State CGState a

bshow = BS.pack . show

proposeTranslations     :: XMLString -> [XMLString]
proposeTranslations name = [normName <> bshow i | i <- [1..]]
  where
    normName | name==""  = "XMLElem"
             | otherwise = normalizeTypeName name

translate :: XMLString -> CG XMLString
translate xmlName = do
  tr <- Lens.use translations
  case Map.lookup xmlName tr of
    Just r  -> return r
    Nothing ->
      let proposals = proposeTranslations xmlName
      in do
        case filter (`Map.notMember` tr) proposals of
          (goodProposal:_) -> do
            _ <- translations %= Map.insert xmlName goodProposal
            return goodProposal

codegen    :: Schema -> XMLString
codegen sch = ""

