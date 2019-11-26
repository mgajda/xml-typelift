{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}


-- {-# OPTIONS -fno-warn-unused-imports #-}
module SAX where

import GHC.Types
import Control.Applicative
import Control.Monad
import Control.Monad.Cont
import Control.Monad.Reader
import Data.Proxy
import Control.Monad.State
import Data.ByteString.Char8 (ByteString)
-- import GHC.OverloadedLabels
import GHC.TypeLits
import Data.Map.Strict (Map)
import qualified Data.ByteString.Char8 as C8
import qualified Data.Map.Strict as Map
import Prelude hiding (fail)

import ParserXP
import Exp



-- | wrappers for primitive events. If you are not careful they will
-- beep repeatedly and break everything
-- {{{
openingTag :: Parser l m ByteString
openingTag = [t | OpeningTagE t <- readEvent ]

attribute :: Parser l m (ByteString, ByteString)
attribute = [(k, v) | AttributeE k v <- readEvent]

openingTagEnd :: Parser l m ByteString
openingTagEnd = [t | OpeningTagEndE t <- readEvent ]

text :: Parser l m ByteString
text = [t | TextE t <- readEvent]

closingTag :: Parser l m ByteString
closingTag = [t | CloseTagE t <- readEvent]

cdata :: Parser l m ByteString
cdata = [t | CDataE t <- readEvent]
-- }}}

-- | should be called between OpeningTagE and OpeningTagEndE
xattrs :: Parser l m (Map ByteString ByteString)
xattrs = go [] where
    go xs = (try attribute >>= go . (: xs)) <|> return (Map.fromList xs)


type family Len xs where
   Len '[]       = 0
   Len (x ': xs) = 1 + Len xs

--  to Parser?
type KnownLocation xs =
    ( KnownNat (Eval (Length xs))
    , KnownSymbol (Eval (FoldList PathCons "" xs))
    )

locationDepth :: forall l m. KnownLocation l => Parser l m Int
locationDepth = return $ fromIntegral $ natVal (Proxy @ (Eval (Length l)))

locationName :: forall l m. KnownLocation l => Parser l m String
locationName = return $ symbolVal (Proxy @ (Eval (ToPath l)))


locDepth :: forall xs m. KnownNat (Len xs) => Parser xs m Int
locDepth = return . fromIntegral $ natVal (Proxy @ (Len xs))


-- | drop unused messages we still need to consume to go up a level
pillageOrphans :: KnownLocation l => Parser l m ()
pillageOrphans = do
    targetDepth <- locationDepth
    fix $ \self -> do
        d <- gets stackDepth
        when (d /= targetDepth) $ readEvent >> self

pillageOrphans' :: Int -> Parser l m ()
pillageOrphans' targetDepth = do
    fix $ \self -> do
        d <- gets stackDepth
        when (d /= targetDepth) $ readEvent >> self


-- | drop empty formatting message, if any
pillageFormatting :: Parser l m ()
pillageFormatting = (try text >>= guard . C8.all (flip elem (" \t\n\r" :: String))) <|> pure ()


-- | helper for xnode
nested :: Map ByteString ByteString -> Parser (s ': l) m a -> Parser l m a
nested attrs act = Parser (\s t ok bad -> unParser act s t { attrs = attrs } ok bad)


-- | ensure we have a node s, parse it's attributes and parse internals using provided parser
xnode :: forall s m l a. (KnownSymbol s) => Parser (s ': l) m a -> Parser l m a
xnode act = do
    let tag = C8.pack (symbolVal (Proxy @ s))

    -- look for opening tag first, fail if it's not, drop formatting
    try $ pillageFormatting >> openingTag >>= guard . (== tag)
    d <- gets stackDepth
    -- then we want attributes
    attrs <- xattrs
    -- check that end of opening tag event is present
    openingTagEnd >>= guard . (== tag)

    nested attrs act <* (pillageOrphans' (d - 1))


xnode' :: forall s m l a. (KnownSymbol s, KnownLocation l, MonadIO m, Show a) => Parser (s ': l) m a -> Parser l m a
xnode' act = xnode act <|> (liftIO (print $ "nope with " ++ symbolVal (Proxy @ s)) >> mzero)


string :: Parser s m String
string = C8.unpack <$> text

attr :: ByteString -> Parser s m (Maybe ByteString)
attr name = Map.lookup name <$> asks attrs

attr' :: ByteString -> Parser s m ByteString
attr' name = attr name >>= maybe mzero return

-------------------------------------------------------------------------------------------
