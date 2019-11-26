{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
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
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}


module Exp where

import GHC.Types
import GHC.TypeLits


type Exp a = a -> Type

type family Eval (e :: Exp a) :: a

data Length :: [a] -> Exp Nat

type instance Eval (Length '[]) = 0
type instance Eval (Length (x ': xs)) = 1 + Eval (Length xs)




data FoldList :: (b -> a -> Exp b) -> b -> [a] -> Exp b
type instance Eval (FoldList _fn acc '[]) = acc
type instance Eval (FoldList fn acc (x ': xs)) = Eval (FoldList fn (Eval (fn acc x)) xs)

data PathCons :: Symbol -> Symbol -> Exp Symbol
type instance Eval (PathCons a b) = AppendSymbol (AppendSymbol a "/") b

data ToPath :: [Symbol] -> Exp Symbol
type instance Eval (ToPath xs) = Eval (FoldList PathCons "" xs)
