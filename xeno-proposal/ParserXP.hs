{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module ParserXP where

import Control.Applicative
import Control.Concurrent
import Control.Monad.Cont
import Control.Monad.Fail as Fail
import Control.Monad.Reader
import Control.Monad.State
import Data.ByteString.Char8 (ByteString)
import Data.Either
import Data.Map.Strict (Map)
import GHC.TypeLits
import GHC.Types
import Prelude hiding (fail)
import qualified Data.Map.Strict as Map
import Queue
import System.IO.Unsafe (unsafePerformIO)
import Xeno.SAX


data SaxEvent
    = OpeningTagE !ByteString
    | AttributeE !ByteString !ByteString
    | OpeningTagEndE !ByteString
    | TextE !ByteString
    | CloseTagE !ByteString
    | CDataE !ByteString
    deriving Show

runner :: Monad m => ByteString -> ContT () m SaxEvent
runner bs = ContT $ \k -> process (Process
    (k . OpeningTagE)
    (\ke va -> k (AttributeE ke va))
    (k . OpeningTagEndE)
    (k . TextE)
    (k . CloseTagE)
    (k . CDataE)) bs


data T = T { queue :: Queue SaxEvent, attrs :: Map ByteString ByteString }

type Tag = ByteString
data Stack = Stack { stackDepth :: !Int, stackTags :: [Tag] }
-- invariant length stackTags == stackDepth


newtype Parser (l :: [Symbol]) m a
    = Parser { unParser :: forall r. Stack -> T -> (a -> Stack -> m r) -> (String -> m r) -> m r }

-- {{{ MonadPlus, MonadTrans, MonadFail, MonadIO if m is MonadIO
instance Functor (Parser l m) where
    fmap fn p = Parser $ \s t ok bad -> unParser p s t (ok . fn) bad

instance Monad (Parser l m) where
    return a = Parser $ \s _t ok _bad -> ok a s
    f >>= g = Parser $ \s t ok bad ->
        let mok a s' = unParser (g a) s' t ok bad
        in unParser f s t mok bad

instance Applicative (Parser l m) where
    pure = return
    (<*>) = ap

instance MonadReader T (Parser l m) where
    ask = Parser $ \s t ok _bad -> ok t s
    local fn p = Parser $ \s t ok bad -> unParser p s (fn t) ok bad

instance MonadState Stack (Parser l m) where
    state fn = Parser $ \s _t ok _bad -> uncurry ok (fn s)

instance Alternative (Parser l m) where
    empty = Fail.fail "empty"
    f <|> g = Parser $ \s t ok bad -> unParser f s t ok (\_ -> unParser g s t ok bad)

instance MonadPlus (Parser l m) where
    mzero = Fail.fail "mzero"
    mplus = (<|>)

instance Fail.MonadFail (Parser l m) where
    fail msg = Parser $ \_s _t _ok bad -> bad msg

instance MonadTrans (Parser l) where
    lift f = Parser $ \s _t ok _bad -> f >>= flip ok s

instance MonadIO m => MonadIO (Parser l m) where
    liftIO f = Parser $ \s _t ok _bad -> liftIO f >>= flip ok s
-- }}}


-- all uses of unsafePerformIO are between those two lines
------------------------------------------------------------------------

-- begin + commit + rollback are not exposed to end user by default import, only via .Internal
begin :: Parser l m ()
begin = asks queue >>= \q -> return $! unsafePerformIO (queueBegin q)

commit :: Parser l m ()
commit = asks queue >>= \q -> return $! unsafePerformIO (queueCommit q)

rollback :: Parser l m ()
rollback = asks queue >>= \q -> return $! unsafePerformIO (queueRollback q)

readEvent :: Parser l m SaxEvent
readEvent = do
    evt <- asks queue >>= \q -> return $! unsafePerformIO (readQueue q)

    case evt of
        OpeningTagE tag -> push tag
        CloseTagE tag -> pop tag
        _ -> return ()
    return evt


runParser :: Monad m => ByteString -> Parser l m a -> m (Either String a)
runParser bs p = do
    let q = unsafePerformIO $ do
            queue <- newEmptyQueue
            void . forkIO $ runContT (runner bs >>= liftIO . writeQueue queue) return
            return queue
    unParser p (Stack 0 []) (T q Map.empty) (\a _st -> return (Right a)) (\err -> return (Left err))
-------------------------------------------------------------------------


-- see try in parsec, don't try too much - it use memory
-- also when in try - you don't have a monad transformer - you can't unlaunch missiles
try :: (forall s. Parser l s a) -> Parser l m a
try act = begin >> (act <* commit) <|> (rollback >> mzero)

-- | peek a, but don't consume anything
peek :: (forall s. Parser l s a) -> Parser l m a
peek act = get >>= \st -> begin *> act <* put st <* rollback


push :: Tag -> Parser l m ()
push t = modify $ \(Stack depth xs) -> Stack (depth + 1) (t:xs)

pop :: Tag -> Parser l m ()
pop t = do
    get >>= \case
        Stack depth (x:xs) -> do
            guard (t == x)
            put $ Stack (depth-1) xs
        Stack _ [] -> error "pop: empty stack?"

