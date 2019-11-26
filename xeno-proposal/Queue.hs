{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Queue
    ( Queue
    , newEmptyQueue
    , writeQueue
    , readQueue

    -- transactional stuff
    , queueBegin
    , queueCommit
    , queueRollback
    ) where

import Control.Concurrent.MVar

import Control.Concurrent (forkIO) -- for tests
import Control.Monad


-- this can be implemented more efficiently
-- TODO - GADTify, get rid of impossible cases in matchers
data Src a
    = Raw
    | Collect [a] (Src a)
    | Drain [a] (Src a)
    deriving (Eq, Show)




data Queue a
    = Queue
    { stash :: MVar (Src a)
    , body :: MVar a
    }

-- | Creates new queue. D'oh!
newEmptyQueue :: IO (Queue a)
newEmptyQueue = Queue <$> newMVar Raw <*> newEmptyMVar

-- | Write stuff to queue. FIFO
writeQueue :: Queue a -> a -> IO ()
writeQueue Queue{body} = putMVar body


-- | Helper function, viva la recursion
srcRead :: MVar a -> Src a -> IO (Src a, a)
srcRead var = \case
        Raw -> (,) Raw <$> takeMVar var
        Drain [] src -> do
            (src', x) <- srcRead var src
            return (Drain [] src', x)
        Drain (x:xs) src -> return (Drain xs src, x)
        Collect xs src -> do
            (src', x) <- srcRead var src
            return (Collect (x:xs) src', x)

-- | Read from the queue handling with transactional shenanigans
readQueue :: Show a => Queue a -> IO a
readQueue Queue{..} = modifyMVar stash (srcRead body)


-- | start transaction that can be either comitted or rolled back
queueBegin :: Queue a -> IO ()
queueBegin Queue{..} = do
    modifyMVar_ stash (return . Collect [])

-- | transaction was successfull, pretend that queueBegin never existed
queueCommit :: Queue a -> IO ()
queueCommit Queue{..} = do
    modifyMVar_ stash $ \case
        Raw -> error $ "commit on raw?"
        x@Drain{} -> return x
        Collect _ src -> return src

-- | transaction failed, pretend that queueBegin and all the reads in between never existed
queueRollback :: Queue a -> IO ()
queueRollback Queue{..} = do
    modifyMVar_ stash $ \case
        Raw -> error "rollback on raw?"
        Collect xs src -> return $ Drain (reverse xs) src
        Drain _ (Collect xs src) -> return $ Drain (reverse xs) src
        Drain _ Raw -> return Raw
        Drain _ (Drain _ _) -> error "nested drain? O_o"








-- TODO - factor out to external file, prettify


ds :: Show a => [Char] -> (Queue a -> IO ()) -> Queue a -> IO ()
ds msg act q = do
    putStrLn $ "BEF: " ++ msg
    readMVar (stash q) >>= print
    act q
    putStrLn $ "AFT : " ++ msg
    readMVar (stash q) >>= print
    putStrLn ""


_unitTests :: IO ()
_unitTests = do

        simpleRead
        transaction1
        transaction2
        transaction3
        transaction4
        transaction5
        putStrLn "OK"
    where
        setup :: Int -> IO (Queue Int)
        setup l = do
            q <- newEmptyQueue
            void . forkIO $ mapM_ (writeQueue q) [1..l]
            return q

        rd :: Queue Int -> Int -> IO ()
        rd q e = do
            st <- readMVar ( stash q)
            a <- readQueue q
            unless (e == a) $
                error $ unwords [ "Expected", show e ++ ", but got"
                                , show a, "after reading from", show st
                                ]

        simpleRead = do
            q <- setup 1
            rd q 1

        transaction1 = do
            q <- setup 2
            queueBegin q
            rd q 1
            rd q 2
            queueRollback q
            rd q 1
            rd q 2
            return ()

        transaction2 = do
            q <- setup 2
            queueBegin q
            queueBegin q
            rd q 1
            queueRollback q
            queueCommit q
            rd q 1
            rd q 2

        transaction3 = do
            q <- setup 4
            ds "BEGIN 1" queueBegin q
            rd q 1
            rd q 2
            ds "BEGIN 2" queueBegin q
            rd q 3
            rd q 4
            ds "ROLLBACK 1" queueRollback q
            rd q 3
            rd q 4
            ds "ROLLBACK 2" queueRollback q
            rd q 1
            rd q 2
            rd q 3
            rd q 4

        transaction4 = do
            q <- setup 4
            queueBegin q
            queueBegin q
            rd q 1
            rd q 2
            queueCommit q
            rd q 3
            rd q 4
            queueRollback q
            rd q 1
            rd q 2
            rd q 3
            rd q 4


        transaction5 = do
            q <- setup 4
            queueBegin q
            rd q 1
            rd q 2
            queueRollback q
            queueBegin q
            rd q 1
            rd q 2
            queueCommit q
            rd q 3
            rd q 4





