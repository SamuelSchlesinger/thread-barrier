{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
module Control.Concurrent.Barrier
( Barrier
, newBarrier
, FullBarrier(FullBarrier)
, wait
) where

import Control.Monad
import Control.Exception
import Control.Concurrent
import Data.IORef

data Barrier a = Barrier
  { counter :: IORef Word
  , topCount :: Word
  , item :: a
  , mvar :: MVar a
  }

newBarrier :: a -> Word -> IO (Barrier a)
newBarrier item topCount = do
  counter <- newIORef 0
  mvar <- newEmptyMVar
  pure Barrier{..}

data FullBarrier = FullBarrier
  deriving Show

instance Exception FullBarrier

wait :: Barrier a -> IO a
wait Barrier{..} =
  join $ atomicModifyIORef' counter \x ->
    if x == topCount then (x, throwIO FullBarrier)
    else if x == topCount - 1 then (x + 1, do putMVar mvar item; pure item)
    else (x + 1, readMVar mvar)

test :: IO Bool
test = do
  and <$> forM [2..100] \(m :: Word) -> do
    b <- newBarrier m m
    forM [1..m - 1] \_ -> void . forkIO . void . wait $ b
    (&&)
      <$> ((== m) <$> wait b)
      <*> (try @SomeException (wait b) >>= either (const (pure True)) (const (pure False)))
