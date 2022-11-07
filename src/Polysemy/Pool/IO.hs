{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
module Polysemy.Pool.IO (runPoolIO, runPoolFinalIO) where

import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Monad
import Data.Foldable (traverse_)
import Polysemy
import Polysemy.Internal.CustomErrors
import Polysemy.Resource
import Polysemy.Pool.Effect

runPoolIO :: Members '[Embed IO] r
          => Sem r t
          -> (t -> Sem r ())
          -> Word
          -> Sem (Pool t ': r) a
          -> Sem r a
runPoolIO create destroy num eff = do
  pool <- replicateM (fromIntegral num) create
  queueVar <- embed $ do
    queue <- atomically $ do
      q <- newTQueue
      traverse_ (writeTQueue q) pool
      pure q
    newMVar queue
  r <- finterpret eff $ \action ->
    resourceToIO $ case action of
      Acquire -> bracket (embed $ takeMVar queueVar)
                         (embed . putMVar queueVar)
                         (embed . atomically . readTQueue)
      AcquireMaybe -> bracket (embed $ takeMVar queueVar)
                              (embed . putMVar queueVar)
                              (embed . atomically . tryReadTQueue)
      Release r -> bracket (embed $ takeMVar queueVar)
                           (embed . putMVar queueVar)
                           (embed . atomically . flip writeTQueue r)
  traverse_ destroy pool
  pure r

runPoolFinalIO :: (Members '[Final IO] r, Eq t)
               => Sem r t
               -> (t -> Sem r ())
               -> Word
               -> Sem (Pool t ': r) a
               -> Sem r a
runPoolFinalIO create destroy num eff = do
  pool <- replicateM (fromIntegral num) create
  queueVar <- embedFinal $ do
    queue <- atomically $ do
      q <- newTQueue
      traverse_ (writeTQueue q) pool
      pure q
    newMVar queue
  r <- finterpret eff $ \action ->
    resourceToIOFinal $ case action of
      Acquire -> bracket (embedFinal $ takeMVar queueVar)
                         (embedFinal . putMVar queueVar)
                         (embedFinal . atomically . readTQueue)
      AcquireMaybe -> bracket (embedFinal $ takeMVar queueVar)
                              (embedFinal . putMVar queueVar)
                              (embedFinal . atomically . tryReadTQueue)
      Release r -> bracket (embedFinal $ takeMVar queueVar)
                           (embedFinal . putMVar queueVar)
                           (embedFinal . atomically . flip writeTQueue r)
  traverse_ destroy pool
  pure r

-- | Flipped 'interpret'
finterpret
    :: FirstOrder e "interpret"
    => Sem (e ': r) a
    -> (forall rInitial x. e (Sem rInitial) x -> Sem r x)
    -> Sem r a
finterpret eff nt = interpret nt eff
