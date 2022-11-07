{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
module Polysemy.Pool.Effect ( Pool(..)
                            , acquire
                            , acquireMaybe
                            , release
                            , runPurePool
                            , withResource
                            , withResourceIO
                            , withResourceFinalIO
                            ) where

import Polysemy
import Polysemy.Error
import Polysemy.Internal.Combinators
import Polysemy.Resource

data Pool r m a where
  Acquire :: Pool r m r
  AcquireMaybe :: Pool r m (Maybe r)
  Release :: r -> Pool r m ()

makeSem ''Pool

data PoolExhausted = PoolExhausted

runPurePool :: Member (Error PoolExhausted) r => [t] -> Sem (Pool t ': r) a -> Sem r a
runPurePool = stateful_ $ \case
  Acquire -> \ts -> if null ts then throw PoolExhausted else pure (tail ts, head ts)
  AcquireMaybe -> \ts -> pure $ if null ts then (ts, Nothing) else (tail ts, Just $ head ts)
  Release t -> \ts -> pure (t:ts, ())

stateful_ :: (forall x m. e m x -> s -> Sem r (s, x))
          -> s
          -> Sem (e ': r) a
          -> Sem r a
stateful_ f s e = do (_, x) <- stateful f s e
                     pure x

withResource :: Member (Pool t) r => (t -> Sem (Resource : r) a) -> Sem r a
withResource = runResource . bracket acquire release

withResourceIO :: Members [Pool t, Embed IO] r => (t -> Sem (Resource : r) a) -> Sem r a
withResourceIO = resourceToIO . bracket acquire release

withResourceFinalIO :: Members [Pool t, Final IO] r => (t -> Sem (Resource : r) a) -> Sem r a
withResourceFinalIO = resourceToIOFinal . bracket acquire release
