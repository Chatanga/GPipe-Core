{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- | Introduces the SNMapReaderT monad, a MonadTrans instance with the capability
to memoize intermediate results.
-}
module Data.SNMap
    ( SNMapReaderT
    , runSNMapReaderT
    , memoizeM
    , scopedM
    ) where

import Prelude

-- base
import Control.Applicative
import Data.Functor
import System.Mem.StableName
import System.Mem.Weak

-- hashtables
import qualified Data.HashTable.IO as HT

-- transformers
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict

-- exception-transformers
import Control.Monad.Exception

-- ***

{- | A map (SN stands for stable name) to cache the results 'a' of computations 'm a'.
-}
newtype SNMap m a = SNMap (HT.BasicHashTable (StableName (m a)) a)

-- Create a SNMap and introduce a IO requirement to use a IO hastable internally.
newSNMap :: IO (SNMap m a)
newSNMap = SNMap <$> HT.new

memoize :: MonadIO m
    => m (SNMap m a) -- ^ A "IO call" to retrieve our cache.
    -> m a -- ^ The "IO call" to execute and cache the result.
    -> m a -- ^ The result being naturally also returned.
memoize getter m = do
    s <- liftIO $ makeStableName $! m -- Does forcing the evaluation make sense here (since we try to avoid it...)?
    SNMap h <- getter
    x <- liftIO $ HT.lookup h s
    case x of
        Just a -> return a
        Nothing -> do
            a <- m
            SNMap h' <- getter -- Reload the cache which could have been updated recursively when evaluating m.
            liftIO $ HT.insert h' s a
            return a

-- ***

-- An action with the capability to recursively memoize intermediate results along the way.
newtype SNMapReaderT a m b = SNMapReaderT (StateT (SNMap (SNMapReaderT a m) a) m b)
    deriving (Functor, Applicative, Monad, MonadIO, MonadException, MonadAsyncException)

runSNMapReaderT :: MonadIO m => SNMapReaderT a m b -> m b
runSNMapReaderT (SNMapReaderT m) = do
    h <- liftIO newSNMap
    evalStateT m h

instance MonadTrans (SNMapReaderT a) where
    lift = SNMapReaderT . lift

-- Easily memoize when inside a SNMapReaderT.
memoizeM :: MonadIO m => SNMapReaderT a m a -> SNMapReaderT a m a
memoizeM = memoize (SNMapReaderT get)

-- | Run a subcomputation in a scope, where nothing memoized inside will be remembered after.
scopedM :: MonadIO m => SNMapReaderT a m x -> SNMapReaderT a m x
scopedM m = do
    SNMap h <- SNMapReaderT get
    save <- liftIO $ HT.toList h
    x <- m
    h' <- liftIO $ HT.fromList save
    SNMapReaderT $ put (SNMap h')
    return x
