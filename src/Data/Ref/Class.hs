{-# LANGUAGE
    DefaultSignatures
  , FlexibleInstances
  , MultiParamTypeClasses #-}
module Data.Ref.Class
       ( Ref (..)
       ) where

import Control.Monad.ST.Safe
import Control.Monad.Trans.Class

import Data.IORef
import Data.STRef

class Monad m => Ref ref a m where
  newRef :: a -> m (ref a)
  readRef :: ref a -> m a
  writeRef :: ref a -> a -> m ()
  modifyRef :: ref a -> (a -> a) -> m ()
  modifyRef' :: ref a -> (a -> a) -> m ()
  
  default newRef :: (MonadTrans t, Ref ref a m) => a -> t m (ref a)
  newRef = lift . newRef
  
  default readRef :: (MonadTrans t, Ref ref a m) => ref a -> t m a
  readRef = lift . readRef
  
  default writeRef :: (MonadTrans t, Ref ref a m) => ref a -> a -> t m ()
  writeRef ref = lift . writeRef ref
  
  modifyRef ref f = readRef ref >>= writeRef ref . f
  
  modifyRef' ref f = readRef ref >>= \ a -> writeRef ref $! f a

instance Ref IORef a IO where
  newRef = newIORef
  readRef = readIORef
  writeRef = writeIORef
  modifyRef = modifyIORef
  modifyRef' = modifyIORef'

instance Ref (STRef s) a (ST s) where
  newRef = newSTRef
  readRef = readSTRef
  writeRef = writeSTRef
  modifyRef = modifySTRef
  modifyRef' = modifySTRef'
