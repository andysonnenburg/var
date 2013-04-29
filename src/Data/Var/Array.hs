{-# LANGUAGE
    FlexibleContexts
  , FlexibleInstances
  , MagicHash
  , MultiParamTypeClasses
  , Rank2Types
  , TypeFamilies
  , UnboxedTuples #-}
{- |
Copyright   :  (c) Andy Sonnenburg 2013
License     :  BSD3
Maintainer  :  andy22286@gmail.com
-}
module Data.Var.Array
       ( ArrayVar
       , newVars
       ) where

import Control.Monad (liftM)
import Control.Monad.Prim.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict

import Data.Var.Class
import Data.Var.Tuple

import GHC.Exts

import Unsafe.Coerce (unsafeCoerce)

data ArrayVar s a = ArrayVar {-# UNPACK #-} !(MutableArray s Any) {-# UNPACK #-} !Int

instance (MonadPrim m, s ~ World m) => Var (ArrayVar s) a m where
  newVar a = do
    array <- newArray 1 undefined
    writeArray array 0 (unsafeCoerce a)
    return $ ArrayVar array 0
  {-# INLINE newVar #-}
  readVar (ArrayVar array i) = liftM unsafeCoerce $ readArray array i
  {-# INLINE readVar #-}
  writeVar (ArrayVar array i) = writeArray array i . unsafeCoerce
  {-# INLINE writeVar #-}

newVars :: ( FixedSize as
           , Heterogeneous f as vars
           , MonadPrim m
           ) => (forall a . ArrayVar (World m) a -> f a) -> as -> m vars
newVars f tuple = do
  array <- newArray (sizeOf tuple) undefined
  flip evalStateT 0 $ forM tuple $ \ a -> do
    i <- get
    lift $ writeArray array i (unsafeCoerce a)
    put $! i + 1
    return $ f (ArrayVar array i)

data MutableArray s a = MutableArray (MutableArray# s a)

newArray :: MonadPrim m => Int -> a -> m (MutableArray (World m) a)
newArray (I# n) a = liftPrim $ \ s -> case newArray# n a s of
  (# s', array #) -> (# s', MutableArray array #)

readArray :: MonadPrim m => MutableArray (World m) a -> Int -> m a
readArray (MutableArray array) (I# i) = liftPrim $ readArray# array i

writeArray :: MonadPrim m => MutableArray (World m) a -> Int -> a -> m ()
writeArray (MutableArray array) (I# i) a = liftPrim $ \ s ->
  case writeArray# array i a s of
    s' -> (# s', () #)
