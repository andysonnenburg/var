{-# LANGUAGE
    CPP
  , FlexibleContexts
  , FlexibleInstances
  , MagicHash
  , MultiParamTypeClasses
  , Rank2Types #-}
#ifdef LANGUAGE_Trustworthy
{-# LANGUAGE Trustworthy #-}
#endif
{-# LANGUAGE TypeFamilies, UnboxedTuples #-}
{- |
Copyright   :  (c) Andy Sonnenburg 2013
License     :  BSD3
Maintainer  :  andy22286@gmail.com
-}
module Data.Var.Array
       ( ArrayVar
       , newVars
       , Kleisli (..)
       , Empty (..)
       , Wrap (..)
       , Traversable (..)
       ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Prim.Class

import Data.Var.ByteArray
import Data.Var.Class
import Data.Var.Traversable

import GHC.Exts

import Prelude hiding (mapM)

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

newVars :: ( Traversable Empty as vars
           , Traversable (Wrap w) as vars
           , Applicative m
           , MonadPrim m
           , Var (ByteArrayVar (World m)) Int m
           ) => (forall a . ArrayVar (World m) a -> w a) -> as -> m vars
newVars f as = do
  array <- newArray (size as) undefined
  i <- newByteArrayVar 0
  for as $ Wrap $ \ a -> do
    i' <- readVar i
    writeArray array i' (unsafeCoerce a)
    modifyVar' i (+ 1)
    return $! f (ArrayVar array i')
{-# INLINE newVars #-}

data MutableArray s a = MutableArray (MutableArray# s a)

newArray :: MonadPrim m => Int -> a -> m (MutableArray (World m) a)
newArray (I# n) a = liftPrim $ \ s -> case newArray# n a s of
  (# s', array #) -> (# s', MutableArray array #)
{-# INLINE newArray #-}

readArray :: MonadPrim m => MutableArray (World m) a -> Int -> m a
readArray (MutableArray array) (I# i) = liftPrim $ readArray# array i
{-# INLINE readArray #-}

writeArray :: MonadPrim m => MutableArray (World m) a -> Int -> a -> m ()
writeArray (MutableArray array) (I# i) a = liftPrim $ \ s ->
  case writeArray# array i a s of
    s' -> (# s', () #)
{-# INLINE writeArray #-}

newtype Size a = Size { getSize :: Int }

instance Functor Size where
  fmap _ (Size a) = Size a

instance Applicative Size where
  pure _ = Size 0
  Size f <*> Size a = Size $! f + a

size :: Traversable Empty a b => a -> Int
size = getSize . traverse (Empty $ Size 1)
