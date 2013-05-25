{-# LANGUAGE CPP, DeriveDataTypeable, MagicHash, UnboxedTuples #-}
{- |
Copyright   :  (c) Andy Sonnenburg 2013
License     :  BSD3
Maintainer  :  andy22286@gmail.com
-}
module Data.Prim.Array
       ( MutableArray
       , newArray
       , readArray
       , writeArray
       ) where

import Control.Monad.Prim

import Data.Typeable (Typeable)

import GHC.Exts

data MutableArray s a = MutableArray (MutableArray# s a) deriving Typeable

instance Eq (MutableArray s a) where
  MutableArray a == MutableArray b = sameMutableArray# a b
  {-# INLINE (==) #-}

newArray :: Int -> a -> Prim s (MutableArray s a)
newArray (I# i) a = prim $ \ s -> case newArray# i a s of
  (# s', array #) -> (# s', MutableArray array #)
{-# INLINE newArray #-}

readArray :: MutableArray s a -> Int -> Prim s a
readArray (MutableArray array) (I# i) = prim $ readArray# array i
{-# INLINE readArray #-}

writeArray :: MutableArray s a -> Int -> a -> Prim s ()
writeArray (MutableArray array) (I# i) a =
  prim $ \ s -> case writeArray# array i a s of
    s' -> (# s', () #)
{-# INLINE writeArray #-}
