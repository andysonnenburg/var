{-# LANGUAGE CPP, DeriveDataTypeable, MagicHash, UnboxedTuples #-}
{- |
Copyright   :  (c) Andy Sonnenburg 2013
License     :  BSD3
Maintainer  :  andy22286@gmail.com
-}
module Data.Prim.ByteArray
       ( MutableByteArray
       , newByteArray
       , readCharArray
       , readWideCharArray
       , readIntArray
       , readWordArray
       , readFloatArray
       , readDoubleArray
       , readInt8Array
       , readInt16Array
       , readInt32Array
       , readInt64Array
       , readWord8Array
       , readWord16Array
       , readWord32Array
       , readWord64Array
       , readStablePtrArray
       , readFunPtrArray
       , readPtrArray
       , writeCharArray
       , writeWideCharArray
       , writeIntArray
       , writeWordArray
       , writeFloatArray
       , writeDoubleArray
       , writeInt8Array
       , writeInt16Array
       , writeInt32Array
       , writeInt64Array
       , writeWord8Array
       , writeWord16Array
       , writeWord32Array
       , writeWord64Array
       , writeStablePtrArray
       , writeFunPtrArray
       , writePtrArray
       ) where

import Control.Monad.Prim

import Data.Typeable (Typeable)

import GHC.Exts
import GHC.Int
import GHC.Stable
import GHC.Word

data MutableByteArray s = MutableByteArray (MutableByteArray# s) deriving Typeable

instance Eq (MutableByteArray s) where
  MutableByteArray a == MutableByteArray b = sameMutableByteArray# a b
  {-# INLINE (==) #-}

newByteArray :: Int -> Prim s (MutableByteArray s)
newByteArray (I# i) = prim $ \ s -> case newByteArray# i s of
  (# s', array #) -> (# s', MutableByteArray array #)
{-# INLINE newByteArray #-}

readCharArray :: MutableByteArray s -> Int -> Prim s Char
readCharArray (MutableByteArray array) (I# i) =
  prim $ \ s -> case readCharArray# array i s of
    (# s', e #) -> (# s', C# e #)
{-# INLINE readCharArray #-}

readWideCharArray :: MutableByteArray s -> Int -> Prim s Char
readWideCharArray (MutableByteArray array) (I# i) =
  prim $ \ s -> case readWideCharArray# array i s of
    (# s', e #) -> (# s', C# e #)
{-# INLINE readWideCharArray #-}

readIntArray :: MutableByteArray s -> Int -> Prim s Int
readIntArray (MutableByteArray array) (I# i) =
  prim $ \ s -> case readIntArray# array i s of
    (# s', e #) -> (# s', I# e #)
{-# INLINE readIntArray #-}

readWordArray :: MutableByteArray s -> Int -> Prim s Word
readWordArray (MutableByteArray array) (I# i) =
  prim $ \ s -> case readWordArray# array i s of
    (# s', e #) -> (# s', W# e #)
{-# INLINE readWordArray #-}

readFloatArray :: MutableByteArray s -> Int -> Prim s Float
readFloatArray (MutableByteArray array) (I# i) =
  prim $ \ s -> case readFloatArray# array i s of
    (# s', e #) -> (# s', F# e #)
{-# INLINE readFloatArray #-}

readDoubleArray :: MutableByteArray s -> Int -> Prim s Double
readDoubleArray (MutableByteArray array) (I# i) =
  prim $ \ s -> case readDoubleArray# array i s of
    (# s', e #) -> (# s', D# e #)
{-# INLINE readDoubleArray #-}

readInt8Array :: MutableByteArray s -> Int -> Prim s Int8
readInt8Array (MutableByteArray array) (I# i) =
  prim $ \ s -> case readInt8Array# array i s of
    (# s', e #) -> (# s', I8# e #)
{-# INLINE readInt8Array #-}

readInt16Array :: MutableByteArray s -> Int -> Prim s Int16
readInt16Array (MutableByteArray array) (I# i) =
  prim $ \ s -> case readInt16Array# array i s of
    (# s', e #) -> (# s', I16# e #)
{-# INLINE readInt16Array #-}

readInt32Array :: MutableByteArray s -> Int -> Prim s Int32
readInt32Array (MutableByteArray array) (I# i) =
  prim $ \ s -> case readInt32Array# array i s of
    (# s', e #) -> (# s', I32# e #)
{-# INLINE readInt32Array #-}

readInt64Array :: MutableByteArray s -> Int -> Prim s Int64
readInt64Array (MutableByteArray array) (I# i) =
  prim $ \ s -> case readInt64Array# array i s of
    (# s', e #) -> (# s', I64# e #)
{-# INLINE readInt64Array #-}

readWord8Array :: MutableByteArray s -> Int -> Prim s Word8
readWord8Array (MutableByteArray array) (I# i) =
  prim $ \ s -> case readWord8Array# array i s of
    (# s', e #) -> (# s', W8# e #)
{-# INLINE readWord8Array #-}

readWord16Array :: MutableByteArray s -> Int -> Prim s Word16
readWord16Array (MutableByteArray array) (I# i) =
  prim $ \ s -> case readWord16Array# array i s of
    (# s', e #) -> (# s', W16# e #)
{-# INLINE readWord16Array #-}

readWord32Array :: MutableByteArray s -> Int -> Prim s Word32
readWord32Array (MutableByteArray array) (I# i) =
  prim $ \ s -> case readWord32Array# array i s of
    (# s', e #) -> (# s', W32# e #)
{-# INLINE readWord32Array #-}

readWord64Array :: MutableByteArray s -> Int -> Prim s Word64
readWord64Array (MutableByteArray array) (I# i) =
  prim $ \ s -> case readWord64Array# array i s of
    (# s', e #) -> (# s', W64# e #)
{-# INLINE readWord64Array #-}

readStablePtrArray :: MutableByteArray s -> Int -> Prim s (StablePtr a)
readStablePtrArray (MutableByteArray array) (I# i) =
  prim $ \ s -> case readStablePtrArray# array i s of
    (# s', e #) -> (# s', StablePtr e #)
{-# INLINE readStablePtrArray #-}

readFunPtrArray :: MutableByteArray s -> Int -> Prim s (FunPtr a)
readFunPtrArray (MutableByteArray array) (I# i) =
  prim $ \ s -> case readAddrArray# array i s of
    (# s', e #) -> (# s', FunPtr e #)
{-# INLINE readFunPtrArray #-}

readPtrArray :: MutableByteArray s -> Int -> Prim s (Ptr a)
readPtrArray (MutableByteArray array) (I# i) =
  prim $ \ s -> case readAddrArray# array i s of
    (# s', e #) -> (# s', Ptr e #)
{-# INLINE readPtrArray #-}

writeCharArray :: MutableByteArray s -> Int -> Char -> Prim s ()
writeCharArray (MutableByteArray array) (I# i) (C# e) =
  prim $ \ s -> case writeCharArray# array i e s of
    s' -> (# s', () #)
{-# INLINE writeCharArray #-}

writeWideCharArray :: MutableByteArray s -> Int -> Char -> Prim s ()
writeWideCharArray (MutableByteArray array) (I# i) (C# e) =
  prim $ \ s -> case writeWideCharArray# array i e s of
    s' -> (# s', () #)
{-# INLINE writeWideCharArray #-}

writeIntArray :: MutableByteArray s -> Int -> Int -> Prim s ()
writeIntArray (MutableByteArray array) (I# i) (I# e) =
  prim $ \ s -> case writeIntArray# array i e s of
    s' -> (# s', () #)
{-# INLINE writeIntArray #-}

writeWordArray :: MutableByteArray s -> Int -> Word -> Prim s ()
writeWordArray (MutableByteArray array) (I# i) (W# e) =
  prim $ \ s -> case writeWordArray# array i e s of
    s' -> (# s', () #)
{-# INLINE writeWordArray #-}

writeFloatArray :: MutableByteArray s -> Int -> Float -> Prim s ()
writeFloatArray (MutableByteArray array) (I# i) (F# e) =
  prim $ \ s -> case writeFloatArray# array i e s of
    s' -> (# s', () #)
{-# INLINE writeFloatArray #-}

writeDoubleArray :: MutableByteArray s -> Int -> Double -> Prim s ()
writeDoubleArray (MutableByteArray array) (I# i) (D# e) =
  prim $ \ s -> case writeDoubleArray# array i e s of
    s' -> (# s', () #)
{-# INLINE writeDoubleArray #-}

writeInt8Array :: MutableByteArray s -> Int -> Int8 -> Prim s ()
writeInt8Array (MutableByteArray array) (I# i) (I8# e) =
  prim $ \ s -> case writeInt8Array# array i e s of
    s' -> (# s', () #)
{-# INLINE writeInt8Array #-}

writeInt16Array :: MutableByteArray s -> Int -> Int16 -> Prim s ()
writeInt16Array (MutableByteArray array) (I# i) (I16# e) =
  prim $ \ s -> case writeInt16Array# array i e s of
    s' -> (# s', () #)
{-# INLINE writeInt16Array #-}

writeInt32Array :: MutableByteArray s -> Int -> Int32 -> Prim s ()
writeInt32Array (MutableByteArray array) (I# i) (I32# e) =
  prim $ \ s -> case writeInt32Array# array i e s of
    s' -> (# s', () #)
{-# INLINE writeInt32Array #-}

writeInt64Array :: MutableByteArray s -> Int -> Int64 -> Prim s ()
writeInt64Array (MutableByteArray array) (I# i) (I64# e) =
  prim $ \ s -> case writeInt64Array# array i e s of
    s' -> (# s', () #)
{-# INLINE writeInt64Array #-}

writeWord8Array :: MutableByteArray s -> Int -> Word8 -> Prim s ()
writeWord8Array (MutableByteArray array) (I# i) (W8# e) =
  prim $ \ s -> case writeWord8Array# array i e s of
    s' -> (# s', () #)
{-# INLINE writeWord8Array #-}

writeWord16Array :: MutableByteArray s -> Int -> Word16 -> Prim s ()
writeWord16Array (MutableByteArray array) (I# i) (W16# e) =
  prim $ \ s -> case writeWord16Array# array i e s of
    s' -> (# s', () #)
{-# INLINE writeWord16Array #-}

writeWord32Array :: MutableByteArray s -> Int -> Word32 -> Prim s ()
writeWord32Array (MutableByteArray array) (I# i) (W32# e) =
  prim $ \ s -> case writeWord32Array# array i e s of
    s' -> (# s', () #)
{-# INLINE writeWord32Array #-}

writeWord64Array :: MutableByteArray s -> Int -> Word64 -> Prim s ()
writeWord64Array (MutableByteArray array) (I# i) (W64# e) =
  prim $ \ s -> case writeWord64Array# array i e s of
    s' -> (# s', () #)
{-# INLINE writeWord64Array #-}

writeStablePtrArray :: MutableByteArray s -> Int -> StablePtr a -> Prim s ()
writeStablePtrArray (MutableByteArray array) (I# i) (StablePtr e) =
  prim $ \ s -> case writeStablePtrArray# array i e s of
    s' -> (# s', () #)
{-# INLINE writeStablePtrArray #-}

writeFunPtrArray :: MutableByteArray s -> Int -> FunPtr a -> Prim s ()
writeFunPtrArray (MutableByteArray array) (I# i) (FunPtr e) =
  prim $ \ s -> case writeAddrArray# array i e s of
    s' -> (# s', () #)
{-# INLINE writeFunPtrArray #-}

writePtrArray :: MutableByteArray s -> Int -> Ptr a -> Prim s ()
writePtrArray (MutableByteArray array) (I# i) (Ptr e) =
  prim $ \ s -> case writeAddrArray# array i e s of
    s' -> (# s', () #)
{-# INLINE writePtrArray #-}
