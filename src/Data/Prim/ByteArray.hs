{-# LANGUAGE CPP, DeriveDataTypeable, MagicHash, UnboxedTuples #-}
#ifdef LANGUAGE_Unsafe
{-# LANGUAGE Unsafe #-}
#endif
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
       ) where

import Control.Monad.Prim.Class

import Data.Typeable (Typeable)

import GHC.Exts
import GHC.Int
import GHC.Word

data MutableByteArray s = MutableByteArray (MutableByteArray# s) deriving Typeable

instance Eq (MutableByteArray s) where
  MutableByteArray a == MutableByteArray b = sameMutableByteArray# a b
  {-# INLINE (==) #-}

newByteArray :: MonadPrim m => Int -> m (MutableByteArray (World m))
newByteArray (I# i) = liftPrim $ \ s -> case newByteArray# i s of
  (# s', array #) -> (# s', MutableByteArray array #)
{-# INLINE newByteArray #-}

readCharArray :: MonadPrim m => MutableByteArray (World m) -> Int -> m Char
readCharArray (MutableByteArray array) (I# i) =
  liftPrim $ \ s -> case readCharArray# array i s of
    (# s', e #) -> (# s', C# e #)
{-# INLINE readCharArray #-}

readWideCharArray :: MonadPrim m => MutableByteArray (World m) -> Int -> m Char
readWideCharArray (MutableByteArray array) (I# i) =
  liftPrim $ \ s -> case readWideCharArray# array i s of
    (# s', e #) -> (# s', C# e #)
{-# INLINE readWideCharArray #-}

readIntArray :: MonadPrim m => MutableByteArray (World m) -> Int -> m Int
readIntArray (MutableByteArray array) (I# i) =
  liftPrim $ \ s -> case readIntArray# array i s of
    (# s', e #) -> (# s', I# e #)
{-# INLINE readIntArray #-}

readWordArray :: MonadPrim m => MutableByteArray (World m) -> Int -> m Word
readWordArray (MutableByteArray array) (I# i) =
  liftPrim $ \ s -> case readWordArray# array i s of
    (# s', e #) -> (# s', W# e #)
{-# INLINE readWordArray #-}

readFloatArray :: MonadPrim m => MutableByteArray (World m) -> Int -> m Float
readFloatArray (MutableByteArray array) (I# i) =
  liftPrim $ \ s -> case readFloatArray# array i s of
    (# s', e #) -> (# s', F# e #)
{-# INLINE readFloatArray #-}

readDoubleArray :: MonadPrim m => MutableByteArray (World m) -> Int -> m Double
readDoubleArray (MutableByteArray array) (I# i) =
  liftPrim $ \ s -> case readDoubleArray# array i s of
    (# s', e #) -> (# s', D# e #)
{-# INLINE readDoubleArray #-}

readInt8Array :: MonadPrim m => MutableByteArray (World m) -> Int -> m Int8
readInt8Array (MutableByteArray array) (I# i) =
  liftPrim $ \ s -> case readInt8Array# array i s of
    (# s', e #) -> (# s', I8# e #)
{-# INLINE readInt8Array #-}

readInt16Array :: MonadPrim m => MutableByteArray (World m) -> Int -> m Int16
readInt16Array (MutableByteArray array) (I# i) =
  liftPrim $ \ s -> case readInt16Array# array i s of
    (# s', e #) -> (# s', I16# e #)
{-# INLINE readInt16Array #-}

readInt32Array :: MonadPrim m => MutableByteArray (World m) -> Int -> m Int32
readInt32Array (MutableByteArray array) (I# i) =
  liftPrim $ \ s -> case readInt32Array# array i s of
    (# s', e #) -> (# s', I32# e #)
{-# INLINE readInt32Array #-}

readInt64Array :: MonadPrim m => MutableByteArray (World m) -> Int -> m Int64
readInt64Array (MutableByteArray array) (I# i) =
  liftPrim $ \ s -> case readInt64Array# array i s of
    (# s', e #) -> (# s', I64# e #)
{-# INLINE readInt64Array #-}

readWord8Array :: MonadPrim m => MutableByteArray (World m) -> Int -> m Word8
readWord8Array (MutableByteArray array) (I# i) =
  liftPrim $ \ s -> case readWord8Array# array i s of
    (# s', e #) -> (# s', W8# e #)
{-# INLINE readWord8Array #-}

readWord16Array :: MonadPrim m => MutableByteArray (World m) -> Int -> m Word16
readWord16Array (MutableByteArray array) (I# i) =
  liftPrim $ \ s -> case readWord16Array# array i s of
    (# s', e #) -> (# s', W16# e #)
{-# INLINE readWord16Array #-}

readWord32Array :: MonadPrim m => MutableByteArray (World m) -> Int -> m Word32
readWord32Array (MutableByteArray array) (I# i) =
  liftPrim $ \ s -> case readWord32Array# array i s of
    (# s', e #) -> (# s', W32# e #)
{-# INLINE readWord32Array #-}

readWord64Array :: MonadPrim m => MutableByteArray (World m) -> Int -> m Word64
readWord64Array (MutableByteArray array) (I# i) =
  liftPrim $ \ s -> case readWord64Array# array i s of
    (# s', e #) -> (# s', W64# e #)
{-# INLINE readWord64Array #-}

writeCharArray :: MonadPrim m => MutableByteArray (World m) -> Int -> Char -> m ()
writeCharArray (MutableByteArray array) (I# i) (C# e) =
  liftPrim $ \ s -> case writeCharArray# array i e s of
    s' -> (# s', () #)
{-# INLINE writeCharArray #-}

writeWideCharArray :: MonadPrim m => MutableByteArray (World m) -> Int -> Char -> m ()
writeWideCharArray (MutableByteArray array) (I# i) (C# e) =
  liftPrim $ \ s -> case writeWideCharArray# array i e s of
    s' -> (# s', () #)
{-# INLINE writeWideCharArray #-}

writeIntArray :: MonadPrim m => MutableByteArray (World m) -> Int -> Int -> m ()
writeIntArray (MutableByteArray array) (I# i) (I# e) =
  liftPrim $ \ s -> case writeIntArray# array i e s of
    s' -> (# s', () #)
{-# INLINE writeIntArray #-}

writeWordArray :: MonadPrim m => MutableByteArray (World m) -> Int -> Word -> m ()
writeWordArray (MutableByteArray array) (I# i) (W# e) =
  liftPrim $ \ s -> case writeWordArray# array i e s of
    s' -> (# s', () #)
{-# INLINE writeWordArray #-}

writeFloatArray :: MonadPrim m => MutableByteArray (World m) -> Int -> Float -> m ()
writeFloatArray (MutableByteArray array) (I# i) (F# e) =
  liftPrim $ \ s -> case writeFloatArray# array i e s of
    s' -> (# s', () #)
{-# INLINE writeFloatArray #-}

writeDoubleArray :: MonadPrim m => MutableByteArray (World m) -> Int -> Double -> m ()
writeDoubleArray (MutableByteArray array) (I# i) (D# e) =
  liftPrim $ \ s -> case writeDoubleArray# array i e s of
    s' -> (# s', () #)
{-# INLINE writeDoubleArray #-}

writeInt8Array :: MonadPrim m => MutableByteArray (World m) -> Int -> Int8 -> m ()
writeInt8Array (MutableByteArray array) (I# i) (I8# e) =
  liftPrim $ \ s -> case writeInt8Array# array i e s of
    s' -> (# s', () #)
{-# INLINE writeInt8Array #-}

writeInt16Array :: MonadPrim m => MutableByteArray (World m) -> Int -> Int16 -> m ()
writeInt16Array (MutableByteArray array) (I# i) (I16# e) =
  liftPrim $ \ s -> case writeInt16Array# array i e s of
    s' -> (# s', () #)
{-# INLINE writeInt16Array #-}

writeInt32Array :: MonadPrim m => MutableByteArray (World m) -> Int -> Int32 -> m ()
writeInt32Array (MutableByteArray array) (I# i) (I32# e) =
  liftPrim $ \ s -> case writeInt32Array# array i e s of
    s' -> (# s', () #)
{-# INLINE writeInt32Array #-}

writeInt64Array :: MonadPrim m => MutableByteArray (World m) -> Int -> Int64 -> m ()
writeInt64Array (MutableByteArray array) (I# i) (I64# e) =
  liftPrim $ \ s -> case writeInt64Array# array i e s of
    s' -> (# s', () #)
{-# INLINE writeInt64Array #-}

writeWord8Array :: MonadPrim m => MutableByteArray (World m) -> Int -> Word8 -> m ()
writeWord8Array (MutableByteArray array) (I# i) (W8# e) =
  liftPrim $ \ s -> case writeWord8Array# array i e s of
    s' -> (# s', () #)
{-# INLINE writeWord8Array #-}

writeWord16Array :: MonadPrim m => MutableByteArray (World m) -> Int -> Word16 -> m ()
writeWord16Array (MutableByteArray array) (I# i) (W16# e) =
  liftPrim $ \ s -> case writeWord16Array# array i e s of
    s' -> (# s', () #)
{-# INLINE writeWord16Array #-}

writeWord32Array :: MonadPrim m => MutableByteArray (World m) -> Int -> Word32 -> m ()
writeWord32Array (MutableByteArray array) (I# i) (W32# e) =
  liftPrim $ \ s -> case writeWord32Array# array i e s of
    s' -> (# s', () #)
{-# INLINE writeWord32Array #-}

writeWord64Array :: MonadPrim m => MutableByteArray (World m) -> Int -> Word64 -> m ()
writeWord64Array (MutableByteArray array) (I# i) (W64# e) =
  liftPrim $ \ s -> case writeWord64Array# array i e s of
    s' -> (# s', () #)
{-# INLINE writeWord64Array #-}
