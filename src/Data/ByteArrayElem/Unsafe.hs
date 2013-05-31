{-# LANGUAGE CPP #-}
#ifdef LANGUAGE_Unsafe
{-# LANGUAGE Unsafe #-}
#endif
{- |
Copyright   :  (c) Andy Sonnenburg 2013
License     :  BSD3
Maintainer  :  andy22286@gmail.com
-}
module Data.ByteArrayElem.Unsafe
       ( ByteArrayElem (..)
       , plusByteSizeDefault
       , readByteOffDefault
       , writeByteOffDefault
       ) where

import Control.Monad.Prim

import Data.Int
import Data.Prim.ByteArray
import Data.Proxy
import Data.Word

import Foreign.Ptr
import Foreign.StablePtr

#include "MachDeps.h"

class ByteArrayElem a where
  byteSize :: t a -> Int
  readElemOff :: MutableByteArray s -> Int -> Prim s a
  writeElemOff :: MutableByteArray s -> Int -> a -> Prim s ()

instance ByteArrayElem Bool where
  byteSize _ = 1
  {-# INLINE byteSize #-}
  readElemOff array = fmap (/= 0) . readInt8Array array
  {-# INLINE readElemOff #-}
  writeElemOff array i e = writeInt8Array array i $! if e then 1 else 0
  {-# INLINE writeElemOff #-}

instance ByteArrayElem Char where
  byteSize _ = SIZEOF_HSCHAR
  {-# INLINE byteSize #-}
  readElemOff = readWideCharArray
  {-# INLINE readElemOff #-}
  writeElemOff = writeWideCharArray
  {-# INLINE writeElemOff #-}

instance ByteArrayElem Int where
  byteSize _ = SIZEOF_HSINT
  {-# INLINE byteSize #-}
  readElemOff = readIntArray
  {-# INLINE readElemOff #-}
  writeElemOff = writeIntArray
  {-# INLINE writeElemOff #-}

instance ByteArrayElem Word where
  byteSize _ = SIZEOF_HSWORD
  {-# INLINE byteSize #-}
  readElemOff = readWordArray
  {-# INLINE readElemOff #-}
  writeElemOff = writeWordArray
  {-# INLINE writeElemOff #-}

instance ByteArrayElem Float where
  byteSize _ = SIZEOF_HSFLOAT
  {-# INLINE byteSize #-}
  readElemOff = readFloatArray
  {-# INLINE readElemOff #-}
  writeElemOff = writeFloatArray
  {-# INLINE writeElemOff #-}

instance ByteArrayElem Double where
  byteSize _ = SIZEOF_HSDOUBLE
  {-# INLINE byteSize #-}
  readElemOff = readDoubleArray
  {-# INLINE readElemOff #-}
  writeElemOff = writeDoubleArray
  {-# INLINE writeElemOff #-}

instance ByteArrayElem Int8 where
  byteSize _ = SIZEOF_INT8
  {-# INLINE byteSize #-}
  readElemOff = readInt8Array
  {-# INLINE readElemOff #-}
  writeElemOff = writeInt8Array
  {-# INLINE writeElemOff #-}

instance ByteArrayElem Int16 where
  byteSize _ = SIZEOF_INT16
  {-# INLINE byteSize #-}
  readElemOff = readInt16Array
  {-# INLINE readElemOff #-}
  writeElemOff = writeInt16Array
  {-# INLINE writeElemOff #-}

instance ByteArrayElem Int32 where
  byteSize _ = SIZEOF_INT32
  {-# INLINE byteSize #-}
  readElemOff = readInt32Array
  {-# INLINE readElemOff #-}
  writeElemOff = writeInt32Array
  {-# INLINE writeElemOff #-}

instance ByteArrayElem Int64 where
  byteSize _ = SIZEOF_INT64
  {-# INLINE byteSize #-}
  readElemOff = readInt64Array
  {-# INLINE readElemOff #-}
  writeElemOff = writeInt64Array
  {-# INLINE writeElemOff #-}

instance ByteArrayElem Word8 where
  byteSize _ = SIZEOF_WORD8
  {-# INLINE byteSize #-}
  readElemOff = readWord8Array
  {-# INLINE readElemOff #-}
  writeElemOff = writeWord8Array
  {-# INLINE writeElemOff #-}

instance ByteArrayElem Word16 where
  byteSize _ = SIZEOF_WORD16
  {-# INLINE byteSize #-}
  readElemOff = readWord16Array
  {-# INLINE readElemOff #-}
  writeElemOff = writeWord16Array
  {-# INLINE writeElemOff #-}

instance ByteArrayElem Word32 where
  byteSize _ = SIZEOF_WORD32
  {-# INLINE byteSize #-}
  readElemOff = readWord32Array
  {-# INLINE readElemOff #-}
  writeElemOff = writeWord32Array
  {-# INLINE writeElemOff #-}

instance ByteArrayElem Word64 where
  byteSize _ = SIZEOF_WORD64
  {-# INLINE byteSize #-}
  readElemOff = readWord64Array
  {-# INLINE readElemOff #-}
  writeElemOff = writeWord64Array
  {-# INLINE writeElemOff #-}

instance ByteArrayElem (StablePtr a) where
  byteSize _ = SIZEOF_HSWORD
  {-# INLINE byteSize #-}
  readElemOff = readStablePtrArray
  {-# INLINE readElemOff #-}
  writeElemOff = writeStablePtrArray
  {-# INLINE writeElemOff #-}

instance ByteArrayElem (FunPtr a) where
  byteSize _ = SIZEOF_HSWORD
  {-# INLINE byteSize #-}
  readElemOff = readFunPtrArray
  {-# INLINE readElemOff #-}
  writeElemOff = writeFunPtrArray
  {-# INLINE writeElemOff #-}

instance ByteArrayElem (Ptr a) where
  byteSize _ = SIZEOF_HSWORD
  {-# INLINE byteSize #-}
  readElemOff = readPtrArray
  {-# INLINE readElemOff #-}
  writeElemOff = writePtrArray
  {-# INLINE writeElemOff #-}

plusByteSizeDefault :: ByteArrayElem a => Int -> t a -> Int
plusByteSizeDefault i a = case i `rem` byteSize' of
  0 -> i + byteSize'
  i' -> i + (byteSize' - i') + byteSize'
  where
    byteSize' = byteSize a
{-# INLINE plusByteSizeDefault #-}

readByteOffDefault :: ByteArrayElem a => MutableByteArray s -> Int -> Prim s a
readByteOffDefault array i = m
  where
    m = readElemOff array $ case i `quotRem'` byteSize' of
      (q, 0) -> q
      (q, _) -> q + 1
    byteSize' = byteSize m
{-# INLINE readByteOffDefault #-}

writeByteOffDefault :: ByteArrayElem a => MutableByteArray s -> Int -> a -> Prim s ()
writeByteOffDefault array i a = writeElemOff array i' a
  where
    i' = case i `quotRem'` byteSize (proxy a) of
      (q, 0) -> q
      (q, _) -> q + 1
{-# INLINE writeByteOffDefault #-}

quotRem' :: Integral a => a -> a -> (a, a)
quotRem' x y = (x `quot` y, x `rem` y)
{-# INLINE quotRem' #-}
