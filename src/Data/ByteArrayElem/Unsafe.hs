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
       ) where

import Control.Monad
import Control.Monad.Prim

import Data.Int
import Data.Prim.ByteArray
import Data.Word

#include "MachDeps.h"

class ByteArrayElem a where
  byteSize :: t a -> Int
  readByteArray :: MutableByteArray s -> Int -> Prim s a
  writeByteArray :: MutableByteArray s -> Int -> a -> Prim s ()

instance ByteArrayElem Bool where
  byteSize _ = 1
  {-# INLINE byteSize #-}
  readByteArray array = liftM (/= 0) . readInt8Array array
  {-# INLINE readByteArray #-}
  writeByteArray array i e = writeInt8Array array i $! if e then 1 else 0
  {-# INLINE writeByteArray #-}

instance ByteArrayElem Char where
  byteSize _ = SIZEOF_HSCHAR
  {-# INLINE byteSize #-}
  readByteArray = readWideCharArray
  {-# INLINE readByteArray #-}
  writeByteArray = writeWideCharArray
  {-# INLINE writeByteArray #-}

instance ByteArrayElem Int where
  byteSize _ = SIZEOF_HSINT
  {-# INLINE byteSize #-}
  readByteArray = readIntArray
  {-# INLINE readByteArray #-}
  writeByteArray = writeIntArray
  {-# INLINE writeByteArray #-}

instance ByteArrayElem Word where
  byteSize _ = SIZEOF_HSWORD
  {-# INLINE byteSize #-}
  readByteArray = readWordArray
  {-# INLINE readByteArray #-}
  writeByteArray = writeWordArray
  {-# INLINE writeByteArray #-}

instance ByteArrayElem Float where
  byteSize _ = SIZEOF_HSFLOAT
  {-# INLINE byteSize #-}
  readByteArray = readFloatArray
  {-# INLINE readByteArray #-}
  writeByteArray = writeFloatArray
  {-# INLINE writeByteArray #-}

instance ByteArrayElem Double where
  byteSize _ = SIZEOF_HSDOUBLE
  {-# INLINE byteSize #-}
  readByteArray = readDoubleArray
  {-# INLINE readByteArray #-}
  writeByteArray = writeDoubleArray
  {-# INLINE writeByteArray #-}

instance ByteArrayElem Int8 where
  byteSize _ = SIZEOF_INT8
  {-# INLINE byteSize #-}
  readByteArray = readInt8Array
  {-# INLINE readByteArray #-}
  writeByteArray = writeInt8Array
  {-# INLINE writeByteArray #-}

instance ByteArrayElem Int16 where
  byteSize _ = SIZEOF_INT16
  {-# INLINE byteSize #-}
  readByteArray = readInt16Array
  {-# INLINE readByteArray #-}
  writeByteArray = writeInt16Array
  {-# INLINE writeByteArray #-}

instance ByteArrayElem Int32 where
  byteSize _ = SIZEOF_INT32
  {-# INLINE byteSize #-}
  readByteArray = readInt32Array
  {-# INLINE readByteArray #-}
  writeByteArray = writeInt32Array
  {-# INLINE writeByteArray #-}

instance ByteArrayElem Int64 where
  byteSize _ = SIZEOF_INT64
  {-# INLINE byteSize #-}
  readByteArray = readInt64Array
  {-# INLINE readByteArray #-}
  writeByteArray = writeInt64Array
  {-# INLINE writeByteArray #-}

instance ByteArrayElem Word8 where
  byteSize _ = SIZEOF_WORD8
  {-# INLINE byteSize #-}
  readByteArray = readWord8Array
  {-# INLINE readByteArray #-}
  writeByteArray = writeWord8Array
  {-# INLINE writeByteArray #-}

instance ByteArrayElem Word16 where
  byteSize _ = SIZEOF_WORD16
  {-# INLINE byteSize #-}
  readByteArray = readWord16Array
  {-# INLINE readByteArray #-}
  writeByteArray = writeWord16Array
  {-# INLINE writeByteArray #-}

instance ByteArrayElem Word32 where
  byteSize _ = SIZEOF_WORD32
  {-# INLINE byteSize #-}
  readByteArray = readWord32Array
  {-# INLINE readByteArray #-}
  writeByteArray = writeWord32Array
  {-# INLINE writeByteArray #-}

instance ByteArrayElem Word64 where
  byteSize _ = SIZEOF_WORD64
  {-# INLINE byteSize #-}
  readByteArray = readWord64Array
  {-# INLINE readByteArray #-}
  writeByteArray = writeWord64Array
  {-# INLINE writeByteArray #-}
