{-# LANGUAGE
    CPP
  , DefaultSignatures
  , FlexibleContexts
  , TypeOperators #-}
#ifdef LANGUAGE_Unsafe
{-# LANGUAGE Unsafe #-}
#endif
{- |
Copyright   :  (c) Andy Sonnenburg 2013
License     :  BSD3
Maintainer  :  andy22286@gmail.com
-}
module Data.ByteArraySlice.Unsafe
       ( ByteArraySlice (..)
       , byteSizeOf
       ) where

import Control.Monad
import Control.Monad.Prim.Class

import Data.Int
import Data.Prim.ByteArray
import Data.Proxy
import Data.Word

import GHC.Generics

#include "MachDeps.h"

class ByteArraySlice a where
  byteSize :: t a -> Int
  readByteArray :: MonadPrim m => MutableByteArray (World m) -> Int -> m a
  writeByteArray :: MonadPrim m => MutableByteArray (World m) -> Int -> a -> m ()

  default byteSize :: (Generic a, GByteArraySlice (Rep a)) => t a -> Int
  byteSize a = gbyteSize (reproxyRep a)
  {-# INLINE byteSize #-}

  default readByteArray :: ( Generic a
                           , GByteArraySlice (Rep a)
                           , MonadPrim m
                           ) => MutableByteArray (World m) -> Int -> m a
  readByteArray array = liftM to . greadByteArray array
  {-# INLINE readByteArray #-}

  default writeByteArray :: ( Generic a
                            , GByteArraySlice (Rep a)
                            , MonadPrim m
                            ) => MutableByteArray (World m) -> Int -> a -> m ()
  writeByteArray array i = gwriteByteArray array i . from
  {-# INLINE writeByteArray #-}

byteSizeOf :: ByteArraySlice a => a -> Int
byteSizeOf a = byteSize (proxy a)
{-# INLINE byteSizeOf #-}

class GByteArraySlice a where
  gbyteSize :: t (a p) -> Int
  greadByteArray :: MonadPrim m => MutableByteArray (World m) -> Int -> m (a p)
  gwriteByteArray :: MonadPrim m => MutableByteArray (World m) -> Int -> a p -> m ()

instance GByteArraySlice U1 where
  gbyteSize _ = 0
  {-# INLINE gbyteSize #-}
  greadByteArray _ _ = return U1
  {-# INLINE greadByteArray #-}
  gwriteByteArray _ _ _ = return ()
  {-# INLINE gwriteByteArray #-}

instance ByteArraySlice c => GByteArraySlice (K1 i c) where
  gbyteSize = byteSize . reproxyK1
  {-# INLINE gbyteSize #-}
  greadByteArray array = liftM K1 . readByteArray array
  {-# INLINE greadByteArray #-}
  gwriteByteArray array i = writeByteArray array i . unK1
  {-# INLINE gwriteByteArray #-}

instance GByteArraySlice f => GByteArraySlice (M1 i c f) where
  gbyteSize = gbyteSize . reproxyM1
  {-# INLINE gbyteSize #-}
  greadByteArray array = liftM M1 . greadByteArray array
  {-# INLINE greadByteArray #-}
  gwriteByteArray array i = gwriteByteArray array i . unM1
  {-# INLINE gwriteByteArray #-}

instance (GByteArraySlice a, GByteArraySlice b) => GByteArraySlice (a :*: b) where
  gbyteSize a = gbyteSize (reproxyFst a) + gbyteSize (reproxySnd a)
  {-# INLINE gbyteSize #-}
  greadByteArray array i = do
    a <- greadByteArray array i
    b <- greadByteArray array (i + gbyteSizeOf a)
    return $ a :*: b
  {-# INLINE greadByteArray #-}
  gwriteByteArray array i (a :*: b) = do
    gwriteByteArray array i a
    gwriteByteArray array (i + gbyteSizeOf a) b
  {-# INLINE gwriteByteArray #-}

gbyteSizeOf :: GByteArraySlice a => a p -> Int
gbyteSizeOf a = gbyteSize (proxy a)
{-# INLINE gbyteSizeOf #-}

instance ByteArraySlice ()
instance (ByteArraySlice a, ByteArraySlice b) => ByteArraySlice (a, b)
instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         ) => ByteArraySlice (a, b, c)
instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         , ByteArraySlice d
         ) => ByteArraySlice (a, b, c, d)
instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         , ByteArraySlice d
         , ByteArraySlice e
         ) => ByteArraySlice (a, b, c, d, e)
instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         , ByteArraySlice d
         , ByteArraySlice e
         , ByteArraySlice f
         ) => ByteArraySlice (a, b, c, d, e, f)
instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         , ByteArraySlice d
         , ByteArraySlice e
         , ByteArraySlice f
         , ByteArraySlice g
         ) => ByteArraySlice (a, b, c, d, e, f, g)

instance ByteArraySlice Bool where
  byteSize _ = 1
  {-# INLINE byteSize #-}
  readByteArray array = liftM (/= 0) . readInt8Array array
  {-# INLINE readByteArray #-}
  writeByteArray array i e = writeInt8Array array i (if e then 1 else 0)
  {-# INLINE writeByteArray #-}

instance ByteArraySlice Char where
  byteSize _ = SIZEOF_HSCHAR
  {-# INLINE byteSize #-}
  readByteArray = readWideCharArray
  {-# INLINE readByteArray #-}
  writeByteArray = writeWideCharArray
  {-# INLINE writeByteArray #-}

instance ByteArraySlice Int where
  byteSize _ = SIZEOF_HSINT
  {-# INLINE byteSize #-}
  readByteArray = readIntArray
  {-# INLINE readByteArray #-}
  writeByteArray = writeIntArray
  {-# INLINE writeByteArray #-}

instance ByteArraySlice Word where
  byteSize _ = SIZEOF_HSWORD
  {-# INLINE byteSize #-}
  readByteArray = readWordArray
  {-# INLINE readByteArray #-}
  writeByteArray = writeWordArray
  {-# INLINE writeByteArray #-}

instance ByteArraySlice Float where
  byteSize _ = SIZEOF_HSFLOAT
  {-# INLINE byteSize #-}
  readByteArray = readFloatArray
  {-# INLINE readByteArray #-}
  writeByteArray = writeFloatArray
  {-# INLINE writeByteArray #-}

instance ByteArraySlice Double where
  byteSize _ = SIZEOF_HSDOUBLE
  {-# INLINE byteSize #-}
  readByteArray = readDoubleArray
  {-# INLINE readByteArray #-}
  writeByteArray = writeDoubleArray
  {-# INLINE writeByteArray #-}

instance ByteArraySlice Int8 where
  byteSize _ = SIZEOF_INT8
  {-# INLINE byteSize #-}
  readByteArray = readInt8Array
  {-# INLINE readByteArray #-}
  writeByteArray = writeInt8Array
  {-# INLINE writeByteArray #-}

instance ByteArraySlice Int16 where
  byteSize _ = SIZEOF_INT16
  {-# INLINE byteSize #-}
  readByteArray = readInt16Array
  {-# INLINE readByteArray #-}
  writeByteArray = writeInt16Array
  {-# INLINE writeByteArray #-}

instance ByteArraySlice Int32 where
  byteSize _ = SIZEOF_INT32
  {-# INLINE byteSize #-}
  readByteArray = readInt32Array
  {-# INLINE readByteArray #-}
  writeByteArray = writeInt32Array
  {-# INLINE writeByteArray #-}

instance ByteArraySlice Int64 where
  byteSize _ = SIZEOF_INT64
  {-# INLINE byteSize #-}
  readByteArray = readInt64Array
  {-# INLINE readByteArray #-}
  writeByteArray = writeInt64Array
  {-# INLINE writeByteArray #-}

instance ByteArraySlice Word8 where
  byteSize _ = SIZEOF_WORD8
  {-# INLINE byteSize #-}
  readByteArray = readWord8Array
  {-# INLINE readByteArray #-}
  writeByteArray = writeWord8Array
  {-# INLINE writeByteArray #-}

instance ByteArraySlice Word16 where
  byteSize _ = SIZEOF_WORD16
  {-# INLINE byteSize #-}
  readByteArray = readWord16Array
  {-# INLINE readByteArray #-}
  writeByteArray = writeWord16Array
  {-# INLINE writeByteArray #-}

instance ByteArraySlice Word32 where
  byteSize _ = SIZEOF_WORD32
  {-# INLINE byteSize #-}
  readByteArray = readWord32Array
  {-# INLINE readByteArray #-}
  writeByteArray = writeWord32Array
  {-# INLINE writeByteArray #-}

instance ByteArraySlice Word64 where
  byteSize _ = SIZEOF_WORD64
  {-# INLINE byteSize #-}
  readByteArray = readWord64Array
  {-# INLINE readByteArray #-}
  writeByteArray = writeWord64Array
  {-# INLINE writeByteArray #-}
