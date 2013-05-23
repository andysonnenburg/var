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
  plusByteSize :: Int -> t a -> Int
  readBytes :: MonadPrim m => MutableByteArray (World m) -> Int -> m a
  writeBytes :: MonadPrim m => MutableByteArray (World m) -> Int -> a -> m ()

  default plusByteSize :: (Generic a, GByteArraySlice (Rep a)) => Int -> t a -> Int
  plusByteSize i a = gplusByteSize i (reproxyRep a)
  {-# INLINE plusByteSize #-}

  default readBytes :: ( Generic a
                           , GByteArraySlice (Rep a)
                           , MonadPrim m
                           ) => MutableByteArray (World m) -> Int -> m a
  readBytes array = liftM to . greadBytes array
  {-# INLINE readBytes #-}

  default writeBytes :: ( Generic a
                            , GByteArraySlice (Rep a)
                            , MonadPrim m
                            ) => MutableByteArray (World m) -> Int -> a -> m ()
  writeBytes array i = gwriteBytes array i . from
  {-# INLINE writeBytes #-}

byteSizeOf :: ByteArraySlice a => a -> Int
byteSizeOf = plusByteSize 0 . proxy
{-# INLINE byteSizeOf #-}

class GByteArraySlice a where
  gplusByteSize :: Int -> t (a p) -> Int
  greadBytes :: MonadPrim m => MutableByteArray (World m) -> Int -> m (a p)
  gwriteBytes :: MonadPrim m => MutableByteArray (World m) -> Int -> a p -> m ()

instance GByteArraySlice U1 where
  gplusByteSize = const
  {-# INLINE gplusByteSize #-}
  greadBytes _ _ = return U1
  {-# INLINE greadBytes #-}
  gwriteBytes _ _ _ = return ()
  {-# INLINE gwriteBytes #-}

instance ByteArraySlice c => GByteArraySlice (K1 i c) where
  gplusByteSize i = plusByteSize i . reproxyK1
  {-# INLINE gplusByteSize #-}
  greadBytes array = liftM K1 . readBytes array
  {-# INLINE greadBytes #-}
  gwriteBytes array i = writeBytes array i . unK1
  {-# INLINE gwriteBytes #-}

instance GByteArraySlice f => GByteArraySlice (M1 i c f) where
  gplusByteSize i = gplusByteSize i . reproxyM1
  {-# INLINE gplusByteSize #-}
  greadBytes array = liftM M1 . greadBytes array
  {-# INLINE greadBytes #-}
  gwriteBytes array i = gwriteBytes array i . unM1
  {-# INLINE gwriteBytes #-}

instance (GByteArraySlice a, GByteArraySlice b) => GByteArraySlice (a :*: b) where
  gplusByteSize i a =
    gplusByteSize (gplusByteSize i (reproxyFst a)) (reproxySnd a)
  {-# INLINE gplusByteSize #-}
  greadBytes array i = do
    a <- greadBytes array i
    b <- greadBytes array (gplusByteSize i (proxy a))
    return $ a :*: b
  {-# INLINE greadBytes #-}
  gwriteBytes array i (a :*: b) = do
    gwriteBytes array i a
    gwriteBytes array (gplusByteSize i (proxy a)) b
  {-# INLINE gwriteBytes #-}

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
  plusByteSize = plusByteSizeDefault 1
  {-# INLINE plusByteSize #-}
  readBytes array = liftM (/= 0) . readInt8Array array
  {-# INLINE readBytes #-}
  writeBytes array i e = writeInt8Array array i $! if e then 1 else 0
  {-# INLINE writeBytes #-}

instance ByteArraySlice Char where
  plusByteSize = plusByteSizeDefault SIZEOF_HSCHAR
  {-# INLINE plusByteSize #-}
  readBytes = readBytesDefault SIZEOF_HSCHAR readWideCharArray
  {-# INLINE readBytes #-}
  writeBytes = writeBytesDefault SIZEOF_HSCHAR writeWideCharArray
  {-# INLINE writeBytes #-}

instance ByteArraySlice Int where
  plusByteSize = plusByteSizeDefault SIZEOF_HSINT
  {-# INLINE plusByteSize #-}
  readBytes = readBytesDefault SIZEOF_HSINT readIntArray
  {-# INLINE readBytes #-}
  writeBytes = writeBytesDefault SIZEOF_HSINT writeIntArray
  {-# INLINE writeBytes #-}

instance ByteArraySlice Word where
  plusByteSize = plusByteSizeDefault SIZEOF_HSWORD
  {-# INLINE plusByteSize #-}
  readBytes = readBytesDefault SIZEOF_HSWORD readWordArray
  {-# INLINE readBytes #-}
  writeBytes = writeBytesDefault SIZEOF_HSWORD writeWordArray
  {-# INLINE writeBytes #-}

instance ByteArraySlice Float where
  plusByteSize = plusByteSizeDefault SIZEOF_HSFLOAT
  {-# INLINE plusByteSize #-}
  readBytes = readBytesDefault SIZEOF_HSFLOAT readFloatArray
  {-# INLINE readBytes #-}
  writeBytes = writeBytesDefault SIZEOF_HSFLOAT writeFloatArray
  {-# INLINE writeBytes #-}

instance ByteArraySlice Double where
  plusByteSize = plusByteSizeDefault SIZEOF_HSDOUBLE
  {-# INLINE plusByteSize #-}
  readBytes = readBytesDefault SIZEOF_HSDOUBLE readDoubleArray
  {-# INLINE readBytes #-}
  writeBytes = writeBytesDefault SIZEOF_HSDOUBLE writeDoubleArray
  {-# INLINE writeBytes #-}

instance ByteArraySlice Int8 where
  plusByteSize = plusByteSizeDefault SIZEOF_INT8
  {-# INLINE plusByteSize #-}
  readBytes = readBytesDefault SIZEOF_INT8 readInt8Array
  {-# INLINE readBytes #-}
  writeBytes = writeBytesDefault SIZEOF_INT8 writeInt8Array
  {-# INLINE writeBytes #-}

instance ByteArraySlice Int16 where
  plusByteSize = plusByteSizeDefault SIZEOF_INT16
  {-# INLINE plusByteSize #-}
  readBytes = readBytesDefault SIZEOF_INT16 readInt16Array
  {-# INLINE readBytes #-}
  writeBytes = writeBytesDefault SIZEOF_INT16 writeInt16Array
  {-# INLINE writeBytes #-}

instance ByteArraySlice Int32 where
  plusByteSize = plusByteSizeDefault SIZEOF_INT32
  {-# INLINE plusByteSize #-}
  readBytes = readBytesDefault SIZEOF_INT32 readInt32Array
  {-# INLINE readBytes #-}
  writeBytes = writeBytesDefault SIZEOF_INT32 writeInt32Array
  {-# INLINE writeBytes #-}

instance ByteArraySlice Int64 where
  plusByteSize = plusByteSizeDefault SIZEOF_INT64
  {-# INLINE plusByteSize #-}
  readBytes = readBytesDefault SIZEOF_INT64 readInt64Array
  {-# INLINE readBytes #-}
  writeBytes = writeBytesDefault SIZEOF_INT64 writeInt64Array
  {-# INLINE writeBytes #-}

instance ByteArraySlice Word8 where
  plusByteSize = plusByteSizeDefault SIZEOF_WORD8
  {-# INLINE plusByteSize #-}
  readBytes = readBytesDefault SIZEOF_WORD8 readWord8Array
  {-# INLINE readBytes #-}
  writeBytes = writeBytesDefault SIZEOF_WORD8 writeWord8Array
  {-# INLINE writeBytes #-}

instance ByteArraySlice Word16 where
  plusByteSize = plusByteSizeDefault SIZEOF_WORD16
  {-# INLINE plusByteSize #-}
  readBytes = readBytesDefault SIZEOF_WORD16 readWord16Array
  {-# INLINE readBytes #-}
  writeBytes = writeBytesDefault SIZEOF_WORD16 writeWord16Array
  {-# INLINE writeBytes #-}

instance ByteArraySlice Word32 where
  plusByteSize = plusByteSizeDefault SIZEOF_WORD32
  {-# INLINE plusByteSize #-}
  readBytes = readBytesDefault SIZEOF_WORD32 readWord32Array
  {-# INLINE readBytes #-}
  writeBytes = writeBytesDefault SIZEOF_WORD32 writeWord32Array
  {-# INLINE writeBytes #-}

instance ByteArraySlice Word64 where
  plusByteSize = plusByteSizeDefault SIZEOF_WORD64
  {-# INLINE plusByteSize #-}
  readBytes = readBytesDefault SIZEOF_WORD64 readWord64Array
  {-# INLINE readBytes #-}
  writeBytes = writeBytesDefault SIZEOF_WORD64 writeWord64Array
  {-# INLINE writeBytes #-}

plusByteSizeDefault :: Int -> Int -> a -> Int
plusByteSizeDefault size = \ i _ -> case i `rem` size of
  0 -> i + size
  i' -> i + (size - i') + size
{-# INLINE plusByteSizeDefault #-}

readBytesDefault :: Int ->
                        (MutableByteArray s -> Int -> a) ->
                        MutableByteArray s -> Int -> a
readBytesDefault alignment f = \ array i ->
  f array $! case i `quotRem'` alignment of
    (q, 0) -> q
    (q, _) -> q + 1
{-# INLINE readBytesDefault #-}

writeBytesDefault :: Int ->
                         (MutableByteArray s -> Int -> a -> b) ->
                         MutableByteArray s -> Int -> a -> b
writeBytesDefault alignment f array i a = i' `seq` f array i' a
  where
    i' = case i `quotRem'` alignment of
      (q, 0) -> q
      (q, _) -> q + 1
{-# INLINE writeBytesDefault #-}

quotRem' :: Integral a => a -> a -> (a, a)
quotRem' x y = (x `quot` y, x `rem` y)
{-# INLINE quotRem' #-}
