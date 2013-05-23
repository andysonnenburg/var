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
  readByteArray :: MonadPrim m => MutableByteArray (World m) -> Int -> m a
  writeByteArray :: MonadPrim m => MutableByteArray (World m) -> Int -> a -> m ()

  default plusByteSize :: (Generic a, GByteArraySlice (Rep a)) => Int -> t a -> Int
  plusByteSize i a = gplusByteSize i (reproxyRep a)
  {-# INLINE plusByteSize #-}

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
byteSizeOf = plusByteSize 0 . proxy
{-# INLINE byteSizeOf #-}

class GByteArraySlice a where
  gplusByteSize :: Int -> t (a p) -> Int
  greadByteArray :: MonadPrim m => MutableByteArray (World m) -> Int -> m (a p)
  gwriteByteArray :: MonadPrim m => MutableByteArray (World m) -> Int -> a p -> m ()

instance GByteArraySlice U1 where
  gplusByteSize i _ = i
  {-# INLINE gplusByteSize #-}
  greadByteArray _ _ = return U1
  {-# INLINE greadByteArray #-}
  gwriteByteArray _ _ _ = return ()
  {-# INLINE gwriteByteArray #-}

instance ByteArraySlice c => GByteArraySlice (K1 i c) where
  gplusByteSize i = plusByteSize i . reproxyK1
  {-# INLINE gplusByteSize #-}
  greadByteArray array = liftM K1 . readByteArray array
  {-# INLINE greadByteArray #-}
  gwriteByteArray array i = writeByteArray array i . unK1
  {-# INLINE gwriteByteArray #-}

instance GByteArraySlice f => GByteArraySlice (M1 i c f) where
  gplusByteSize i = gplusByteSize i . reproxyM1
  {-# INLINE gplusByteSize #-}
  greadByteArray array = liftM M1 . greadByteArray array
  {-# INLINE greadByteArray #-}
  gwriteByteArray array i = gwriteByteArray array i . unM1
  {-# INLINE gwriteByteArray #-}

instance (GByteArraySlice a, GByteArraySlice b) => GByteArraySlice (a :*: b) where
  gplusByteSize i a =
    gplusByteSize (gplusByteSize i (reproxyFst a)) (reproxySnd a)
  {-# INLINE gplusByteSize #-}
  greadByteArray array i = do
    a <- greadByteArray array i
    b <- greadByteArray array (gplusByteSize i (proxy a))
    return $ a :*: b
  {-# INLINE greadByteArray #-}
  gwriteByteArray array i (a :*: b) = do
    gwriteByteArray array i a
    gwriteByteArray array (gplusByteSize i (proxy a)) b
  {-# INLINE gwriteByteArray #-}

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
  readByteArray array = liftM (/= 0) . readInt8Array array
  {-# INLINE readByteArray #-}
  writeByteArray array i e = writeInt8Array array i $! if e then 1 else 0
  {-# INLINE writeByteArray #-}

instance ByteArraySlice Char where
  plusByteSize = plusByteSizeDefault SIZEOF_HSCHAR
  {-# INLINE plusByteSize #-}
  readByteArray = readByteArrayDefault SIZEOF_HSCHAR readWideCharArray
  {-# INLINE readByteArray #-}
  writeByteArray = writeByteArrayDefault SIZEOF_HSCHAR writeWideCharArray
  {-# INLINE writeByteArray #-}

instance ByteArraySlice Int where
  plusByteSize = plusByteSizeDefault SIZEOF_HSINT
  {-# INLINE plusByteSize #-}
  readByteArray = readByteArrayDefault SIZEOF_HSINT readIntArray
  {-# INLINE readByteArray #-}
  writeByteArray = writeByteArrayDefault SIZEOF_HSINT writeIntArray
  {-# INLINE writeByteArray #-}

instance ByteArraySlice Word where
  plusByteSize = plusByteSizeDefault SIZEOF_HSWORD
  {-# INLINE plusByteSize #-}
  readByteArray = readByteArrayDefault SIZEOF_HSWORD readWordArray
  {-# INLINE readByteArray #-}
  writeByteArray = writeByteArrayDefault SIZEOF_HSWORD writeWordArray
  {-# INLINE writeByteArray #-}

instance ByteArraySlice Float where
  plusByteSize = plusByteSizeDefault SIZEOF_HSFLOAT
  {-# INLINE plusByteSize #-}
  readByteArray = readByteArrayDefault SIZEOF_HSFLOAT readFloatArray
  {-# INLINE readByteArray #-}
  writeByteArray = writeByteArrayDefault SIZEOF_HSFLOAT writeFloatArray
  {-# INLINE writeByteArray #-}

instance ByteArraySlice Double where
  plusByteSize = plusByteSizeDefault SIZEOF_HSDOUBLE
  {-# INLINE plusByteSize #-}
  readByteArray = readByteArrayDefault SIZEOF_HSDOUBLE readDoubleArray
  {-# INLINE readByteArray #-}
  writeByteArray = writeByteArrayDefault SIZEOF_HSDOUBLE writeDoubleArray
  {-# INLINE writeByteArray #-}

instance ByteArraySlice Int8 where
  plusByteSize = plusByteSizeDefault SIZEOF_INT8
  {-# INLINE plusByteSize #-}
  readByteArray = readByteArrayDefault SIZEOF_INT8 readInt8Array
  {-# INLINE readByteArray #-}
  writeByteArray = writeByteArrayDefault SIZEOF_INT8 writeInt8Array
  {-# INLINE writeByteArray #-}

instance ByteArraySlice Int16 where
  plusByteSize = plusByteSizeDefault SIZEOF_INT16
  {-# INLINE plusByteSize #-}
  readByteArray = readByteArrayDefault SIZEOF_INT16 readInt16Array
  {-# INLINE readByteArray #-}
  writeByteArray = writeByteArrayDefault SIZEOF_INT16 writeInt16Array
  {-# INLINE writeByteArray #-}

instance ByteArraySlice Int32 where
  plusByteSize = plusByteSizeDefault SIZEOF_INT32
  {-# INLINE plusByteSize #-}
  readByteArray = readByteArrayDefault SIZEOF_INT32 readInt32Array
  {-# INLINE readByteArray #-}
  writeByteArray = writeByteArrayDefault SIZEOF_INT32 writeInt32Array
  {-# INLINE writeByteArray #-}

instance ByteArraySlice Int64 where
  plusByteSize = plusByteSizeDefault SIZEOF_INT64
  {-# INLINE plusByteSize #-}
  readByteArray = readByteArrayDefault SIZEOF_INT64 readInt64Array
  {-# INLINE readByteArray #-}
  writeByteArray = writeByteArrayDefault SIZEOF_INT64 writeInt64Array
  {-# INLINE writeByteArray #-}

instance ByteArraySlice Word8 where
  plusByteSize = plusByteSizeDefault SIZEOF_WORD8
  {-# INLINE plusByteSize #-}
  readByteArray = readByteArrayDefault SIZEOF_WORD8 readWord8Array
  {-# INLINE readByteArray #-}
  writeByteArray = writeByteArrayDefault SIZEOF_WORD8 writeWord8Array
  {-# INLINE writeByteArray #-}

instance ByteArraySlice Word16 where
  plusByteSize = plusByteSizeDefault SIZEOF_WORD16
  {-# INLINE plusByteSize #-}
  readByteArray = readByteArrayDefault SIZEOF_WORD16 readWord16Array
  {-# INLINE readByteArray #-}
  writeByteArray = writeByteArrayDefault SIZEOF_WORD16 writeWord16Array
  {-# INLINE writeByteArray #-}

instance ByteArraySlice Word32 where
  plusByteSize = plusByteSizeDefault SIZEOF_WORD32
  {-# INLINE plusByteSize #-}
  readByteArray = readByteArrayDefault SIZEOF_WORD32 readWord32Array
  {-# INLINE readByteArray #-}
  writeByteArray = writeByteArrayDefault SIZEOF_WORD32 writeWord32Array
  {-# INLINE writeByteArray #-}

instance ByteArraySlice Word64 where
  plusByteSize = plusByteSizeDefault SIZEOF_WORD64
  {-# INLINE plusByteSize #-}
  readByteArray = readByteArrayDefault SIZEOF_WORD64 readWord64Array
  {-# INLINE readByteArray #-}
  writeByteArray = writeByteArrayDefault SIZEOF_WORD64 writeWord64Array
  {-# INLINE writeByteArray #-}

plusByteSizeDefault :: Int -> Int -> a -> Int
plusByteSizeDefault size = \ i _ -> case i `rem` size of
  0 -> i + size
  i' -> i + (size - i') + size
{-# INLINE plusByteSizeDefault #-}

readByteArrayDefault :: Int ->
                        (MutableByteArray s -> Int -> a) ->
                        MutableByteArray s -> Int -> a
readByteArrayDefault alignment f = \ array i ->
  f array $! case i `quotRem'` alignment of
    (q, 0) -> q
    (q, _) -> q + 1
{-# INLINE readByteArrayDefault #-}

writeByteArrayDefault :: Int ->
                         (MutableByteArray s -> Int -> a -> b) ->
                         MutableByteArray s -> Int -> a -> b
writeByteArrayDefault alignment f array i a = i' `seq` f array i' a
  where
    i' = case i `quotRem'` alignment of
      (q, 0) -> q
      (q, _) -> q + 1
{-# INLINE writeByteArrayDefault #-}

quotRem' :: Integral a => a -> a -> (a, a)
quotRem' x y = (x `quot` y, x `rem` y)
{-# INLINE quotRem' #-}
