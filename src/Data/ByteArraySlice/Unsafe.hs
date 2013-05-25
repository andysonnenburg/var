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
       ( MutableByteArray
       , module Control.Monad.Prim
       , ByteArraySlice (..)
       , byteSizeOf
       ) where

import Control.Monad.Prim

import Data.ByteArrayElem.Unsafe
import Data.Int
import Data.Prim.ByteArray
import Data.Proxy
import Data.Word

import GHC.Generics

import Foreign.Ptr
import Foreign.StablePtr

class ByteArraySlice a where
  plusByteSize :: Int -> t a -> Int
  readByteOff :: MutableByteArray s -> Int -> Prim s a
  writeByteOff :: MutableByteArray s -> Int -> a -> Prim s ()

  default plusByteSize :: (Generic a, GByteArraySlice (Rep a)) => Int -> t a -> Int
  plusByteSize i = gplusByteSize i . reproxyRep
  {-# INLINE plusByteSize #-}

  default readByteOff :: ( Generic a
                         , GByteArraySlice (Rep a)
                         ) => MutableByteArray s -> Int -> Prim s a
  readByteOff array = fmap to . greadByteOff array
  {-# INLINE readByteOff #-}

  default writeByteOff :: ( Generic a
                          , GByteArraySlice (Rep a)
                          ) => MutableByteArray s -> Int -> a -> Prim s ()
  writeByteOff array i = gwriteByteOff array i . from
  {-# INLINE writeByteOff #-}

byteSizeOf :: ByteArraySlice a => a -> Int
byteSizeOf = plusByteSize 0 . proxy
{-# INLINE byteSizeOf #-}

class GByteArraySlice a where
  gplusByteSize :: Int -> t (a p) -> Int
  greadByteOff :: MutableByteArray s -> Int -> Prim s (a p)
  gwriteByteOff :: MutableByteArray s -> Int -> a p -> Prim s ()

instance GByteArraySlice U1 where
  gplusByteSize = const
  {-# INLINE gplusByteSize #-}
  greadByteOff _ _ = return U1
  {-# INLINE greadByteOff #-}
  gwriteByteOff _ _ _ = return ()
  {-# INLINE gwriteByteOff #-}

instance ByteArraySlice c => GByteArraySlice (K1 i c) where
  gplusByteSize i = plusByteSize i . reproxyK1
  {-# INLINE gplusByteSize #-}
  greadByteOff array = fmap K1 . readByteOff array
  {-# INLINE greadByteOff #-}
  gwriteByteOff array i = writeByteOff array i . unK1
  {-# INLINE gwriteByteOff #-}

instance GByteArraySlice f => GByteArraySlice (M1 i c f) where
  gplusByteSize i = gplusByteSize i . reproxyM1
  {-# INLINE gplusByteSize #-}
  greadByteOff array = fmap M1 . greadByteOff array
  {-# INLINE greadByteOff #-}
  gwriteByteOff array i = gwriteByteOff array i . unM1
  {-# INLINE gwriteByteOff #-}

instance (GByteArraySlice a, GByteArraySlice b) => GByteArraySlice (a :*: b) where
  gplusByteSize i a =
    gplusByteSize (gplusByteSize i (reproxyFst a)) (reproxySnd a)
  {-# INLINE gplusByteSize #-}
  greadByteOff array i = do
    a <- greadByteOff array i
    b <- greadByteOff array (gplusByteSize i (proxy a))
    return $ a :*: b
  {-# INLINE greadByteOff #-}
  gwriteByteOff array i (a :*: b) = do
    gwriteByteOff array i a
    gwriteByteOff array (gplusByteSize i (proxy a)) b
  {-# INLINE gwriteByteOff #-}

instance ByteArraySlice () where
  {-# INLINE plusByteSize #-}
  {-# INLINE readByteOff #-}
  {-# INLINE writeByteOff #-}
instance (ByteArraySlice a, ByteArraySlice b) => ByteArraySlice (a, b) where
  {-# INLINE plusByteSize #-}
  {-# INLINE readByteOff #-}
  {-# INLINE writeByteOff #-}
instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         ) => ByteArraySlice (a, b, c) where
  {-# INLINE plusByteSize #-}
  {-# INLINE readByteOff #-}
  {-# INLINE writeByteOff #-}
instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         , ByteArraySlice d
         ) => ByteArraySlice (a, b, c, d) where
  {-# INLINE plusByteSize #-}
  {-# INLINE readByteOff #-}
  {-# INLINE writeByteOff #-}
instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         , ByteArraySlice d
         , ByteArraySlice e
         ) => ByteArraySlice (a, b, c, d, e) where
  {-# INLINE plusByteSize #-}
  {-# INLINE readByteOff #-}
  {-# INLINE writeByteOff #-}
instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         , ByteArraySlice d
         , ByteArraySlice e
         , ByteArraySlice f
         ) => ByteArraySlice (a, b, c, d, e, f) where
  {-# INLINE plusByteSize #-}
  {-# INLINE readByteOff #-}
  {-# INLINE writeByteOff #-}
instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         , ByteArraySlice d
         , ByteArraySlice e
         , ByteArraySlice f
         , ByteArraySlice g
         ) => ByteArraySlice (a, b, c, d, e, f, g) where
  {-# INLINE plusByteSize #-}
  {-# INLINE readByteOff #-}
  {-# INLINE writeByteOff #-}

instance ByteArraySlice Bool where
  plusByteSize = plusByteSizeDefault
  {-# INLINE plusByteSize #-}
  readByteOff = readByteOffDefault
  {-# INLINE readByteOff #-}
  writeByteOff = writeByteOffDefault
  {-# INLINE writeByteOff #-}

instance ByteArraySlice Char where
  plusByteSize = plusByteSizeDefault
  {-# INLINE plusByteSize #-}
  readByteOff = readByteOffDefault
  {-# INLINE readByteOff #-}
  writeByteOff = writeByteOffDefault
  {-# INLINE writeByteOff #-}

instance ByteArraySlice Int where
  plusByteSize = plusByteSizeDefault
  {-# INLINE plusByteSize #-}
  readByteOff = readByteOffDefault
  {-# INLINE readByteOff #-}
  writeByteOff = writeByteOffDefault
  {-# INLINE writeByteOff #-}

instance ByteArraySlice Word where
  plusByteSize = plusByteSizeDefault
  {-# INLINE plusByteSize #-}
  readByteOff = readByteOffDefault
  {-# INLINE readByteOff #-}
  writeByteOff = writeByteOffDefault
  {-# INLINE writeByteOff #-}

instance ByteArraySlice Float where
  plusByteSize = plusByteSizeDefault
  {-# INLINE plusByteSize #-}
  readByteOff = readByteOffDefault
  {-# INLINE readByteOff #-}
  writeByteOff = writeByteOffDefault
  {-# INLINE writeByteOff #-}

instance ByteArraySlice Double where
  plusByteSize = plusByteSizeDefault
  {-# INLINE plusByteSize #-}
  readByteOff = readByteOffDefault
  {-# INLINE readByteOff #-}
  writeByteOff = writeByteOffDefault
  {-# INLINE writeByteOff #-}

instance ByteArraySlice Int8 where
  plusByteSize = plusByteSizeDefault
  {-# INLINE plusByteSize #-}
  readByteOff = readByteOffDefault
  {-# INLINE readByteOff #-}
  writeByteOff = writeByteOffDefault
  {-# INLINE writeByteOff #-}

instance ByteArraySlice Int16 where
  plusByteSize = plusByteSizeDefault
  {-# INLINE plusByteSize #-}
  readByteOff = readByteOffDefault
  {-# INLINE readByteOff #-}
  writeByteOff = writeByteOffDefault
  {-# INLINE writeByteOff #-}

instance ByteArraySlice Int32 where
  plusByteSize = plusByteSizeDefault
  {-# INLINE plusByteSize #-}
  readByteOff = readByteOffDefault
  {-# INLINE readByteOff #-}
  writeByteOff = writeByteOffDefault
  {-# INLINE writeByteOff #-}

instance ByteArraySlice Int64 where
  plusByteSize = plusByteSizeDefault
  {-# INLINE plusByteSize #-}
  readByteOff = readByteOffDefault
  {-# INLINE readByteOff #-}
  writeByteOff = writeByteOffDefault
  {-# INLINE writeByteOff #-}

instance ByteArraySlice Word8 where
  plusByteSize = plusByteSizeDefault
  {-# INLINE plusByteSize #-}
  readByteOff = readByteOffDefault
  {-# INLINE readByteOff #-}
  writeByteOff = writeByteOffDefault
  {-# INLINE writeByteOff #-}

instance ByteArraySlice Word16 where
  plusByteSize = plusByteSizeDefault
  {-# INLINE plusByteSize #-}
  readByteOff = readByteOffDefault
  {-# INLINE readByteOff #-}
  writeByteOff = writeByteOffDefault
  {-# INLINE writeByteOff #-}

instance ByteArraySlice Word32 where
  plusByteSize = plusByteSizeDefault
  {-# INLINE plusByteSize #-}
  readByteOff = readByteOffDefault
  {-# INLINE readByteOff #-}
  writeByteOff = writeByteOffDefault
  {-# INLINE writeByteOff #-}

instance ByteArraySlice Word64 where
  plusByteSize = plusByteSizeDefault
  {-# INLINE plusByteSize #-}
  readByteOff = readByteOffDefault
  {-# INLINE readByteOff #-}
  writeByteOff = writeByteOffDefault
  {-# INLINE writeByteOff #-}

instance ByteArraySlice (StablePtr a) where
  plusByteSize = plusByteSizeDefault
  {-# INLINE plusByteSize #-}
  readByteOff = readByteOffDefault
  {-# INLINE readByteOff #-}
  writeByteOff = writeByteOffDefault
  {-# INLINE writeByteOff #-}

instance ByteArraySlice (FunPtr a) where
  plusByteSize = plusByteSizeDefault
  {-# INLINE plusByteSize #-}
  readByteOff = readByteOffDefault
  {-# INLINE readByteOff #-}
  writeByteOff = writeByteOffDefault
  {-# INLINE writeByteOff #-}

instance ByteArraySlice (Ptr a) where
  plusByteSize = plusByteSizeDefault
  {-# INLINE plusByteSize #-}
  readByteOff = readByteOffDefault
  {-# INLINE readByteOff #-}
  writeByteOff = writeByteOffDefault
  {-# INLINE writeByteOff #-}

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
    m = readElemOff array $! case i `quotRem'` byteSize' of
      (q, 0) -> q
      (q, _) -> q + 1
    byteSize' = byteSize m
{-# INLINE readByteOffDefault #-}

writeByteOffDefault :: ByteArrayElem a => MutableByteArray s -> Int -> a -> Prim s ()
writeByteOffDefault array i a = i' `seq` writeElemOff array i' a
  where
    i' = case i `quotRem'` byteSize (proxy a) of
      (q, 0) -> q
      (q, _) -> q + 1
{-# INLINE writeByteOffDefault #-}

quotRem' :: Integral a => a -> a -> (a, a)
quotRem' x y = (x `quot` y, x `rem` y)
{-# INLINE quotRem' #-}
