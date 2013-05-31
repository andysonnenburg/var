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
       , plusByteSizeDefault
       , readByteOffDefault
       , writeByteOffDefault
       , byteSizeOf
       ) where

import Control.Monad.Prim

import qualified Data.ByteArrayElem.Unsafe as ByteArrayElem
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
  plusByteSize = plusByteSizeDefault
  {-# INLINE plusByteSize #-}

  default readByteOff :: ( Generic a
                         , GByteArraySlice (Rep a)
                         ) => MutableByteArray s -> Int -> Prim s a
  readByteOff = readByteOffDefault
  {-# INLINE readByteOff #-}

  default writeByteOff :: ( Generic a
                          , GByteArraySlice (Rep a)
                          ) => MutableByteArray s -> Int -> a -> Prim s ()
  writeByteOff = writeByteOffDefault
  {-# INLINE writeByteOff #-}

plusByteSizeDefault :: (Generic a, GByteArraySlice (Rep a)) => Int -> t a -> Int
plusByteSizeDefault i = gplusByteSize i . reproxyRep
{-# INLINE plusByteSizeDefault #-}

readByteOffDefault :: ( Generic a
                      , GByteArraySlice (Rep a)
                      ) => MutableByteArray s -> Int -> Prim s a
readByteOffDefault array = fmap to . greadByteOff array
{-# INLINE readByteOffDefault #-}

writeByteOffDefault :: ( Generic a
                       , GByteArraySlice (Rep a)
                       ) => MutableByteArray s -> Int -> a -> Prim s ()
writeByteOffDefault array i = gwriteByteOff array i . from
{-# INLINE writeByteOffDefault #-}

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
  plusByteSize = plusByteSizeDefault
  {-# INLINE plusByteSize #-}
  readByteOff = readByteOffDefault
  {-# INLINE readByteOff #-}
  writeByteOff = writeByteOffDefault
  {-# INLINE writeByteOff #-}

instance (ByteArraySlice a, ByteArraySlice b) => ByteArraySlice (a, b) where
  plusByteSize = plusByteSizeDefault
  {-# INLINE plusByteSize #-}
  readByteOff = readByteOffDefault
  {-# INLINE readByteOff #-}
  writeByteOff = writeByteOffDefault
  {-# INLINE writeByteOff #-}

instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         ) => ByteArraySlice (a, b, c) where
  plusByteSize = plusByteSizeDefault
  {-# INLINE plusByteSize #-}
  readByteOff = readByteOffDefault
  {-# INLINE readByteOff #-}
  writeByteOff = writeByteOffDefault
  {-# INLINE writeByteOff #-}

instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         , ByteArraySlice d
         ) => ByteArraySlice (a, b, c, d) where
  plusByteSize = plusByteSizeDefault
  {-# INLINE plusByteSize #-}
  readByteOff = readByteOffDefault
  {-# INLINE readByteOff #-}
  writeByteOff = writeByteOffDefault
  {-# INLINE writeByteOff #-}

instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         , ByteArraySlice d
         , ByteArraySlice e
         ) => ByteArraySlice (a, b, c, d, e) where
  plusByteSize = plusByteSizeDefault
  {-# INLINE plusByteSize #-}
  readByteOff = readByteOffDefault
  {-# INLINE readByteOff #-}
  writeByteOff = writeByteOffDefault
  {-# INLINE writeByteOff #-}

instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         , ByteArraySlice d
         , ByteArraySlice e
         , ByteArraySlice f
         ) => ByteArraySlice (a, b, c, d, e, f) where
  plusByteSize = plusByteSizeDefault
  {-# INLINE plusByteSize #-}
  readByteOff = readByteOffDefault
  {-# INLINE readByteOff #-}
  writeByteOff = writeByteOffDefault
  {-# INLINE writeByteOff #-}

instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         , ByteArraySlice d
         , ByteArraySlice e
         , ByteArraySlice f
         , ByteArraySlice g
         ) => ByteArraySlice (a, b, c, d, e, f, g) where
  plusByteSize = plusByteSizeDefault
  {-# INLINE plusByteSize #-}
  readByteOff = readByteOffDefault
  {-# INLINE readByteOff #-}
  writeByteOff = writeByteOffDefault
  {-# INLINE writeByteOff #-}

instance ByteArraySlice Bool where
  plusByteSize = ByteArrayElem.plusByteSizeDefault
  {-# INLINE plusByteSize #-}
  readByteOff = ByteArrayElem.readByteOffDefault
  {-# INLINE readByteOff #-}
  writeByteOff = ByteArrayElem.writeByteOffDefault
  {-# INLINE writeByteOff #-}

instance ByteArraySlice Char where
  plusByteSize = ByteArrayElem.plusByteSizeDefault
  {-# INLINE plusByteSize #-}
  readByteOff = ByteArrayElem.readByteOffDefault
  {-# INLINE readByteOff #-}
  writeByteOff = ByteArrayElem.writeByteOffDefault
  {-# INLINE writeByteOff #-}

instance ByteArraySlice Int where
  plusByteSize = ByteArrayElem.plusByteSizeDefault
  {-# INLINE plusByteSize #-}
  readByteOff = ByteArrayElem.readByteOffDefault
  {-# INLINE readByteOff #-}
  writeByteOff = ByteArrayElem.writeByteOffDefault
  {-# INLINE writeByteOff #-}

instance ByteArraySlice Word where
  plusByteSize = ByteArrayElem.plusByteSizeDefault
  {-# INLINE plusByteSize #-}
  readByteOff = ByteArrayElem.readByteOffDefault
  {-# INLINE readByteOff #-}
  writeByteOff = ByteArrayElem.writeByteOffDefault
  {-# INLINE writeByteOff #-}

instance ByteArraySlice Float where
  plusByteSize = ByteArrayElem.plusByteSizeDefault
  {-# INLINE plusByteSize #-}
  readByteOff = ByteArrayElem.readByteOffDefault
  {-# INLINE readByteOff #-}
  writeByteOff = ByteArrayElem.writeByteOffDefault
  {-# INLINE writeByteOff #-}

instance ByteArraySlice Double where
  plusByteSize = ByteArrayElem.plusByteSizeDefault
  {-# INLINE plusByteSize #-}
  readByteOff = ByteArrayElem.readByteOffDefault
  {-# INLINE readByteOff #-}
  writeByteOff = ByteArrayElem.writeByteOffDefault
  {-# INLINE writeByteOff #-}

instance ByteArraySlice Int8 where
  plusByteSize = ByteArrayElem.plusByteSizeDefault
  {-# INLINE plusByteSize #-}
  readByteOff = ByteArrayElem.readByteOffDefault
  {-# INLINE readByteOff #-}
  writeByteOff = ByteArrayElem.writeByteOffDefault
  {-# INLINE writeByteOff #-}

instance ByteArraySlice Int16 where
  plusByteSize = ByteArrayElem.plusByteSizeDefault
  {-# INLINE plusByteSize #-}
  readByteOff = ByteArrayElem.readByteOffDefault
  {-# INLINE readByteOff #-}
  writeByteOff = ByteArrayElem.writeByteOffDefault
  {-# INLINE writeByteOff #-}

instance ByteArraySlice Int32 where
  plusByteSize = ByteArrayElem.plusByteSizeDefault
  {-# INLINE plusByteSize #-}
  readByteOff = ByteArrayElem.readByteOffDefault
  {-# INLINE readByteOff #-}
  writeByteOff = ByteArrayElem.writeByteOffDefault
  {-# INLINE writeByteOff #-}

instance ByteArraySlice Int64 where
  plusByteSize = ByteArrayElem.plusByteSizeDefault
  {-# INLINE plusByteSize #-}
  readByteOff = ByteArrayElem.readByteOffDefault
  {-# INLINE readByteOff #-}
  writeByteOff = ByteArrayElem.writeByteOffDefault
  {-# INLINE writeByteOff #-}

instance ByteArraySlice Word8 where
  plusByteSize = ByteArrayElem.plusByteSizeDefault
  {-# INLINE plusByteSize #-}
  readByteOff = ByteArrayElem.readByteOffDefault
  {-# INLINE readByteOff #-}
  writeByteOff = ByteArrayElem.writeByteOffDefault
  {-# INLINE writeByteOff #-}

instance ByteArraySlice Word16 where
  plusByteSize = ByteArrayElem.plusByteSizeDefault
  {-# INLINE plusByteSize #-}
  readByteOff = ByteArrayElem.readByteOffDefault
  {-# INLINE readByteOff #-}
  writeByteOff = ByteArrayElem.writeByteOffDefault
  {-# INLINE writeByteOff #-}

instance ByteArraySlice Word32 where
  plusByteSize = ByteArrayElem.plusByteSizeDefault
  {-# INLINE plusByteSize #-}
  readByteOff = ByteArrayElem.readByteOffDefault
  {-# INLINE readByteOff #-}
  writeByteOff = ByteArrayElem.writeByteOffDefault
  {-# INLINE writeByteOff #-}

instance ByteArraySlice Word64 where
  plusByteSize = ByteArrayElem.plusByteSizeDefault
  {-# INLINE plusByteSize #-}
  readByteOff = ByteArrayElem.readByteOffDefault
  {-# INLINE readByteOff #-}
  writeByteOff = ByteArrayElem.writeByteOffDefault
  {-# INLINE writeByteOff #-}

instance ByteArraySlice (StablePtr a) where
  plusByteSize = ByteArrayElem.plusByteSizeDefault
  {-# INLINE plusByteSize #-}
  readByteOff = ByteArrayElem.readByteOffDefault
  {-# INLINE readByteOff #-}
  writeByteOff = ByteArrayElem.writeByteOffDefault
  {-# INLINE writeByteOff #-}

instance ByteArraySlice (FunPtr a) where
  plusByteSize = ByteArrayElem.plusByteSizeDefault
  {-# INLINE plusByteSize #-}
  readByteOff = ByteArrayElem.readByteOffDefault
  {-# INLINE readByteOff #-}
  writeByteOff = ByteArrayElem.writeByteOffDefault
  {-# INLINE writeByteOff #-}

instance ByteArraySlice (Ptr a) where
  plusByteSize = ByteArrayElem.plusByteSizeDefault
  {-# INLINE plusByteSize #-}
  readByteOff = ByteArrayElem.readByteOffDefault
  {-# INLINE readByteOff #-}
  writeByteOff = ByteArrayElem.writeByteOffDefault
  {-# INLINE writeByteOff #-}
