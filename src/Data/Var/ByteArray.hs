{-# LANGUAGE
    CPP
  , DefaultSignatures
  , DeriveDataTypeable
  , FlexibleContexts
  , FlexibleInstances
  , MagicHash
  , MultiParamTypeClasses #-}
#ifdef LANGUAGE_Trustworthy
{-# LANGUAGE Trustworthy #-}
#endif
{-# LANGUAGE TypeFamilies, TypeOperators, UnboxedTuples #-}
{- |
Copyright   :  (c) Andy Sonnenburg 2013
License     :  BSD3
Maintainer  :  andy22286@gmail.com
-}
module Data.Var.ByteArray
       ( ByteArrayVar
       , ByteArrayElem
       ) where

import Control.Monad.Prim.Class

import Data.Proxy
import Data.Var.Class
import Data.Typeable (Typeable)

import GHC.Exts
import GHC.Generics
import GHC.Int
import GHC.Stable (StablePtr (StablePtr))
import GHC.Word

#include "MachDeps.h"

data ByteArrayVar s a = ByteArrayVar (MutableByteArray# s) deriving Typeable

instance Eq (ByteArrayVar s a) where
  ByteArrayVar a == ByteArrayVar b = sameMutableByteArray# a b

instance (ByteArrayElem a, MonadPrim m, s ~ World m) => Var (ByteArrayVar s) a m where
  newVar a = liftPrim $ \ s -> case newByteArray# (sizeOf# a) s of
    (# s', array #) -> case writeByteArray# array 0# a s' of
      s'' -> (# s'', ByteArrayVar array #)
  {-# INLINE newVar #-}
  readVar (ByteArrayVar array) = liftPrim $ readByteArray# array 0#
  {-# INLINE readVar #-}
  writeVar (ByteArrayVar array) a = liftPrim $ \ s ->
    case writeByteArray# array 0# a s of
      s' -> (# s', () #)
  {-# INLINE writeVar #-}

class ByteArrayElem a where
  size# :: t a -> Int#
  readByteArray# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, a #)
  writeByteArray# :: MutableByteArray# s -> Int# -> a -> State# s -> State# s

  default size# :: (Generic a, GByteArrayElem (Rep a)) => t a -> Int#
  size# a = gsize# (reproxyRep a)
  {-# INLINE size# #-}

  default readByteArray# :: ( Generic a
                            , GByteArrayElem (Rep a)
                            ) =>
                            MutableByteArray# s -> Int# -> State# s -> (# State# s, a #)
  readByteArray# array i s = case greadByteArray# array i s of
    (# s', a #) -> (# s', to a #)
  {-# INLINE readByteArray# #-}

  default writeByteArray# :: ( Generic a
                             , GByteArrayElem (Rep a)
                             ) =>
                             MutableByteArray# s -> Int# -> a -> State# s -> State# s
  writeByteArray# array i a = gwriteByteArray# array i (from a)
  {-# INLINE writeByteArray# #-}

sizeOf# :: ByteArrayElem a => a -> Int#
sizeOf# a = size# (proxy a)
{-# INLINE sizeOf# #-}

class GByteArrayElem a where
  gsize# :: t (a p) -> Int#
  greadByteArray# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, a p #)
  gwriteByteArray# :: MutableByteArray# s -> Int# -> a p -> State# s -> State# s

instance GByteArrayElem U1 where
  gsize# _ = 0#
  greadByteArray# _ _ s = (# s, U1 #)
  gwriteByteArray# _ _ _ s = s

instance ByteArrayElem c => GByteArrayElem (K1 i c) where
  gsize# a = size# (reproxyK1 a)
  greadByteArray# array i s = case readByteArray# array i s of
    (# s', a #) -> (# s', K1 a #)
  gwriteByteArray# array i = writeByteArray# array i . unK1

instance GByteArrayElem f => GByteArrayElem (M1 i c f) where
  gsize# a = gsize# (reproxyM1 a)
  greadByteArray# array i s = case greadByteArray# array i s of
    (# s', a #) -> (# s', M1 a #)
  gwriteByteArray# array i = gwriteByteArray# array i . unM1

instance (GByteArrayElem a, GByteArrayElem b) => GByteArrayElem (a :*: b) where
  gsize# a = gsize# (reproxyFst a) +# gsize# (reproxySnd a)
  greadByteArray# array i s = case greadByteArray# array i s of
    (# s', a #) -> case greadByteArray# array (i +# gsizeOf# a) s' of
      (# s'', b #) -> (# s'', a :*: b #)
  gwriteByteArray# array i (a :*: b) s = case gwriteByteArray# array i a s of
    s' -> gwriteByteArray# array (i +# gsizeOf# a) b s'

reproxyK1 :: t (K1 i c p) -> Proxy c
reproxyK1 _ = Proxy

gsizeOf# :: GByteArrayElem a => a p -> Int#
gsizeOf# a = gsize# (proxy a)
{-# INLINE gsizeOf# #-}

instance ByteArrayElem ()
instance (ByteArrayElem a, ByteArrayElem b) => ByteArrayElem (a, b)
instance ( ByteArrayElem a
         , ByteArrayElem b
         , ByteArrayElem c
         ) => ByteArrayElem (a, b, c)
instance ( ByteArrayElem a
         , ByteArrayElem b
         , ByteArrayElem c
         , ByteArrayElem d
         ) => ByteArrayElem (a, b, c, d)
instance ( ByteArrayElem a
         , ByteArrayElem b
         , ByteArrayElem c
         , ByteArrayElem d
         , ByteArrayElem e
         ) => ByteArrayElem (a, b, c, d, e)
instance ( ByteArrayElem a
         , ByteArrayElem b
         , ByteArrayElem c
         , ByteArrayElem d
         , ByteArrayElem e
         , ByteArrayElem f
         ) => ByteArrayElem (a, b, c, d, e, f)
instance ( ByteArrayElem a
         , ByteArrayElem b
         , ByteArrayElem c
         , ByteArrayElem d
         , ByteArrayElem e
         , ByteArrayElem f
         , ByteArrayElem g
         ) => ByteArrayElem (a, b, c, d, e, f, g)

instance ByteArrayElem Bool where
  size# _ = 1#
  {-# INLINE size# #-}
  readByteArray# array i s = case readInt8Array# array i s of
    (# s', a #) -> (# s', a /=# 0# #)
  {-# INLINE readByteArray# #-}
  writeByteArray# array i e
    | e = writeInt8Array# array i 1#
    | otherwise = writeInt8Array# array i 0#
  {-# INLINE writeByteArray# #-}

instance ByteArrayElem Char where
  size# _ = SIZEOF_HSCHAR#
  {-# INLINE size# #-}
  readByteArray# array i s = case readWideCharArray# array i s of
    (# s', a #) -> (# s', C# a #)
  {-# INLINE readByteArray# #-}
  writeByteArray# array i (C# e) = writeWideCharArray# array i e
  {-# INLINE writeByteArray# #-}

instance ByteArrayElem Int where
  size# _ = SIZEOF_HSINT#
  {-# INLINE size# #-}
  readByteArray# array i s = case readIntArray# array i s of
    (# s', a #) -> (# s', I# a #)
  {-# INLINE readByteArray# #-}
  writeByteArray# array i (I# e) = writeIntArray# array i e
  {-# INLINE writeByteArray# #-}

instance ByteArrayElem Word where
  size# _ = SIZEOF_HSWORD#
  {-# INLINE size# #-}
  readByteArray# array i s = case readWordArray# array i s of
    (# s', a #) -> (# s', W# a #)
  {-# INLINE readByteArray# #-}
  writeByteArray# array i (W# e) = writeWordArray# array i e
  {-# INLINE writeByteArray# #-}

instance ByteArrayElem (Ptr a) where
  size# _ = SIZEOF_HSWORD#
  {-# INLINE size# #-}
  readByteArray# array i s = case readAddrArray# array i s of
    (# s', a #) -> (# s', Ptr a #)
  {-# INLINE readByteArray# #-}
  writeByteArray# array i (Ptr e) = writeAddrArray# array i e
  {-# INLINE writeByteArray# #-}

instance ByteArrayElem (FunPtr a) where
  size# _ = SIZEOF_HSWORD#
  {-# INLINE size# #-}
  readByteArray# array i s = case readAddrArray# array i s of
    (# s', a #) -> (# s', FunPtr a #)
  {-# INLINE readByteArray# #-}
  writeByteArray# array i (FunPtr e) = writeAddrArray# array i e
  {-# INLINE writeByteArray# #-}

instance ByteArrayElem Float where
  size# _ = SIZEOF_HSFLOAT#
  {-# INLINE size# #-}
  readByteArray# array i s = case readFloatArray# array i s of
    (# s', a #) -> (# s', F# a #)
  {-# INLINE readByteArray# #-}
  writeByteArray# array i (F# e) = writeFloatArray# array i e
  {-# INLINE writeByteArray# #-}

instance ByteArrayElem Double where
  size# _ = SIZEOF_HSDOUBLE#
  {-# INLINE size# #-}
  readByteArray# array i s = case readDoubleArray# array i s of
    (# s', a #) -> (# s', D# a #)
  {-# INLINE readByteArray# #-}
  writeByteArray# array i (D# e) = writeDoubleArray# array i e
  {-# INLINE writeByteArray# #-}

instance ByteArrayElem (StablePtr a) where
  size# _ = SIZEOF_HSWORD#
  {-# INLINE size# #-}
  readByteArray# array i s = case readStablePtrArray# array i s of
    (# s', a #) -> (# s', StablePtr a #)
  {-# INLINE readByteArray# #-}
  writeByteArray# array i (StablePtr e) = writeStablePtrArray# array i e
  {-# INLINE writeByteArray# #-}

instance ByteArrayElem Int8 where
  size# _ = 1#
  {-# INLINE size# #-}
  readByteArray# array i s = case readInt8Array# array i s of
    (# s', a #) -> (# s', I8# a #)
  {-# INLINE readByteArray# #-}
  writeByteArray# array i (I8# e) = writeInt8Array# array i e
  {-# INLINE writeByteArray# #-}

instance ByteArrayElem Int16 where
  size# _ = 2#
  {-# INLINE size# #-}
  readByteArray# array i s = case readInt16Array# array i s of
    (# s', a #) -> (# s', I16# a #)
  {-# INLINE readByteArray# #-}
  writeByteArray# array i (I16# e) = writeInt16Array# array i e
  {-# INLINE writeByteArray# #-}

instance ByteArrayElem Int32 where
  size# _ = 4#
  {-# INLINE size# #-}
  readByteArray# array i s = case readInt32Array# array i s of
    (# s', a #) -> (# s', I32# a #)
  {-# INLINE readByteArray# #-}
  writeByteArray# array i (I32# e) = writeInt32Array# array i e
  {-# INLINE writeByteArray# #-}

instance ByteArrayElem Int64 where
  size# _ = 8#
  {-# INLINE size# #-}
  readByteArray# array i s = case readInt64Array# array i s of
    (# s', a #) -> (# s', I64# a #)
  {-# INLINE readByteArray# #-}
  writeByteArray# array i (I64# e) = writeInt64Array# array i e
  {-# INLINE writeByteArray# #-}

instance ByteArrayElem Word8 where
  size# _ = 1#
  {-# INLINE size# #-}
  readByteArray# array i s = case readWord8Array# array i s of
    (# s', a #) -> (# s', W8# a #)
  {-# INLINE readByteArray# #-}
  writeByteArray# array i (W8# e) = writeWord8Array# array i e
  {-# INLINE writeByteArray# #-}

instance ByteArrayElem Word16 where
  size# _ = 2#
  {-# INLINE size# #-}
  readByteArray# array i s = case readWord16Array# array i s of
    (# s', a #) -> (# s', W16# a #)
  {-# INLINE readByteArray# #-}
  writeByteArray# array i (W16# e) = writeWord16Array# array i e
  {-# INLINE writeByteArray# #-}

instance ByteArrayElem Word32 where
  size# _ = 4#
  {-# INLINE size# #-}
  readByteArray# array i s = case readWord32Array# array i s of
    (# s', a #) -> (# s', W32# a #)
  {-# INLINE readByteArray# #-}
  writeByteArray# array i (W32# e) = writeWord32Array# array i e
  {-# INLINE writeByteArray# #-}

instance ByteArrayElem Word64 where
  size# _ = 8#
  {-# INLINE size# #-}
  readByteArray# array i s = case readWord64Array# array i s of
    (# s', a #) -> (# s', W64# a #)
  {-# INLINE readByteArray# #-}
  writeByteArray# array i (W64# e) = writeWord64Array# array i e
  {-# INLINE writeByteArray# #-}
