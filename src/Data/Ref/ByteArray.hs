{-# LANGUAGE
    CPP
  , DeriveDataTypeable
  , FlexibleInstances
  , MagicHash
  , MultiParamTypeClasses
  , Trustworthy
  , TypeFamilies
  , UnboxedTuples #-}
module Data.Ref.ByteArray
       ( ByteArrayRef
       ) where

import Data.Ref.Class
import Data.Typeable

import GHC.Exts
import GHC.Int
import GHC.IO (IO (IO))
import GHC.ST (ST (ST))
import GHC.Stable (StablePtr (StablePtr))
import GHC.Word

#include "MachDeps.h"

data ByteArrayRef s a = ByteArrayRef (MutableByteArray# s) deriving Typeable

instance Eq (ByteArrayRef s a) where
  ByteArrayRef a == ByteArrayRef b = sameMutableByteArray# a b

instance (ByteArrayElem a, MonadPrim m, s ~ World m) => Ref (ByteArrayRef s) a m where
  newRef a = liftPrim $ \ s -> case newByteArray# (sizeOf# a) s of
    (# s', array #) -> case writeByteArray# array 0# a s' of
      s'' -> (# s'', ByteArrayRef array #)
  {-# INLINE newRef #-}
  readRef (ByteArrayRef array) = liftPrim $ readByteArray# array 0#
  {-# INLINE readRef #-}
  writeRef (ByteArrayRef array) a = liftPrim $ \ s ->
    case writeByteArray# array 0# a s of
      s' -> (# s', () #)
  {-# INLINE writeRef #-}

class ByteArrayElem a where
  sizeOf# :: a -> Int#
  readByteArray# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, a #)
  writeByteArray# :: MutableByteArray# s -> Int# -> a -> State# s -> State# s

instance ByteArrayElem Char where
  sizeOf# _ = SIZEOF_HSCHAR#
  {-# INLINE sizeOf# #-}
  readByteArray# array i s = case readWideCharArray# array i s of
    (# s', a #) -> (# s', C# a #)
  {-# INLINE readByteArray# #-}
  writeByteArray# array i (C# e) = writeWideCharArray# array i e
  {-# INLINE writeByteArray# #-}

instance ByteArrayElem Int where
  sizeOf# _ = SIZEOF_HSINT#
  {-# INLINE sizeOf# #-}
  readByteArray# array i s = case readIntArray# array i s of
    (# s', a #) -> (# s', I# a #)
  {-# INLINE readByteArray# #-}
  writeByteArray# array i (I# e) = writeIntArray# array i e
  {-# INLINE writeByteArray# #-}

instance ByteArrayElem Word where
  sizeOf# _ = SIZEOF_HSWORD#
  {-# INLINE sizeOf# #-}
  readByteArray# array i s = case readWordArray# array i s of
    (# s', a #) -> (# s', W# a #)
  {-# INLINE readByteArray# #-}
  writeByteArray# array i (W# e) = writeWordArray# array i e
  {-# INLINE writeByteArray# #-}

instance ByteArrayElem (Ptr a) where
  sizeOf# _ = SIZEOF_HSWORD#
  {-# INLINE sizeOf# #-}
  readByteArray# array i s = case readAddrArray# array i s of
    (# s', a #) -> (# s', Ptr a #)
  {-# INLINE readByteArray# #-}
  writeByteArray# array i (Ptr e) = writeAddrArray# array i e
  {-# INLINE writeByteArray# #-}

instance ByteArrayElem (FunPtr a) where
  sizeOf# _ = SIZEOF_HSWORD#
  {-# INLINE sizeOf# #-}
  readByteArray# array i s = case readAddrArray# array i s of
    (# s', a #) -> (# s', FunPtr a #)
  {-# INLINE readByteArray# #-}
  writeByteArray# array i (FunPtr e) = writeAddrArray# array i e
  {-# INLINE writeByteArray# #-}

instance ByteArrayElem Float where
  sizeOf# _ = SIZEOF_HSFLOAT#
  {-# INLINE sizeOf# #-}
  readByteArray# array i s = case readFloatArray# array i s of
    (# s', a #) -> (# s', F# a #)
  {-# INLINE readByteArray# #-}
  writeByteArray# array i (F# e) = writeFloatArray# array i e
  {-# INLINE writeByteArray# #-}

instance ByteArrayElem Double where
  sizeOf# _ = SIZEOF_HSDOUBLE#
  {-# INLINE sizeOf# #-}
  readByteArray# array i s = case readDoubleArray# array i s of
    (# s', a #) -> (# s', D# a #)
  {-# INLINE readByteArray# #-}
  writeByteArray# array i (D# e) = writeDoubleArray# array i e
  {-# INLINE writeByteArray# #-}

instance ByteArrayElem (StablePtr a) where
  sizeOf# _ = SIZEOF_HSWORD#
  {-# INLINE sizeOf# #-}
  readByteArray# array i s = case readStablePtrArray# array i s of
    (# s', a #) -> (# s', StablePtr a #)
  {-# INLINE readByteArray# #-}
  writeByteArray# array i (StablePtr e) = writeStablePtrArray# array i e
  {-# INLINE writeByteArray# #-}

instance ByteArrayElem Int8 where
  sizeOf# _ = 1#
  {-# INLINE sizeOf# #-}
  readByteArray# array i s = case readInt8Array# array i s of
    (# s', a #) -> (# s', I8# a #)
  {-# INLINE readByteArray# #-}
  writeByteArray# array i (I8# e) = writeInt8Array# array i e
  {-# INLINE writeByteArray# #-}

instance ByteArrayElem Int16 where
  sizeOf# _ = 2#
  {-# INLINE sizeOf# #-}
  readByteArray# array i s = case readInt16Array# array i s of
    (# s', a #) -> (# s', I16# a #)
  {-# INLINE readByteArray# #-}
  writeByteArray# array i (I16# e) = writeInt16Array# array i e
  {-# INLINE writeByteArray# #-}

instance ByteArrayElem Int32 where
  sizeOf# _ = 4#
  {-# INLINE sizeOf# #-}
  readByteArray# array i s = case readInt32Array# array i s of
    (# s', a #) -> (# s', I32# a #)
  {-# INLINE readByteArray# #-}
  writeByteArray# array i (I32# e) = writeInt32Array# array i e
  {-# INLINE writeByteArray# #-}

instance ByteArrayElem Int64 where
  sizeOf# _ = 8#
  {-# INLINE sizeOf# #-}
  readByteArray# array i s = case readInt64Array# array i s of
    (# s', a #) -> (# s', I64# a #)
  {-# INLINE readByteArray# #-}
  writeByteArray# array i (I64# e) = writeInt64Array# array i e
  {-# INLINE writeByteArray# #-}

instance ByteArrayElem Word8 where
  sizeOf# _ = 1#
  {-# INLINE sizeOf# #-}
  readByteArray# array i s = case readWord8Array# array i s of
    (# s', a #) -> (# s', W8# a #)
  {-# INLINE readByteArray# #-}
  writeByteArray# array i (W8# e) = writeWord8Array# array i e
  {-# INLINE writeByteArray# #-}

instance ByteArrayElem Word16 where
  sizeOf# _ = 2#
  {-# INLINE sizeOf# #-}
  readByteArray# array i s = case readWord16Array# array i s of
    (# s', a #) -> (# s', W16# a #)
  {-# INLINE readByteArray# #-}
  writeByteArray# array i (W16# e) = writeWord16Array# array i e
  {-# INLINE writeByteArray# #-}

instance ByteArrayElem Word32 where
  sizeOf# _ = 4#
  {-# INLINE sizeOf# #-}
  readByteArray# array i s = case readWord32Array# array i s of
    (# s', a #) -> (# s', W32# a #)
  {-# INLINE readByteArray# #-}
  writeByteArray# array i (W32# e) = writeWord32Array# array i e
  {-# INLINE writeByteArray# #-}

instance ByteArrayElem Word64 where
  sizeOf# _ = 8#
  {-# INLINE sizeOf# #-}
  readByteArray# array i s = case readWord64Array# array i s of
    (# s', a #) -> (# s', W64# a #)
  {-# INLINE readByteArray# #-}
  writeByteArray# array i (W64# e) = writeWord64Array# array i e
  {-# INLINE writeByteArray# #-}

class Monad m => MonadPrim m where
  type World m
  liftPrim :: (State# (World m) -> (# State# (World m), a #)) -> m a

instance MonadPrim (ST s) where
  type World (ST s) = s
  liftPrim = ST

instance MonadPrim IO where
  type World IO = RealWorld
  liftPrim = IO
