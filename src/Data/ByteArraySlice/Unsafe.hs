{-# LANGUAGE
    CPP
  , DefaultSignatures
  , FlexibleContexts
  , MagicHash
  , TypeOperators
  , UnboxedTuples #-}
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
       , byteSizeOf#
       ) where

import Data.Proxy

import GHC.Exts
import GHC.Generics
import GHC.Int
import GHC.Stable (StablePtr (StablePtr))
import GHC.Word

#include "MachDeps.h"

class ByteArraySlice a where
  byteSize# :: t a -> Int#
  readByteArray# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, a #)
  writeByteArray# :: MutableByteArray# s -> Int# -> a -> State# s -> State# s

  default byteSize# :: (Generic a, GByteArraySlice (Rep a)) => t a -> Int#
  byteSize# a = gbyteSize# (reproxyRep a)
  {-# INLINE byteSize# #-}

  default readByteArray# :: ( Generic a
                            , GByteArraySlice (Rep a)
                            ) =>
                            MutableByteArray# s -> Int# -> State# s -> (# State# s, a #)
  readByteArray# array i s = case greadByteArray# array i s of
    (# s', a #) -> (# s', to a #)
  {-# INLINE readByteArray# #-}

  default writeByteArray# :: ( Generic a
                             , GByteArraySlice (Rep a)
                             ) =>
                             MutableByteArray# s -> Int# -> a -> State# s -> State# s
  writeByteArray# array i a = gwriteByteArray# array i (from a)
  {-# INLINE writeByteArray# #-}

byteSizeOf# :: ByteArraySlice a => a -> Int#
byteSizeOf# a = byteSize# (proxy a)
{-# INLINE byteSizeOf# #-}

class GByteArraySlice a where
  gbyteSize# :: t (a p) -> Int#
  greadByteArray# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, a p #)
  gwriteByteArray# :: MutableByteArray# s -> Int# -> a p -> State# s -> State# s

instance GByteArraySlice U1 where
  gbyteSize# _ = 0#
  greadByteArray# _ _ s = (# s, U1 #)
  gwriteByteArray# _ _ _ s = s

instance ByteArraySlice c => GByteArraySlice (K1 i c) where
  gbyteSize# a = byteSize# (reproxyK1 a)
  greadByteArray# array i s = case readByteArray# array i s of
    (# s', a #) -> (# s', K1 a #)
  gwriteByteArray# array i = writeByteArray# array i . unK1

instance GByteArraySlice f => GByteArraySlice (M1 i c f) where
  gbyteSize# a = gbyteSize# (reproxyM1 a)
  greadByteArray# array i s = case greadByteArray# array i s of
    (# s', a #) -> (# s', M1 a #)
  gwriteByteArray# array i = gwriteByteArray# array i . unM1

instance (GByteArraySlice a, GByteArraySlice b) => GByteArraySlice (a :*: b) where
  gbyteSize# a = gbyteSize# (reproxyFst a) +# gbyteSize# (reproxySnd a)
  greadByteArray# array i s = case greadByteArray# array i s of
    (# s', a #) -> case greadByteArray# array (i +# gsizeOf# a) s' of
      (# s'', b #) -> (# s'', a :*: b #)
  gwriteByteArray# array i (a :*: b) s = case gwriteByteArray# array i a s of
    s' -> gwriteByteArray# array (i +# gsizeOf# a) b s'

gsizeOf# :: GByteArraySlice a => a p -> Int#
gsizeOf# a = gbyteSize# (proxy a)
{-# INLINE gsizeOf# #-}

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
  byteSize# _ = 1#
  {-# INLINE byteSize# #-}
  readByteArray# array i s = case readInt8Array# array i s of
    (# s', a #) -> (# s', a /=# 0# #)
  {-# INLINE readByteArray# #-}
  writeByteArray# array i e
    | e = writeInt8Array# array i 1#
    | otherwise = writeInt8Array# array i 0#
  {-# INLINE writeByteArray# #-}

instance ByteArraySlice Char where
  byteSize# _ = SIZEOF_HSCHAR#
  {-# INLINE byteSize# #-}
  readByteArray# array i s = case readWideCharArray# array i s of
    (# s', a #) -> (# s', C# a #)
  {-# INLINE readByteArray# #-}
  writeByteArray# array i (C# e) = writeWideCharArray# array i e
  {-# INLINE writeByteArray# #-}

instance ByteArraySlice Int where
  byteSize# _ = SIZEOF_HSINT#
  {-# INLINE byteSize# #-}
  readByteArray# array i s = case readIntArray# array i s of
    (# s', a #) -> (# s', I# a #)
  {-# INLINE readByteArray# #-}
  writeByteArray# array i (I# e) = writeIntArray# array i e
  {-# INLINE writeByteArray# #-}

instance ByteArraySlice Word where
  byteSize# _ = SIZEOF_HSWORD#
  {-# INLINE byteSize# #-}
  readByteArray# array i s = case readWordArray# array i s of
    (# s', a #) -> (# s', W# a #)
  {-# INLINE readByteArray# #-}
  writeByteArray# array i (W# e) = writeWordArray# array i e
  {-# INLINE writeByteArray# #-}

instance ByteArraySlice (Ptr a) where
  byteSize# _ = SIZEOF_HSWORD#
  {-# INLINE byteSize# #-}
  readByteArray# array i s = case readAddrArray# array i s of
    (# s', a #) -> (# s', Ptr a #)
  {-# INLINE readByteArray# #-}
  writeByteArray# array i (Ptr e) = writeAddrArray# array i e
  {-# INLINE writeByteArray# #-}

instance ByteArraySlice (FunPtr a) where
  byteSize# _ = SIZEOF_HSWORD#
  {-# INLINE byteSize# #-}
  readByteArray# array i s = case readAddrArray# array i s of
    (# s', a #) -> (# s', FunPtr a #)
  {-# INLINE readByteArray# #-}
  writeByteArray# array i (FunPtr e) = writeAddrArray# array i e
  {-# INLINE writeByteArray# #-}

instance ByteArraySlice Float where
  byteSize# _ = SIZEOF_HSFLOAT#
  {-# INLINE byteSize# #-}
  readByteArray# array i s = case readFloatArray# array i s of
    (# s', a #) -> (# s', F# a #)
  {-# INLINE readByteArray# #-}
  writeByteArray# array i (F# e) = writeFloatArray# array i e
  {-# INLINE writeByteArray# #-}

instance ByteArraySlice Double where
  byteSize# _ = SIZEOF_HSDOUBLE#
  {-# INLINE byteSize# #-}
  readByteArray# array i s = case readDoubleArray# array i s of
    (# s', a #) -> (# s', D# a #)
  {-# INLINE readByteArray# #-}
  writeByteArray# array i (D# e) = writeDoubleArray# array i e
  {-# INLINE writeByteArray# #-}

instance ByteArraySlice (StablePtr a) where
  byteSize# _ = SIZEOF_HSWORD#
  {-# INLINE byteSize# #-}
  readByteArray# array i s = case readStablePtrArray# array i s of
    (# s', a #) -> (# s', StablePtr a #)
  {-# INLINE readByteArray# #-}
  writeByteArray# array i (StablePtr e) = writeStablePtrArray# array i e
  {-# INLINE writeByteArray# #-}

instance ByteArraySlice Int8 where
  byteSize# _ = 1#
  {-# INLINE byteSize# #-}
  readByteArray# array i s = case readInt8Array# array i s of
    (# s', a #) -> (# s', I8# a #)
  {-# INLINE readByteArray# #-}
  writeByteArray# array i (I8# e) = writeInt8Array# array i e
  {-# INLINE writeByteArray# #-}

instance ByteArraySlice Int16 where
  byteSize# _ = 2#
  {-# INLINE byteSize# #-}
  readByteArray# array i s = case readInt16Array# array i s of
    (# s', a #) -> (# s', I16# a #)
  {-# INLINE readByteArray# #-}
  writeByteArray# array i (I16# e) = writeInt16Array# array i e
  {-# INLINE writeByteArray# #-}

instance ByteArraySlice Int32 where
  byteSize# _ = 4#
  {-# INLINE byteSize# #-}
  readByteArray# array i s = case readInt32Array# array i s of
    (# s', a #) -> (# s', I32# a #)
  {-# INLINE readByteArray# #-}
  writeByteArray# array i (I32# e) = writeInt32Array# array i e
  {-# INLINE writeByteArray# #-}

instance ByteArraySlice Int64 where
  byteSize# _ = 8#
  {-# INLINE byteSize# #-}
  readByteArray# array i s = case readInt64Array# array i s of
    (# s', a #) -> (# s', I64# a #)
  {-# INLINE readByteArray# #-}
  writeByteArray# array i (I64# e) = writeInt64Array# array i e
  {-# INLINE writeByteArray# #-}

instance ByteArraySlice Word8 where
  byteSize# _ = 1#
  {-# INLINE byteSize# #-}
  readByteArray# array i s = case readWord8Array# array i s of
    (# s', a #) -> (# s', W8# a #)
  {-# INLINE readByteArray# #-}
  writeByteArray# array i (W8# e) = writeWord8Array# array i e
  {-# INLINE writeByteArray# #-}

instance ByteArraySlice Word16 where
  byteSize# _ = 2#
  {-# INLINE byteSize# #-}
  readByteArray# array i s = case readWord16Array# array i s of
    (# s', a #) -> (# s', W16# a #)
  {-# INLINE readByteArray# #-}
  writeByteArray# array i (W16# e) = writeWord16Array# array i e
  {-# INLINE writeByteArray# #-}

instance ByteArraySlice Word32 where
  byteSize# _ = 4#
  {-# INLINE byteSize# #-}
  readByteArray# array i s = case readWord32Array# array i s of
    (# s', a #) -> (# s', W32# a #)
  {-# INLINE readByteArray# #-}
  writeByteArray# array i (W32# e) = writeWord32Array# array i e
  {-# INLINE writeByteArray# #-}

instance ByteArraySlice Word64 where
  byteSize# _ = 8#
  {-# INLINE byteSize# #-}
  readByteArray# array i s = case readWord64Array# array i s of
    (# s', a #) -> (# s', W64# a #)
  {-# INLINE readByteArray# #-}
  writeByteArray# array i (W64# e) = writeWord64Array# array i e
  {-# INLINE writeByteArray# #-}
