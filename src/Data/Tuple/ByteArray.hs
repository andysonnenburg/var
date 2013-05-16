{-# LANGUAGE CPP #-}
#ifdef LANGUAGE_DataKinds
{-# LANGUAGE DataKinds #-}
#endif
{-# LANGUAGE DefaultSignatures, DeriveDataTypeable #-}
#ifndef LANGUAGE_DataKinds
{-# LANGUAGE EmptyDataDecls #-}
#endif
{-# LANGUAGE
    FlexibleInstances
  , FlexibleContexts
  , MagicHash
  , MultiParamTypeClasses #-}
#ifdef LANGUAGE_Trustworthy
{-# LANGUAGE Trustworthy #-}
#endif
{-# LANGUAGE
    TypeFamilies
  , UnboxedTuples
  , UndecidableInstances #-}
{- |
Copyright   :  (c) Andy Sonnenburg 2013
License     :  BSD3
Maintainer  :  andy22286@gmail.com
-}
module Data.Tuple.ByteArray
       ( ByteArrayTuple
       ) where

import Control.Monad.Prim.Class

import Data.ByteArraySlice.Unsafe
import Data.Proxy
import Data.Tuple.Class
import Data.Tuple.MTuple
import Data.Typeable (Typeable)

import GHC.Exts

data ByteArrayTuple s a = ByteArrayTuple (MutableByteArray# s) deriving Typeable

instance Eq (ByteArrayTuple s a) where
  ByteArrayTuple a == ByteArrayTuple b = sameMutableByteArray# a b

instance ( MonadPrim m
         , s ~ World m
         , Tuple t
         , ByteArraySlice t
         ) => MTuple (ByteArrayTuple s) t m where
  thawTuple a = liftPrim $ \ s -> case newByteArray# (byteSizeOf# a) s of
    (# s', array #) -> case writeByteArray# array 0# a s' of
      s'' -> (# s'', ByteArrayTuple array #)
  freezeTuple (ByteArrayTuple array) = liftPrim $ readByteArray# array 0#

instance ( MonadPrim m
         , s ~ World m
         , Tuple t
         , ByteArraySlice t
         , a ~ Field1 t
         , ByteArraySlice a
         ) => MField1 (ByteArrayTuple s) t a m where
  read1 = unsafeRead offset1#
  write1 = unsafeWrite offset1#

instance ( MonadPrim m
         , s ~ World m
         , Tuple t
         , ByteArraySlice t
         , ByteArraySlice (Field1 t)
         , a ~ Field2 t
         , ByteArraySlice a
         ) => MField2 (ByteArrayTuple s) t a m where
  read2 = unsafeRead offset2#
  write2 = unsafeWrite offset2#

instance ( MonadPrim m
         , s ~ World m
         , Tuple t
         , ByteArraySlice t
         , ByteArraySlice (Field1 t)
         , ByteArraySlice (Field2 t)
         , a ~ Field3 t
         , ByteArraySlice a
         ) => MField3 (ByteArrayTuple s) t a m where
  read3 = unsafeRead offset3#
  write3 = unsafeWrite offset3#

instance ( MonadPrim m
         , s ~ World m
         , Tuple t
         , ByteArraySlice t
         , ByteArraySlice (Field1 t)
         , ByteArraySlice (Field2 t)
         , ByteArraySlice (Field3 t)
         , a ~ Field4 t
         , ByteArraySlice a
         ) => MField4 (ByteArrayTuple s) t a m where
  read4 = unsafeRead offset4#
  write4 = unsafeWrite offset4#

instance ( MonadPrim m
         , s ~ World m
         , Tuple t
         , ByteArraySlice t
         , ByteArraySlice (Field1 t)
         , ByteArraySlice (Field2 t)
         , ByteArraySlice (Field3 t)
         , ByteArraySlice (Field4 t)
         , a ~ Field5 t
         , ByteArraySlice a
         ) => MField5 (ByteArrayTuple s) t a m where
  read5 = unsafeRead offset5#
  write5 = unsafeWrite offset5#

instance ( MonadPrim m
         , s ~ World m
         , Tuple t
         , ByteArraySlice t
         , ByteArraySlice (Field1 t)
         , ByteArraySlice (Field2 t)
         , ByteArraySlice (Field3 t)
         , ByteArraySlice (Field4 t)
         , ByteArraySlice (Field5 t)
         , a ~ Field6 t
         , ByteArraySlice a
         ) => MField6 (ByteArrayTuple s) t a m where
  read6 = unsafeRead offset6#
  write6 = unsafeWrite offset6#

instance ( MonadPrim m
         , s ~ World m
         , Tuple t
         , ByteArraySlice t
         , ByteArraySlice (Field1 t)
         , ByteArraySlice (Field2 t)
         , ByteArraySlice (Field3 t)
         , ByteArraySlice (Field4 t)
         , ByteArraySlice (Field5 t)
         , ByteArraySlice (Field6 t)
         , a ~ Field7 t
         , ByteArraySlice a
         ) => MField7 (ByteArrayTuple s) t a m where
  read7 = unsafeRead offset7#
  write7 = unsafeWrite offset7#

instance ( MonadPrim m
         , s ~ World m
         , Tuple t
         , ByteArraySlice t
         , ByteArraySlice (Field1 t)
         , ByteArraySlice (Field2 t)
         , ByteArraySlice (Field3 t)
         , ByteArraySlice (Field4 t)
         , ByteArraySlice (Field5 t)
         , ByteArraySlice (Field6 t)
         , ByteArraySlice (Field7 t)
         , a ~ Field8 t
         , ByteArraySlice a
         ) => MField8 (ByteArrayTuple s) t a m where
  read8 = unsafeRead offset8#
  write8 = unsafeWrite offset8#

instance ( MonadPrim m
         , s ~ World m
         , Tuple t
         , ByteArraySlice t
         , ByteArraySlice (Field1 t)
         , ByteArraySlice (Field2 t)
         , ByteArraySlice (Field3 t)
         , ByteArraySlice (Field4 t)
         , ByteArraySlice (Field5 t)
         , ByteArraySlice (Field6 t)
         , ByteArraySlice (Field7 t)
         , ByteArraySlice (Field8 t)
         , a ~ Field9 t
         , ByteArraySlice a
         ) => MField9 (ByteArrayTuple s) t a m where
  read9 = unsafeRead offset9#
  write9 = unsafeWrite offset9#

offset1# :: t a -> Int#
offset1# _ = 0#

offset2# :: ByteArraySlice (Field1 a) => t a -> Int#
offset2# a = offset1# a +# byteSize# (reproxyField1 a)

offset3# :: ( ByteArraySlice (Field1 a)
            , ByteArraySlice (Field2 a)
            ) => t a -> Int#
offset3# a = offset2# a +# byteSize# (reproxyField2 a)

offset4# :: ( ByteArraySlice (Field1 a)
            , ByteArraySlice (Field2 a)
            , ByteArraySlice (Field3 a)
            ) => t a -> Int#
offset4# a = offset3# a +# byteSize# (reproxyField3 a)

offset5# :: ( ByteArraySlice (Field1 a)
            , ByteArraySlice (Field2 a)
            , ByteArraySlice (Field3 a)
            , ByteArraySlice (Field4 a)
            ) => t a -> Int#
offset5# a = offset4# a +# byteSize# (reproxyField4 a)

offset6# :: ( ByteArraySlice (Field1 a)
            , ByteArraySlice (Field2 a)
            , ByteArraySlice (Field3 a)
            , ByteArraySlice (Field4 a)
            , ByteArraySlice (Field5 a)
            ) => t a -> Int#
offset6# a = offset5# a +# byteSize# (reproxyField5 a)

offset7# :: ( ByteArraySlice (Field1 a)
            , ByteArraySlice (Field2 a)
            , ByteArraySlice (Field3 a)
            , ByteArraySlice (Field4 a)
            , ByteArraySlice (Field5 a)
            , ByteArraySlice (Field6 a)
            ) => t a -> Int#
offset7# a = offset6# a +# byteSize# (reproxyField6 a)

offset8# :: ( ByteArraySlice (Field1 a)
            , ByteArraySlice (Field2 a)
            , ByteArraySlice (Field3 a)
            , ByteArraySlice (Field4 a)
            , ByteArraySlice (Field5 a)
            , ByteArraySlice (Field6 a)
            , ByteArraySlice (Field7 a)
            ) => t a -> Int#
offset8# a = offset7# a +# byteSize# (reproxyField7 a)

offset9# :: ( ByteArraySlice (Field1 a)
            , ByteArraySlice (Field2 a)
            , ByteArraySlice (Field3 a)
            , ByteArraySlice (Field4 a)
            , ByteArraySlice (Field5 a)
            , ByteArraySlice (Field6 a)
            , ByteArraySlice (Field7 a)
            , ByteArraySlice (Field8 a)
            ) => t a -> Int#
offset9# a = offset8# a +# byteSize# (reproxyField8 a)

proxyTuple :: t a -> Proxy a
proxyTuple = reproxy

reproxyField1 :: t a -> Proxy (Field1 a)
reproxyField1 = reproxy

reproxyField2 :: t a -> Proxy (Field2 a)
reproxyField2 = reproxy

reproxyField3 :: t a -> Proxy (Field3 a)
reproxyField3 = reproxy

reproxyField4 :: t a -> Proxy (Field4 a)
reproxyField4 = reproxy

reproxyField5 :: t a -> Proxy (Field5 a)
reproxyField5 = reproxy

reproxyField6 :: t a -> Proxy (Field6 a)
reproxyField6 = reproxy

reproxyField7 :: t a -> Proxy (Field7 a)
reproxyField7 = reproxy

reproxyField8 :: t a -> Proxy (Field8 a)
reproxyField8 = reproxy

unsafeRead :: ( ByteArraySlice a
              , MonadPrim m
              ) => (Proxy t -> Int#) -> ByteArrayTuple (World m) t -> m a
unsafeRead offset t@(ByteArrayTuple array) =
  liftPrim $ readByteArray# array (offset (proxyTuple t))

unsafeWrite :: ( ByteArraySlice a
               , MonadPrim m
               ) => (Proxy t -> Int#) -> ByteArrayTuple (World m) t -> a -> m ()
unsafeWrite offset t@(ByteArrayTuple array) a = liftPrim $ \ s ->
  case writeByteArray# array (offset (proxyTuple t)) a s of
    s' -> (# s', () #)
