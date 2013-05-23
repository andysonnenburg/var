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
  , MultiParamTypeClasses #-}
#ifdef LANGUAGE_Trustworthy
{-# LANGUAGE Trustworthy #-}
#endif
{-# LANGUAGE TypeFamilies, UndecidableInstances #-}
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
import Data.Prim.ByteArray
import Data.Proxy
import Data.Tuple.Fields
import Data.Tuple.Fields.Extra
import Data.Tuple.MTuple
import Data.Typeable (Typeable)

newtype ByteArrayTuple s a = ByteArrayTuple (MutableByteArray s) deriving (Eq, Typeable)

instance ( MonadPrim m
         , s ~ World m
         , Fields t
         , ByteArraySlice t
         ) => MTuple (ByteArrayTuple s) t m where
  thawTuple a = do
    array <- newByteArray (byteSizeOf a)
    writeByteArray array 0 a
    return $ ByteArrayTuple array
  freezeTuple (ByteArrayTuple array) = readByteArray array 0

instance ( MonadPrim m
         , s ~ World m
         , Fields t
         , ByteArraySlice t
         , a ~ Field1 t
         , ByteArraySlice a
         ) => MField1 (ByteArrayTuple s) t a m where
  read1 = unsafeRead offset1
  write1 = unsafeWrite offset1

instance ( MonadPrim m
         , s ~ World m
         , Fields t
         , ByteArraySlice t
         , ByteArraySlice (Field1 t)
         , a ~ Field2 t
         , ByteArraySlice a
         ) => MField2 (ByteArrayTuple s) t a m where
  read2 = unsafeRead offset2
  write2 = unsafeWrite offset2

instance ( MonadPrim m
         , s ~ World m
         , Fields t
         , ByteArraySlice t
         , ByteArraySlice (Field1 t)
         , ByteArraySlice (Field2 t)
         , a ~ Field3 t
         , ByteArraySlice a
         ) => MField3 (ByteArrayTuple s) t a m where
  read3 = unsafeRead offset3
  write3 = unsafeWrite offset3

instance ( MonadPrim m
         , s ~ World m
         , Fields t
         , ByteArraySlice t
         , ByteArraySlice (Field1 t)
         , ByteArraySlice (Field2 t)
         , ByteArraySlice (Field3 t)
         , a ~ Field4 t
         , ByteArraySlice a
         ) => MField4 (ByteArrayTuple s) t a m where
  read4 = unsafeRead offset4
  write4 = unsafeWrite offset4

instance ( MonadPrim m
         , s ~ World m
         , Fields t
         , ByteArraySlice t
         , ByteArraySlice (Field1 t)
         , ByteArraySlice (Field2 t)
         , ByteArraySlice (Field3 t)
         , ByteArraySlice (Field4 t)
         , a ~ Field5 t
         , ByteArraySlice a
         ) => MField5 (ByteArrayTuple s) t a m where
  read5 = unsafeRead offset5
  write5 = unsafeWrite offset5

instance ( MonadPrim m
         , s ~ World m
         , Fields t
         , ByteArraySlice t
         , ByteArraySlice (Field1 t)
         , ByteArraySlice (Field2 t)
         , ByteArraySlice (Field3 t)
         , ByteArraySlice (Field4 t)
         , ByteArraySlice (Field5 t)
         , a ~ Field6 t
         , ByteArraySlice a
         ) => MField6 (ByteArrayTuple s) t a m where
  read6 = unsafeRead offset6
  write6 = unsafeWrite offset6

instance ( MonadPrim m
         , s ~ World m
         , Fields t
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
  read7 = unsafeRead offset7
  write7 = unsafeWrite offset7

instance ( MonadPrim m
         , s ~ World m
         , Fields t
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
  read8 = unsafeRead offset8
  write8 = unsafeWrite offset8

instance ( MonadPrim m
         , s ~ World m
         , Fields t
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
  read9 = unsafeRead offset9
  write9 = unsafeWrite offset9

offset1 :: t a -> Int
offset1 _ = 0

offset2 :: ByteArraySlice (Field1 a) => t a -> Int
offset2 a = plusByteSize (offset1 a) (reproxyField1 a)

offset3 :: ( ByteArraySlice (Field1 a)
           , ByteArraySlice (Field2 a)
           ) => t a -> Int
offset3 a = plusByteSize (offset2 a) (reproxyField2 a)

offset4 :: ( ByteArraySlice (Field1 a)
           , ByteArraySlice (Field2 a)
           , ByteArraySlice (Field3 a)
           ) => t a -> Int
offset4 a = plusByteSize (offset3 a) (reproxyField3 a)

offset5 :: ( ByteArraySlice (Field1 a)
           , ByteArraySlice (Field2 a)
           , ByteArraySlice (Field3 a)
           , ByteArraySlice (Field4 a)
           ) => t a -> Int
offset5 a = plusByteSize (offset4 a) (reproxyField4 a)

offset6 :: ( ByteArraySlice (Field1 a)
           , ByteArraySlice (Field2 a)
           , ByteArraySlice (Field3 a)
           , ByteArraySlice (Field4 a)
           , ByteArraySlice (Field5 a)
           ) => t a -> Int
offset6 a = plusByteSize (offset5 a) (reproxyField5 a)

offset7 :: ( ByteArraySlice (Field1 a)
           , ByteArraySlice (Field2 a)
           , ByteArraySlice (Field3 a)
           , ByteArraySlice (Field4 a)
           , ByteArraySlice (Field5 a)
           , ByteArraySlice (Field6 a)
           ) => t a -> Int
offset7 a = plusByteSize (offset6 a) (reproxyField6 a)

offset8 :: ( ByteArraySlice (Field1 a)
           , ByteArraySlice (Field2 a)
           , ByteArraySlice (Field3 a)
           , ByteArraySlice (Field4 a)
           , ByteArraySlice (Field5 a)
           , ByteArraySlice (Field6 a)
           , ByteArraySlice (Field7 a)
           ) => t a -> Int
offset8 a = plusByteSize (offset7 a) (reproxyField7 a)

offset9 :: ( ByteArraySlice (Field1 a)
           , ByteArraySlice (Field2 a)
           , ByteArraySlice (Field3 a)
           , ByteArraySlice (Field4 a)
           , ByteArraySlice (Field5 a)
           , ByteArraySlice (Field6 a)
           , ByteArraySlice (Field7 a)
           , ByteArraySlice (Field8 a)
           ) => t a -> Int
offset9 a = plusByteSize (offset8 a) (reproxyField8 a)

proxyFields :: t a -> Proxy a
proxyFields = reproxy

unsafeRead :: ( ByteArraySlice a
              , MonadPrim m
              ) => (Proxy t -> Int) -> ByteArrayTuple (World m) t -> m a
unsafeRead offset t@(ByteArrayTuple array) =
  readByteArray array (offset (proxyFields t))

unsafeWrite :: ( ByteArraySlice a
               , MonadPrim m
               ) => (Proxy t -> Int) -> ByteArrayTuple (World m) t -> a -> m ()
unsafeWrite offset t@(ByteArrayTuple array) a =
  writeByteArray array (offset (proxyFields t)) a
