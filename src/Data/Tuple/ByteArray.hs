{-# LANGUAGE
    CPP
  , DefaultSignatures
  , DeriveDataTypeable
  , FlexibleInstances
  , FlexibleContexts
  , GADTs
  , MultiParamTypeClasses
  , Rank2Types #-}
#ifdef LANGUAGE_Trustworthy
{-# LANGUAGE Trustworthy #-}
#endif
{-# LANGUAGE
    TypeFamilies
  , TypeOperators
  , UndecidableInstances #-}
{- |
Copyright   :  (c) Andy Sonnenburg 2013
License     :  BSD3
Maintainer  :  andy22286@gmail.com
-}
module Data.Tuple.ByteArray
       ( ByteArrayTuple
       ) where

import Control.Applicative
import Control.Monad.Prim

import Data.ByteArraySlice.Unsafe
import Data.Prim.ByteArray
import Data.Proxy
import Data.Tuple.ITuple
import Data.Tuple.ITuple.Proxy
import Data.Tuple.MTuple
import Data.Typeable (Typeable)

newtype ByteArrayTuple s a = ByteArrayTuple (MutableByteArray s) deriving (Eq, Typeable)

instance ( MonadPrim m
         , s ~ World m
         , ITuple t
         , ByteArraySlice (Tuple (ListRep t))
         ) => MTuple (ByteArrayTuple s) t m where
  thawTuple a = runPrim $ do
    array <- newByteArray (byteSizeOf' a)
    writeByteOff array 0 (toTuple a)
    return $ ByteArrayTuple array
  freezeTuple (ByteArrayTuple array) = runPrim $ fromTuple <$> readByteOff array 0

instance ( MonadPrim m
         , s ~ World m
         , ITuple t
         , ByteArraySlice (Tuple (ListRep t))
         , ByteArraySlice (Field1 t)
         ) => MField1 (ByteArrayTuple s) t m where
  read1 = unsafeRead offset1
  write1 = unsafeWrite offset1

instance ( MonadPrim m
         , s ~ World m
         , ITuple t
         , ByteArraySlice (Tuple (ListRep t))
         , ByteArraySlice (Field1 t)
         , ByteArraySlice (Field2 t)
         ) => MField2 (ByteArrayTuple s) t m where
  read2 = unsafeRead offset2
  write2 = unsafeWrite offset2

instance ( MonadPrim m
         , s ~ World m
         , ITuple t
         , ByteArraySlice (Tuple (ListRep t))
         , ByteArraySlice (Field1 t)
         , ByteArraySlice (Field2 t)
         , ByteArraySlice (Field3 t)
         ) => MField3 (ByteArrayTuple s) t m where
  read3 = unsafeRead offset3
  write3 = unsafeWrite offset3

instance ( MonadPrim m
         , s ~ World m
         , ITuple t
         , ByteArraySlice (Tuple (ListRep t))
         , ByteArraySlice (Field1 t)
         , ByteArraySlice (Field2 t)
         , ByteArraySlice (Field3 t)
         , ByteArraySlice (Field4 t)
         ) => MField4 (ByteArrayTuple s) t m where
  read4 = unsafeRead offset4
  write4 = unsafeWrite offset4

instance ( MonadPrim m
         , s ~ World m
         , ITuple t
         , ByteArraySlice (Tuple (ListRep t))
         , ByteArraySlice (Field1 t)
         , ByteArraySlice (Field2 t)
         , ByteArraySlice (Field3 t)
         , ByteArraySlice (Field4 t)
         , ByteArraySlice (Field5 t)
         ) => MField5 (ByteArrayTuple s) t m where
  read5 = unsafeRead offset5
  write5 = unsafeWrite offset5

instance ( MonadPrim m
         , s ~ World m
         , ITuple t
         , ByteArraySlice (Tuple (ListRep t))
         , ByteArraySlice (Field1 t)
         , ByteArraySlice (Field2 t)
         , ByteArraySlice (Field3 t)
         , ByteArraySlice (Field4 t)
         , ByteArraySlice (Field5 t)
         , ByteArraySlice (Field6 t)
         ) => MField6 (ByteArrayTuple s) t m where
  read6 = unsafeRead offset6
  write6 = unsafeWrite offset6

instance ( MonadPrim m
         , s ~ World m
         , ITuple t
         , ByteArraySlice (Tuple (ListRep t))
         , ByteArraySlice (Field1 t)
         , ByteArraySlice (Field2 t)
         , ByteArraySlice (Field3 t)
         , ByteArraySlice (Field4 t)
         , ByteArraySlice (Field5 t)
         , ByteArraySlice (Field6 t)
         , ByteArraySlice (Field7 t)
         ) => MField7 (ByteArrayTuple s) t m where
  read7 = unsafeRead offset7
  write7 = unsafeWrite offset7

instance ( MonadPrim m
         , s ~ World m
         , ITuple t
         , ByteArraySlice (Tuple (ListRep t))
         , ByteArraySlice (Field1 t)
         , ByteArraySlice (Field2 t)
         , ByteArraySlice (Field3 t)
         , ByteArraySlice (Field4 t)
         , ByteArraySlice (Field5 t)
         , ByteArraySlice (Field6 t)
         , ByteArraySlice (Field7 t)
         , ByteArraySlice (Field8 t)
         ) => MField8 (ByteArrayTuple s) t m where
  read8 = unsafeRead offset8
  write8 = unsafeWrite offset8

instance ( MonadPrim m
         , s ~ World m
         , ITuple t
         , ByteArraySlice (Tuple (ListRep t))
         , ByteArraySlice (Field1 t)
         , ByteArraySlice (Field2 t)
         , ByteArraySlice (Field3 t)
         , ByteArraySlice (Field4 t)
         , ByteArraySlice (Field5 t)
         , ByteArraySlice (Field6 t)
         , ByteArraySlice (Field7 t)
         , ByteArraySlice (Field8 t)
         , ByteArraySlice (Field9 t)
         ) => MField9 (ByteArrayTuple s) t m where
  read9 = unsafeRead offset9
  write9 = unsafeWrite offset9

byteSizeOf' :: (ITuple t, ByteArraySlice (Tuple (ListRep t))) => t -> Int
byteSizeOf' = plusByteSize 0 . proxyTuple

unsafeRead :: ( ByteArraySlice a
              , MonadPrim m
              ) => (forall f . f t -> Int) -> ByteArrayTuple (World m) t -> m a
unsafeRead offset t@(ByteArrayTuple array) =
  runPrim $ readByteOff array (offset t)

unsafeWrite :: ( ByteArraySlice a
               , MonadPrim m
               ) => (forall f . f t -> Int) -> ByteArrayTuple (World m) t -> a -> m ()
unsafeWrite offset t@(ByteArrayTuple array) a =
  runPrim $ writeByteOff array (offset t) a

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

proxyTuple :: ITuple a => a -> Proxy (Tuple (ListRep a))
proxyTuple _ = Proxy
