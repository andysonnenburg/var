{-# LANGUAGE CPP #-}
#ifdef LANGUAGE_DataKinds
{-# LANGUAGE DataKinds #-}
#endif
{-# LANGUAGE
    DeriveDataTypeable
  , EmptyDataDecls
  , FlexibleContexts
  , FlexibleInstances
  , GADTs
  , MultiParamTypeClasses
  , Rank2Types
  , TypeOperators
  , UndecidableInstances #-}
{- |
Copyright   :  (c) Andy Sonnenburg 2013
License     :  BSD3
Maintainer  :  andy22286@gmail.com
-}
module Data.Tuple.Storable
       ( module Data.Tuple.MTuple
       , StorableTuple
       , StorableList
       , withStorableTuple
       , touchStorableTuple
       ) where

import Data.Data (Data (..), Typeable, mkNoRepType)
import Data.Proxy
import Data.Tuple.ITuple
import Data.Tuple.ITuple.Proxy
import Data.Tuple.MTuple

import Foreign.ForeignPtr.Safe
import Foreign.Ptr
import Foreign.Storable

newtype StorableTuple a =
  StorableTuple { unStorableTuple :: ForeignPtr Void
                } deriving (Show, Eq, Ord, Typeable)

data Void

instance Typeable a => Data (StorableTuple a) where
  toConstr _ = error "Data.Data.toConstr(StorableTuple)"
  gunfold _ _ = error "Data.Data.gunfold(StorableTuple)"
  dataTypeOf _ = mkNoRepType "Data.Tuple.Storable.StorableTuple"

instance ( ITuple t
         , StorableList (ListRep t)
         ) => MTuple StorableTuple t IO where
  thawTuple t = do
    ptr <- mallocForeignPtrBytes (sizeOf' t)
    withForeignPtr ptr $ \ ptr' -> pokeByteOff' ptr' 0 $ toTuple t
    return $ StorableTuple ptr
  freezeTuple (StorableTuple ptr) =
    fmap fromTuple . withForeignPtr ptr $ flip peekByteOff' 0

instance ( ITuple t
         , StorableList (ListRep t)
         , Storable (Field1 t)
         ) => MField1 StorableTuple t IO where
  read1 = unsafeRead offset1
  write1 = unsafeWrite offset1

instance ( ITuple t
         , StorableList (ListRep t)
         , Storable (Field1 t)
         , Storable (Field2 t)
         ) => MField2 StorableTuple t IO where
  read2 = unsafeRead offset2
  write2 = unsafeWrite offset2

instance ( ITuple t
         , StorableList (ListRep t)
         , Storable (Field1 t)
         , Storable (Field2 t)
         , Storable (Field3 t)
         ) => MField3 StorableTuple t IO where
  read3 = unsafeRead offset3
  write3 = unsafeWrite offset3

instance ( ITuple t
         , StorableList (ListRep t)
         , Storable (Field1 t)
         , Storable (Field2 t)
         , Storable (Field3 t)
         , Storable (Field4 t)
         ) => MField4 StorableTuple t IO where
  read4 = unsafeRead offset4
  write4 = unsafeWrite offset4

instance ( ITuple t
         , StorableList (ListRep t)
         , Storable (Field1 t)
         , Storable (Field2 t)
         , Storable (Field3 t)
         , Storable (Field4 t)
         , Storable (Field5 t)
         ) => MField5 StorableTuple t IO where
  read5 = unsafeRead offset5
  write5 = unsafeWrite offset5

instance ( ITuple t
         , StorableList (ListRep t)
         , Storable (Field1 t)
         , Storable (Field2 t)
         , Storable (Field3 t)
         , Storable (Field4 t)
         , Storable (Field5 t)
         , Storable (Field6 t)
         ) => MField6 StorableTuple t IO where
  read6 = unsafeRead offset6
  write6 = unsafeWrite offset6

instance ( ITuple t
         , StorableList (ListRep t)
         , Storable (Field1 t)
         , Storable (Field2 t)
         , Storable (Field3 t)
         , Storable (Field4 t)
         , Storable (Field5 t)
         , Storable (Field6 t)
         , Storable (Field7 t)
         ) => MField7 StorableTuple t IO where
  read7 = unsafeRead offset7
  write7 = unsafeWrite offset7

instance ( ITuple t
         , StorableList (ListRep t)
         , Storable (Field1 t)
         , Storable (Field2 t)
         , Storable (Field3 t)
         , Storable (Field4 t)
         , Storable (Field5 t)
         , Storable (Field6 t)
         , Storable (Field7 t)
         , Storable (Field8 t)
         ) => MField8 StorableTuple t IO where
  read8 = unsafeRead offset8
  write8 = unsafeWrite offset8

instance ( ITuple t
         , StorableList (ListRep t)
         , Storable (Field1 t)
         , Storable (Field2 t)
         , Storable (Field3 t)
         , Storable (Field4 t)
         , Storable (Field5 t)
         , Storable (Field6 t)
         , Storable (Field7 t)
         , Storable (Field8 t)
         , Storable (Field9 t)
         ) => MField9 StorableTuple t IO where
  read9 = unsafeRead offset9
  write9 = unsafeWrite offset9

withStorableTuple :: StorableTuple a -> (forall e . Ptr e -> IO b) -> IO b
withStorableTuple tuple f = withForeignPtr (unStorableTuple tuple) f

touchStorableTuple :: StorableTuple a -> IO ()
touchStorableTuple = touchForeignPtr . unStorableTuple

sizeOf' :: (ITuple t, StorableList (ListRep t)) => t -> Int
sizeOf' = plusSize' 0 . proxyListRep

unsafeRead :: Storable a => (forall f . f t -> Int) -> StorableTuple t -> IO a
unsafeRead offset t = m
  where
    m = withForeignPtr (unStorableTuple t) $ \ ptr ->
      peekByteOff ptr (align (offset t) (alignment' m))

unsafeWrite :: Storable a => (forall f . f t -> Int) -> StorableTuple t -> a -> IO ()
unsafeWrite offset t a = withForeignPtr (unStorableTuple t) $ \ ptr ->
  pokeByteOff ptr (align (offset t) (alignment a)) a

offset1 :: t a -> Int
offset1 _ = 0

offset2 :: Storable (Field1 a) => t a -> Int
offset2 a = plusSize (offset1 a) (reproxyField1 a)

offset3 :: (Storable (Field1 a), Storable (Field2 a)) => t a -> Int
offset3 a = plusSize (offset2 a) (reproxyField2 a)

offset4 :: ( Storable (Field1 a)
           , Storable (Field2 a)
           , Storable (Field3 a)
           ) => t a -> Int
offset4 a = plusSize (offset3 a) (reproxyField3 a)

offset5 :: ( Storable (Field1 a)
           , Storable (Field2 a)
           , Storable (Field3 a)
           , Storable (Field4 a)
           ) => t a -> Int
offset5 a = plusSize (offset4 a) (reproxyField4 a)

offset6 :: ( Storable (Field1 a)
           , Storable (Field2 a)
           , Storable (Field3 a)
           , Storable (Field4 a)
           , Storable (Field5 a)
           ) => t a -> Int
offset6 a = plusSize (offset5 a) (reproxyField5 a)

offset7 :: ( Storable (Field1 a)
           , Storable (Field2 a)
           , Storable (Field3 a)
           , Storable (Field4 a)
           , Storable (Field5 a)
           , Storable (Field6 a)
           ) => t a -> Int
offset7 a = plusSize (offset6 a) (reproxyField6 a)

offset8 :: ( Storable (Field1 a)
           , Storable (Field2 a)
           , Storable (Field3 a)
           , Storable (Field4 a)
           , Storable (Field5 a)
           , Storable (Field6 a)
           , Storable (Field7 a)
           ) => t a -> Int
offset8 a = plusSize (offset7 a) (reproxyField7 a)

offset9 :: ( Storable (Field1 a)
           , Storable (Field2 a)
           , Storable (Field3 a)
           , Storable (Field4 a)
           , Storable (Field5 a)
           , Storable (Field6 a)
           , Storable (Field7 a)
           , Storable (Field8 a)
           ) => t a -> Int
offset9 a = plusSize (offset8 a) (reproxyField8 a)

plusSize :: Storable a => Int -> t a -> Int
plusSize i t = align i (alignment' t) + size t

alignment' :: Storable a => t a -> Int
alignment' = alignment . unproxy

align :: Int -> Int -> Int
align a i = case a `rem` i of
  0 -> a
  n -> a + (i - n)

size :: Storable a => t a -> Int
size = sizeOf . unproxy

unproxy :: t a -> a
unproxy = undefined

class StorableList xs where
  plusSize' :: Int -> t xs -> Int
  peekByteOff' :: Ptr Void -> Int -> IO (Tuple xs)
  pokeByteOff' :: Ptr Void -> Int -> Tuple xs -> IO ()

instance StorableList Nil where
  plusSize' = const
  peekByteOff' _ _ = return U
  pokeByteOff' _ _ _ = return ()

instance (Storable x, StorableList xs) => StorableList (x :| xs) where
  plusSize' i xs = plusSize' (plusSize i (reproxyHead xs)) (reproxyTail xs)
  peekByteOff' ptr i = do
    x <- let m = peekByteOff (castPtr ptr) (align i $ alignment' m) in m
    xs <- peekByteOff' ptr (plusSize i (proxy x))
    return $ x :* xs
  pokeByteOff' ptr i (x :* xs) = do
    pokeByteOff (castPtr ptr) (align i $ alignment x) x
    pokeByteOff' ptr (plusSize i (proxy x)) xs
