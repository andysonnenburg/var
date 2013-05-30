{-# LANGUAGE
    DefaultSignatures
  , DeriveDataTypeable
  , EmptyDataDecls
  , FlexibleContexts
  , FlexibleInstances
  , MultiParamTypeClasses
  , Rank2Types
  , TypeFamilies
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
       , withStorableTuple
       , touchStorableTuple
       ) where

import Control.Applicative

import Data.Data (Data (..), Typeable, mkNoRepType)
import Data.Proxy
import Data.Tuple.Fields
import Data.Tuple.Fields.Proxy
import Data.Tuple.MTuple

import Foreign.ForeignPtr.Safe
import Foreign.Ptr
import Foreign.Storable

import GHC.Generics

newtype StorableTuple a =
  StorableTuple { unStorableTuple :: ForeignPtr Void
                } deriving (Show, Eq, Ord, Typeable)

data Void

instance Typeable a => Data (StorableTuple a) where
  toConstr _ = error "Data.Data.toConstr(StorableTuple)"
  gunfold _ _ = error "Data.Data.gunfold(StorableTuple)"
  dataTypeOf _ = mkNoRepType "Data.Tuple.Storable.StorableTuple"

instance MTuple StorableTuple () IO where
  thawTuple = thawTuple'
  freezeTuple = freezeTuple'

instance (Storable a, Storable b) => MTuple StorableTuple (a, b) IO where
  thawTuple = thawTuple'
  freezeTuple = freezeTuple'

instance ( Storable a
         , Storable b
         , Storable c
         ) => MTuple StorableTuple (a, b, c) IO where
  thawTuple = thawTuple'
  freezeTuple = freezeTuple'

instance ( Storable a
         , Storable b
         , Storable c
         , Storable d
         ) => MTuple StorableTuple (a, b, c, d) IO where
  thawTuple = thawTuple'
  freezeTuple = freezeTuple'

instance ( Storable a
         , Storable b
         , Storable c
         , Storable d
         , Storable e
         ) => MTuple StorableTuple (a, b, c, d, e) IO where
  thawTuple = thawTuple'
  freezeTuple = freezeTuple'

instance ( Storable a
         , Storable b
         , Storable c
         , Storable d
         , Storable e
         , Storable f
         ) => MTuple StorableTuple (a, b, c, d, e, f) IO where
  thawTuple = thawTuple'
  freezeTuple = freezeTuple'

instance ( Storable a
         , Storable b
         , Storable c
         , Storable d
         , Storable e
         , Storable f
         , Storable g
         ) => MTuple StorableTuple (a, b, c, d, e, f, g) IO where
  thawTuple = thawTuple'
  freezeTuple = freezeTuple'

instance ( Fields t
         , MTuple StorableTuple t IO
         , a ~ Field1 t
         , Storable a
         ) => MField1 StorableTuple t a IO where
  read1 = unsafeRead offset1
  write1 = unsafeWrite offset1

instance ( Fields t
         , MTuple StorableTuple t IO
         , Storable (Field1 t)
         , a ~ Field2 t
         , Storable a
         ) => MField2 StorableTuple t a IO where
  read2 = unsafeRead offset2
  write2 = unsafeWrite offset2

instance ( Fields t
         , MTuple StorableTuple t IO
         , Storable (Field1 t)
         , Storable (Field2 t)
         , a ~ Field3 t
         , Storable a
         ) => MField3 StorableTuple t a IO where
  read3 = unsafeRead offset3
  write3 = unsafeWrite offset3

instance ( Fields t
         , MTuple StorableTuple t IO
         , Storable (Field1 t)
         , Storable (Field2 t)
         , Storable (Field3 t)
         , a ~ Field4 t
         , Storable a
         ) => MField4 StorableTuple t a IO where
  read4 = unsafeRead offset4
  write4 = unsafeWrite offset4

instance ( Fields t
         , MTuple StorableTuple t IO
         , Storable (Field1 t)
         , Storable (Field2 t)
         , Storable (Field3 t)
         , Storable (Field4 t)
         , a ~ Field5 t
         , Storable a
         ) => MField5 StorableTuple t a IO where
  read5 = unsafeRead offset5
  write5 = unsafeWrite offset5

instance ( Fields t
         , MTuple StorableTuple t IO
         , Storable (Field1 t)
         , Storable (Field2 t)
         , Storable (Field3 t)
         , Storable (Field4 t)
         , Storable (Field5 t)
         , a ~ Field6 t
         , Storable a
         ) => MField6 StorableTuple t a IO where
  read6 = unsafeRead offset6
  write6 = unsafeWrite offset6

instance ( Fields t
         , MTuple StorableTuple t IO
         , Storable (Field1 t)
         , Storable (Field2 t)
         , Storable (Field3 t)
         , Storable (Field4 t)
         , Storable (Field5 t)
         , Storable (Field6 t)
         , a ~ Field7 t
         , Storable a
         ) => MField7 StorableTuple t a IO where
  read7 = unsafeRead offset7
  write7 = unsafeWrite offset7

instance ( Fields t
         , MTuple StorableTuple t IO
         , Storable (Field1 t)
         , Storable (Field2 t)
         , Storable (Field3 t)
         , Storable (Field4 t)
         , Storable (Field5 t)
         , Storable (Field6 t)
         , Storable (Field7 t)
         , a ~ Field8 t
         , Storable a
         ) => MField8 StorableTuple t a IO where
  read8 = unsafeRead offset8
  write8 = unsafeWrite offset8

instance ( Fields t
         , MTuple StorableTuple t IO
         , Storable (Field1 t)
         , Storable (Field2 t)
         , Storable (Field3 t)
         , Storable (Field4 t)
         , Storable (Field5 t)
         , Storable (Field6 t)
         , Storable (Field7 t)
         , Storable (Field8 t)
         , a ~ Field9 t
         , Storable a
         ) => MField9 StorableTuple t a IO where
  read9 = unsafeRead offset9
  write9 = unsafeWrite offset9

class Fields a => StorableFields a where
  plusSize :: Int -> t a -> Int
  peekFieldsOff :: Ptr Void -> Int -> IO a
  pokeFieldsOff :: Ptr Void -> Int -> a -> IO ()

  default plusSize :: (Generic a, GStorableFields (Rep a)) => Int -> t a -> Int
  plusSize i = gplusSize i . reproxyRep

  default peekFieldsOff :: ( Generic a
                           , GStorableFields (Rep a)
                           ) => Ptr Void -> Int -> IO a
  peekFieldsOff ptr = fmap to . gpeekFieldsOff ptr

  default pokeFieldsOff :: ( Generic a
                           , GStorableFields (Rep a)
                           ) => Ptr Void -> Int -> a -> IO ()
  pokeFieldsOff ptr i = gpokeFieldsOff ptr i . from

thawTuple' :: StorableFields a => a -> IO (StorableTuple a)
thawTuple' a = do
  ptr <- mallocForeignPtrBytes (sizeOf' a)
  withForeignPtr ptr $ \ ptr' -> pokeFieldsOff ptr' 0 a
  return $ StorableTuple ptr

freezeTuple' :: StorableFields a => StorableTuple a -> IO a
freezeTuple' (StorableTuple ptr) = withForeignPtr ptr $ flip peekFieldsOff 0

sizeOf' :: StorableFields a => a -> Int
sizeOf' = plusSize 0 . proxy

class GStorableFields a where
  gplusSize :: Int -> t (a p) -> Int
  gpeekFieldsOff :: Ptr Void -> Int -> IO (a p)
  gpokeFieldsOff :: Ptr Void -> Int -> a p -> IO ()

instance GStorableFields U1 where
  gplusSize = const
  gpeekFieldsOff _ _ = return U1
  gpokeFieldsOff _ _ _ = return ()

instance Storable c => GStorableFields (K1 i c) where
  gplusSize i = plusSize' i . reproxyK1
  gpeekFieldsOff ptr i = m
    where
      m = K1 <$> peekByteOff (castPtr ptr) (align i $ alignment' $ reproxyK1 m)
  gpokeFieldsOff ptr i (K1 a) =
    pokeByteOff (castPtr ptr) (align i $ alignment a) a

instance GStorableFields f => GStorableFields (M1 i c f) where
  gplusSize i = gplusSize i . reproxyM1
  gpeekFieldsOff ptr = fmap M1 . gpeekFieldsOff ptr
  gpokeFieldsOff ptr i = gpokeFieldsOff ptr i . unM1

instance (GStorableFields a, GStorableFields b) => GStorableFields (a :*: b) where
  gplusSize i a = gplusSize (gplusSize i (reproxyFst a)) (reproxySnd a)
  gpeekFieldsOff ptr i = do
    a <- gpeekFieldsOff ptr i
    b <- gpeekFieldsOff ptr (gplusSize i (proxy a))
    return $ a :*: b
  gpokeFieldsOff ptr i (a :*: b) = do
    gpokeFieldsOff ptr i a
    gpokeFieldsOff ptr (gplusSize i (proxy a)) b

instance StorableFields ()
instance (Storable a, Storable b) => StorableFields (a, b)
instance (Storable a, Storable b, Storable c) => StorableFields (a, b, c)
instance ( Storable a
         , Storable b
         , Storable c
         , Storable d
         ) => StorableFields (a, b, c, d)
instance ( Storable a
         , Storable b
         , Storable c
         , Storable d
         , Storable e
         ) => StorableFields (a, b, c, d, e)
instance ( Storable a
         , Storable b
         , Storable c
         , Storable d
         , Storable e
         , Storable f
         ) => StorableFields (a, b, c, d, e, f)
instance ( Storable a
         , Storable b
         , Storable c
         , Storable d
         , Storable e
         , Storable f
         , Storable g
         ) => StorableFields (a, b, c, d, e, f, g)

offset1 :: t a -> Int
offset1 _ = 0

offset2 :: Storable (Field1 a) => t a -> Int
offset2 a = plusSize' (offset1 a) (reproxyField1 a)

offset3 :: (Storable (Field1 a), Storable (Field2 a)) => t a -> Int
offset3 a = plusSize' (offset2 a) (reproxyField2 a)

offset4 :: ( Storable (Field1 a)
           , Storable (Field2 a)
           , Storable (Field3 a)
           ) => t a -> Int
offset4 a = plusSize' (offset3 a) (reproxyField3 a)

offset5 :: ( Storable (Field1 a)
           , Storable (Field2 a)
           , Storable (Field3 a)
           , Storable (Field4 a)
           ) => t a -> Int
offset5 a = plusSize' (offset4 a) (reproxyField4 a)

offset6 :: ( Storable (Field1 a)
           , Storable (Field2 a)
           , Storable (Field3 a)
           , Storable (Field4 a)
           , Storable (Field5 a)
           ) => t a -> Int
offset6 a = plusSize' (offset5 a) (reproxyField5 a)

offset7 :: ( Storable (Field1 a)
           , Storable (Field2 a)
           , Storable (Field3 a)
           , Storable (Field4 a)
           , Storable (Field5 a)
           , Storable (Field6 a)
           ) => t a -> Int
offset7 a = plusSize' (offset6 a) (reproxyField6 a)

offset8 :: ( Storable (Field1 a)
           , Storable (Field2 a)
           , Storable (Field3 a)
           , Storable (Field4 a)
           , Storable (Field5 a)
           , Storable (Field6 a)
           , Storable (Field7 a)
           ) => t a -> Int
offset8 a = plusSize' (offset7 a) (reproxyField7 a)

offset9 :: ( Storable (Field1 a)
           , Storable (Field2 a)
           , Storable (Field3 a)
           , Storable (Field4 a)
           , Storable (Field5 a)
           , Storable (Field6 a)
           , Storable (Field7 a)
           , Storable (Field8 a)
           ) => t a -> Int
offset9 a = plusSize' (offset8 a) (reproxyField8 a)

plusSize' :: Storable a => Int -> t a -> Int
plusSize' i t = align i (alignment' t) + size t

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

proxyFields :: t a -> Proxy a
proxyFields = reproxy

unsafeRead :: Storable a => (Proxy t -> Int) -> StorableTuple t -> IO a
unsafeRead offset t = m
  where
    m = withForeignPtr (unStorableTuple t) $ \ ptr ->
      peekByteOff ptr (align (offset (proxyFields t)) (alignment' m))

unsafeWrite :: Storable a => (Proxy t -> Int) -> StorableTuple t -> a -> IO ()
unsafeWrite offset t a = withForeignPtr (unStorableTuple t) $ \ ptr ->
  pokeByteOff ptr (align (offset (proxyFields t)) (alignment a)) a

withStorableTuple :: StorableTuple a -> (forall e . Ptr e -> IO b) -> IO b
withStorableTuple tuple f = withForeignPtr (unStorableTuple tuple) f

touchStorableTuple :: StorableTuple a -> IO ()
touchStorableTuple = touchForeignPtr . unStorableTuple
