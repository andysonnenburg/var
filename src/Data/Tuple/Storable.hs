{-# LANGUAGE
    DefaultSignatures
  , DeriveDataTypeable
  , EmptyDataDecls
  , FlexibleContexts
  , FlexibleInstances
  , MultiParamTypeClasses
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
       ) where

import Data.Data (Data (..), Typeable, mkNoRepType)
import Data.Proxy
import Data.Tuple.Fields
import Data.Tuple.Fields.Extra
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
  size :: t a -> Int
  peekFields :: Ptr Void -> IO a
  pokeFields :: Ptr Void -> a -> IO ()

  default size :: (Generic a, GStorableFields (Rep a)) => t a -> Int
  size = gsize . reproxyRep

  default peekFields :: (Generic a, GStorableFields (Rep a)) => Ptr Void -> IO a
  peekFields = fmap to . gpeekFields

  default pokeFields :: (Generic a, GStorableFields (Rep a)) => Ptr Void -> a -> IO ()
  pokeFields ptr = gpokeFields ptr . from

thawTuple' :: StorableFields a => a -> IO (StorableTuple a)
thawTuple' a = do
  ptr <- mallocForeignPtrBytes (sizeOf' a)
  withForeignPtr ptr $ flip pokeFields a
  return $ StorableTuple ptr

freezeTuple' :: StorableFields a => StorableTuple a -> IO a
freezeTuple' = flip withForeignPtr peekFields . unStorableTuple

sizeOf' :: StorableFields a => a -> Int
sizeOf' = size . proxy

class GStorableFields a where
  gsize :: t (a p) -> Int
  gpeekFields :: Ptr Void -> IO (a p)
  gpokeFields :: Ptr Void -> a p -> IO ()

instance GStorableFields U1 where
  gsize _ = 0
  gpeekFields _ = return U1
  gpokeFields _ _ = return ()

instance Storable c => GStorableFields (K1 i c) where
  gsize = sizeOf . unproxy . reproxyK1
  gpeekFields = fmap K1 . peek . castPtr
  gpokeFields ptr = poke (castPtr ptr) . unK1

instance GStorableFields f => GStorableFields (M1 i c f) where
  gsize = gsize . reproxyM1
  gpeekFields = fmap M1 . gpeekFields
  gpokeFields ptr = gpokeFields ptr . unM1

instance (GStorableFields a, GStorableFields b) => GStorableFields (a :*: b) where
  gsize a = gsize (reproxyFst a) + gsize (reproxySnd a)
  gpeekFields ptr = do
    a <- gpeekFields ptr
    b <- gpeekFields (plusPtr ptr (gsize (proxy a)))
    return $ a :*: b
  gpokeFields ptr (a :*: b) = do
    gpokeFields ptr a
    gpokeFields (plusPtr ptr (gsize (proxy a))) b

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
offset2 a = offset1 a + sizeOf (unproxy (reproxyField1 a))

offset3 :: (Storable (Field1 a), Storable (Field2 a)) => t a -> Int
offset3 a = offset2 a + sizeOf (unproxy (reproxyField2 a))

offset4 :: ( Storable (Field1 a)
           , Storable (Field2 a)
           , Storable (Field3 a)
           ) => t a -> Int
offset4 a = offset3 a + sizeOf (unproxy (reproxyField3 a))

offset5 :: ( Storable (Field1 a)
           , Storable (Field2 a)
           , Storable (Field3 a)
           , Storable (Field4 a)
           ) => t a -> Int
offset5 a = offset4 a + sizeOf (unproxy (reproxyField4 a))

offset6 :: ( Storable (Field1 a)
           , Storable (Field2 a)
           , Storable (Field3 a)
           , Storable (Field4 a)
           , Storable (Field5 a)
           ) => t a -> Int
offset6 a = offset5 a + sizeOf (unproxy (reproxyField5 a))

offset7 :: ( Storable (Field1 a)
           , Storable (Field2 a)
           , Storable (Field3 a)
           , Storable (Field4 a)
           , Storable (Field5 a)
           , Storable (Field6 a)
           ) => t a -> Int
offset7 a = offset6 a + sizeOf (unproxy (reproxyField6 a))

offset8 :: ( Storable (Field1 a)
           , Storable (Field2 a)
           , Storable (Field3 a)
           , Storable (Field4 a)
           , Storable (Field5 a)
           , Storable (Field6 a)
           , Storable (Field7 a)
           ) => t a -> Int
offset8 a = offset7 a + sizeOf (unproxy (reproxyField7 a))

offset9 :: ( Storable (Field1 a)
           , Storable (Field2 a)
           , Storable (Field3 a)
           , Storable (Field4 a)
           , Storable (Field5 a)
           , Storable (Field6 a)
           , Storable (Field7 a)
           , Storable (Field8 a)
           ) => t a -> Int
offset9 a = offset8 a + sizeOf (unproxy (reproxyField8 a))

unproxy :: t a -> a
unproxy = undefined

proxyFields :: t a -> Proxy a
proxyFields = reproxy

unsafeRead :: Storable a => (Proxy t -> Int) -> StorableTuple t -> IO a
unsafeRead offset t = withForeignPtr (unStorableTuple t) $ \ ptr ->
  peekByteOff ptr (offset (proxyFields t))

unsafeWrite :: Storable a => (Proxy t -> Int) -> StorableTuple t -> a -> IO ()
unsafeWrite offset t a = withForeignPtr (unStorableTuple t) $ \ ptr ->
  pokeByteOff ptr (offset (proxyFields t)) a
