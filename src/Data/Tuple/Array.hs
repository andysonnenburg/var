{-# LANGUAGE CPP #-}
#ifdef LANGUAGE_DataKinds
{-# LANGUAGE DataKinds #-}
#endif
{-# LANGUAGE DefaultSignatures, DeriveDataTypeable #-}
#ifndef LANGUAGE_DataKinds
{-# LANGUAGE EmptyDataDecls #-}
#endif
{-# LANGUAGE
    FlexibleContexts
  , FlexibleInstances
  , MagicHash
  , MultiParamTypeClasses #-}
#if defined(LANGUAGE_DataKinds) && defined(FEATURE_KindVariables)
{-# LANGUAGE PolyKinds #-}
#endif
#ifdef LANGUAGE_Trustworthy
{-# LANGUAGE Trustworthy #-}
#endif
{-# LANGUAGE
    TypeFamilies
  , TypeOperators
  , UnboxedTuples
  , UndecidableInstances #-}
module Data.Tuple.Array
       ( ArrayTuple
       , Tuple
       , Field1
       , Field2
       , Field3
       , Field4
       , Field5
       , Field6
       , Field7
       ) where

import Control.Monad.Prim.Class

import Data.Proxy
import Data.Tuple.MTuple
import Data.Typeable (Typeable)

import GHC.Exts hiding (readArray#, writeArray#)
import qualified GHC.Exts
import GHC.Generics hiding (S)

import Unsafe.Coerce (unsafeCoerce)

#if defined(LANGUAGE_DataKinds) && defined(FEATURE_KindVariables)
data ArrayTuple s (a :: *) = ArrayTuple (MutableArray# s Any) deriving Typeable
#else
data ArrayTuple s a = ArrayTuple (MutableArray# s Any) deriving Typeable
#endif

instance Eq (ArrayTuple s a) where
  ArrayTuple a == ArrayTuple b = sameMutableArray# a b

instance (Tuple t, MonadPrim m, s ~ World m) => MTuple (ArrayTuple s) t m where
  thawTuple a = liftPrim $ \ s -> case newArray# (sizeOf# a) undefined s of
    (# s', array #) -> case writeArray# array 0# a s' of
      s'' -> (# s'', ArrayTuple array #)
  freezeTuple (ArrayTuple array) = liftPrim $ readArray# array 0#

class Tuple a where
#ifdef LANGUAGE_DataKinds
  type ToList a :: List *
#else
  type ToList a
#endif
  type ToList a = GToList (Rep a)
  size# :: t a -> Int#
  readArray# :: MutableArray# s Any -> Int# -> State# s -> (# State# s, a #)
  writeArray# :: MutableArray# s Any -> Int# -> a -> State# s -> State# s

  default size# :: (Generic a, GTuple (Rep a)) => t a -> Int#
  size# a = gsize# (reproxyRep a)
  {-# INLINE size# #-}

  default readArray# :: ( Generic a
                        , GTuple (Rep a)
                        ) => MutableArray# s Any -> Int# -> State# s -> (# State# s, a #)
  readArray# array i s = case greadArray# array i s of
    (# s', a #) -> (# s', to a #)
  {-# INLINE readArray# #-}

  default writeArray# :: ( Generic a
                         , GTuple (Rep a)
                         ) => MutableArray# s Any -> Int# -> a -> State# s -> State# s
  writeArray# array i a = gwriteArray# array i (from a)
  {-# INLINE writeArray# #-}

sizeOf# :: Tuple a => a -> Int#
sizeOf# a = size# (proxy a)
{-# INLINE sizeOf# #-}

#ifdef LANGUAGE_DataKinds
type family GToList (a :: * -> *) :: List *
#else
type family GToList (a :: * -> *)
#endif

class GTuple a where
  gsize# :: t (a p) -> Int#
  greadArray# :: MutableArray# s Any -> Int# -> State# s -> (# State# s, a p #)
  gwriteArray# :: MutableArray# s Any -> Int# -> a p -> State# s -> State# s

type instance GToList U1 = Nil

instance GTuple U1 where
  gsize# _ = 0#
  greadArray# _ _ s = (# s, U1 #)
  gwriteArray# _ _ _ s = s

type instance GToList (K1 i c) = c :| Nil

instance GTuple (K1 i c) where
  gsize# _ = 1#
  greadArray# array i s = case GHC.Exts.readArray# array i s of
    (# s', a #) -> (# s', K1 (unsafeCoerce a) #)
  gwriteArray# array i = GHC.Exts.writeArray# array i . unsafeCoerce . unK1

type instance GToList (M1 i c f) = GToList f

instance GTuple f => GTuple (M1 i c f) where
  gsize# a = gsize# (reproxyM1 a)
  greadArray# array i s = case greadArray# array i s of
    (# s', a #) -> (# s', M1 a #)
  gwriteArray# array i = gwriteArray# array i . unM1

type instance GToList (a :*: b) = Concat (GToList a) (GToList b)

instance (GTuple a, GTuple b) => GTuple (a :*: b) where
  gsize# a = gsize# (reproxyFst a) +# gsize# (reproxySnd a)
  greadArray# array i s = case greadArray# array i s of
    (# s', a #) -> case greadArray# array (i +# gsizeOf# a) s' of
      (# s'', b #) -> (# s'', a :*: b #)
  gwriteArray# array i (a :*: b) s = case gwriteArray# array i a s of
    s' -> gwriteArray# array (i +# gsizeOf# a) b s'

gsizeOf# :: GTuple a => a p -> Int#
gsizeOf# a = gsize# (proxy a)
{-# INLINE gsizeOf# #-}

instance Tuple ()
instance Tuple (a, b)
instance Tuple (a, b, c)
instance Tuple (a, b, c, d)
instance Tuple (a, b, c, d, e)
instance Tuple (a, b, c, d, e, f)
instance Tuple (a, b, c, d, e, f, g)

#ifdef LANGUAGE_DataKinds
data Nat = Z | S Nat
#else
data Z
data S a
#endif

type N0 = Z
type N1 = S N0
type N2 = S N1
type N3 = S N2
type N4 = S N3
type N5 = S N4
type N6 = S N5

#ifdef LANGUAGE_DataKinds
data List a = Nil | a :| List a
#else
data Nil
data a :| b
#endif

#ifdef LANGUAGE_DataKinds
#ifdef FEATURE_KindVariables
type family Concat (xs :: List k) (ys :: List k) :: List k
#else
type family Concat (xs :: List *) (ys :: List *) :: List *
#endif
#else
type family Concat xs ys
#endif
type instance Concat Nil ys = ys
type instance Concat (x:|xs) ys = x :| Concat xs ys

#ifdef LANGUAGE_DataKinds
#ifdef FEATURE_KindVariables
type family Length (xs :: List k) :: Nat
#else
type family Length (xs :: List *) :: Nat
#endif
#else
type family Length xs
#endif
type instance Length Nil = Z
type instance Length (x:|xs) = S (Length xs)

#ifdef LANGUAGE_DataKinds
#ifdef FEATURE_KindVariables
type family Find (n :: Nat) (xs :: List k) :: k
#else
type family Find (n :: Nat) (xs :: List *)
#endif
#else
type family Find n xs
#endif
type instance Find Z (x :| xs) = x
type instance Find (S n) (x :| xs) = Find n xs

type Field1 a = Find N0 (ToList a)
type Field2 a = Find N1 (ToList a)
type Field3 a = Find N2 (ToList a)
type Field4 a = Find N3 (ToList a)
type Field5 a = Find N4 (ToList a)
type Field6 a = Find N5 (ToList a)
type Field7 a = Find N6 (ToList a)

instance ( MonadPrim m
         , s ~ World m
         , Tuple t
         , a ~ Field1 t
         ) => MField1 (ArrayTuple s) t a m where
  read1 = unsafeRead 0#
  write1 = unsafeWrite 0#

instance ( MonadPrim m
         , s ~ World m
         , Tuple t
         , a ~ Field2 t
         ) => MField2 (ArrayTuple s) t a m where
  read2 = unsafeRead 1#
  write2 = unsafeWrite 1#

instance ( MonadPrim m
         , s ~ World m
         , Tuple t
         , a ~ Field3 t
         ) => MField3 (ArrayTuple s) t a m where
  read3 = unsafeRead 2#
  write3 = unsafeWrite 2#

instance ( MonadPrim m
         , s ~ World m
         , Tuple t
         , a ~ Field4 t
         ) => MField4 (ArrayTuple s) t a m where
  read4 = unsafeRead 3#
  write4 = unsafeWrite 3#

instance ( MonadPrim m
         , s ~ World m
         , Tuple t
         , a ~ Field5 t
         ) => MField5 (ArrayTuple s) t a m where
  read5 = unsafeRead 4#
  write5 = unsafeWrite 4#

instance ( MonadPrim m
         , s ~ World m
         , Tuple t
         , a ~ Field6 t
         ) => MField6 (ArrayTuple s) t a m where
  read6 = unsafeRead 5#
  write6 = unsafeWrite 5#

instance ( MonadPrim m
         , s ~ World m
         , Tuple t
         , a ~ Field7 t
         ) => MField7 (ArrayTuple s) t a m where
  read7 = unsafeRead 6#
  write7 = unsafeWrite 6#

unsafeRead :: MonadPrim m => Int# -> ArrayTuple (World m) t -> m a
unsafeRead i (ArrayTuple array) = liftPrim $ \ s ->
  case GHC.Exts.readArray# array i s of
    (# s', a #) -> (# s', unsafeCoerce a #)

unsafeWrite :: MonadPrim m => Int# -> ArrayTuple (World m) t -> a -> m ()
unsafeWrite i (ArrayTuple array) a = liftPrim $ \ s ->
  case GHC.Exts.writeArray# array i (unsafeCoerce a) s of
    s' -> (# s', () #)
