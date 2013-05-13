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
#ifdef LANGUAGE_Trustworthy
{-# LANGUAGE Trustworthy #-}
#endif
{-# LANGUAGE
    TypeFamilies
  , TypeOperators
  , UnboxedTuples
  , UndecidableInstances #-}
{- |
Copyright   :  (c) Andy Sonnenburg 2013
License     :  BSD3
Maintainer  :  andy22286@gmail.com
-}
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
       , Field8
       , Field9
       ) where

import Control.Monad.Prim.Class

import Data.Proxy
import Data.Tuple.MTuple
import Data.Typeable (Typeable)

import GHC.Exts hiding (readArray#, writeArray#)
import qualified GHC.Exts
import GHC.Generics hiding (S)

import Type.List
import Type.Nat

import Unsafe.Coerce (unsafeCoerce)

data ArrayTuple s a = ArrayTuple (MutableArray# s Any) deriving Typeable

instance Eq (ArrayTuple s a) where
  ArrayTuple a == ArrayTuple b = sameMutableArray# a b

instance (Tuple t, MonadPrim m, s ~ World m) => MTuple (ArrayTuple s) t m where
  thawTuple a = liftPrim $ \ s -> case newArray# (sizeOf# a) undefined s of
    (# s', array #) -> case writeArray# array 0# a s' of
      s'' -> (# s'', ArrayTuple array #)
  freezeTuple (ArrayTuple array) = liftPrim $ readArray# array 0#

class Tuple a where
#ifdef LANGUAGE_DataKinds
  type ListRep a :: List *
#else
  type ListRep a
#endif

  size# :: t a -> Int#
  readArray# :: MutableArray# s Any -> Int# -> State# s -> (# State# s, a #)
  writeArray# :: MutableArray# s Any -> Int# -> a -> State# s -> State# s

  type ListRep a = GListRep (Rep a)

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

class GTuple a where
#ifdef LANGUAGE_DataKinds
  type GListRep (a :: * -> *) :: List *
#else
  type GListRep (a :: * -> *)
#endif
  gsize# :: t (a p) -> Int#
  greadArray# :: MutableArray# s Any -> Int# -> State# s -> (# State# s, a p #)
  gwriteArray# :: MutableArray# s Any -> Int# -> a p -> State# s -> State# s

instance GTuple U1 where
  type GListRep U1 = Nil
  gsize# _ = 0#
  greadArray# _ _ s = (# s, U1 #)
  gwriteArray# _ _ _ s = s

instance GTuple (K1 i c) where
  type GListRep (K1 i c) = c :| Nil
  gsize# _ = 1#
  greadArray# array i s = case GHC.Exts.readArray# array i s of
    (# s', a #) -> (# s', K1 (unsafeCoerce a) #)
  gwriteArray# array i = GHC.Exts.writeArray# array i . unsafeCoerce . unK1

instance GTuple f => GTuple (M1 i c f) where
  type GListRep (M1 i c f) = GListRep f
  gsize# a = gsize# (reproxyM1 a)
  greadArray# array i s = case greadArray# array i s of
    (# s', a #) -> (# s', M1 a #)
  gwriteArray# array i = gwriteArray# array i . unM1

instance (GTuple a, GTuple b) => GTuple (a :*: b) where
  type GListRep (a :*: b) = Concat (GListRep a) (GListRep b)
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

type ToList a = ListRep a

type Field1 a = Find N0 (ToList a)
type Field2 a = Find N1 (ToList a)
type Field3 a = Find N2 (ToList a)
type Field4 a = Find N3 (ToList a)
type Field5 a = Find N4 (ToList a)
type Field6 a = Find N5 (ToList a)
type Field7 a = Find N6 (ToList a)
type Field8 a = Find N7 (ToList a)
type Field9 a = Find N8 (ToList a)

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
