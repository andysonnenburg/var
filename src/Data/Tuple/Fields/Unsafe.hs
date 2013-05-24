{-# LANGUAGE CPP #-}
#ifdef LANGUAGE_DataKinds
{-# LANGUAGE DataKinds #-}
#endif
{-# LANGUAGE DefaultSignatures #-}
#ifndef LANGUAGE_DataKinds
{-# LANGUAGE EmptyDataDecls #-}
#endif
{-# LANGUAGE
    FlexibleContexts
  , MagicHash
  , TypeOperators
  , TypeFamilies
  , UnboxedTuples
  , UndecidableInstances #-}
#ifdef LANGUAGE_Unsafe
{-# LANGUAGE Unsafe #-}
#endif
{- |
Copyright   :  (c) Andy Sonnenburg 2013
License     :  BSD3
Maintainer  :  andy22286@gmail.com
-}
module Data.Tuple.Fields.Unsafe
       ( Fields (..)
       , sizeOf#
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

import Data.Proxy

import GHC.Exts
import GHC.Generics

import Type.List
import Type.Nat

import Unsafe.Coerce (unsafeCoerce)

class Fields a where
#ifdef LANGUAGE_DataKinds
  type ListRep a :: List *
#else
  type ListRep a
#endif

  size# :: t a -> Int#
  readArray# :: MutableArray# s Any -> Int# -> State# s -> (# State# s, a #)
  writeArray# :: MutableArray# s Any -> Int# -> a -> State# s -> State# s

#ifdef FEATURE_TypeFamilyDefaults
  type ListRep a = GListRep (Rep a)
#endif

  default size# :: (Generic a, GFields (Rep a)) => t a -> Int#
  size# a = gsize# (reproxyRep a)
  {-# INLINE size# #-}

  default readArray# :: ( Generic a
                        , GFields (Rep a)
                        ) => MutableArray# s Any -> Int# -> State# s -> (# State# s, a #)
  readArray# array i s = case greadArray# array i s of
    (# s', a #) -> (# s', to a #)
  {-# INLINE readArray# #-}

  default writeArray# :: ( Generic a
                         , GFields (Rep a)
                         ) => MutableArray# s Any -> Int# -> a -> State# s -> State# s
  writeArray# array i a = gwriteArray# array i (from a)
  {-# INLINE writeArray# #-}

sizeOf# :: Fields a => a -> Int#
sizeOf# a = size# (proxy a)
{-# INLINE sizeOf# #-}

class GFields a where
#ifdef LANGUAGE_DataKinds
  type GListRep a :: List *
#else
  type GListRep a
#endif
  gsize# :: t (a p) -> Int#
  greadArray# :: MutableArray# s Any -> Int# -> State# s -> (# State# s, a p #)
  gwriteArray# :: MutableArray# s Any -> Int# -> a p -> State# s -> State# s

instance GFields U1 where
  type GListRep U1 = Nil
  gsize# _ = 0#
  {-# INLINE gsize# #-}
  greadArray# _ _ s = (# s, U1 #)
  {-# INLINE greadArray# #-}
  gwriteArray# _ _ _ s = s
  {-# INLINE gwriteArray# #-}

instance GFields (K1 i c) where
  type GListRep (K1 i c) = c :| Nil
  gsize# _ = 1#
  {-# INLINE gsize# #-}
  greadArray# array i s = case GHC.Exts.readArray# array i s of
    (# s', a #) -> (# s', K1 (unsafeCoerce a) #)
  {-# INLINE greadArray# #-}
  gwriteArray# array i = GHC.Exts.writeArray# array i . unsafeCoerce . unK1
  {-# INLINE gwriteArray# #-}

instance GFields f => GFields (M1 i c f) where
  type GListRep (M1 i c f) = GListRep f
  gsize# a = gsize# (reproxyM1 a)
  {-# INLINE gsize# #-}
  greadArray# array i s = case greadArray# array i s of
    (# s', a #) -> (# s', M1 a #)
  {-# INLINE greadArray# #-}
  gwriteArray# array i = gwriteArray# array i . unM1
  {-# INLINE gwriteArray# #-}

instance (GFields a, GFields b) => GFields (a :*: b) where
  type GListRep (a :*: b) = Concat (GListRep a) (GListRep b)
  gsize# a = gsize# (reproxyFst a) +# gsize# (reproxySnd a)
  {-# INLINE gsize# #-}
  greadArray# array i s = case greadArray# array i s of
    (# s', a #) -> case greadArray# array (i +# gsizeOf# a) s' of
      (# s'', b #) -> (# s'', a :*: b #)
  {-# INLINE greadArray# #-}
  gwriteArray# array i (a :*: b) s = case gwriteArray# array i a s of
    s' -> gwriteArray# array (i +# gsizeOf# a) b s'
  {-# INLINE gwriteArray# #-}

gsizeOf# :: GFields a => a p -> Int#
gsizeOf# a = gsize# (proxy a)
{-# INLINE gsizeOf# #-}

instance Fields ()
#ifndef FEATURE_TypeFamilyDefaults
  where type ListRep () = GListRep (Rep ())
#endif
instance Fields (a, b)
#ifndef FEATURE_TypeFamilyDefaults
  where type ListRep (a, b) = GListRep (Rep (a, b))
#endif
instance Fields (a, b, c)
#ifndef FEATURE_TypeFamilyDefaults
  where type ListRep (a, b, c) = GListRep (Rep (a, b, c))
#endif
instance Fields (a, b, c, d)
#ifndef FEATURE_TypeFamilyDefaults
  where type ListRep (a, b, c, d) = GListRep (Rep (a, b, c, d))
#endif
instance Fields (a, b, c, d, e)
#ifndef FEATURE_TypeFamilyDefaults
  where type ListRep (a, b, c, d, e) = GListRep (Rep (a, b, c, d, e))
#endif
instance Fields (a, b, c, d, e, f)
#ifndef FEATURE_TypeFamilyDefaults
  where type ListRep (a, b, c, d, e, f) = GListRep (Rep (a, b, c, d, e, f))
#endif
instance Fields (a, b, c, d, e, f, g)
#ifndef FEATURE_TypeFamilyDefaults
  where type ListRep (a, b, c, d, e, f, g) = GListRep (Rep (a, b, c, d, e, f, g))
#endif

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
