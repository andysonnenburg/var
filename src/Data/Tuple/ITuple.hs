{-# LANGUAGE CPP #-}
#ifdef LANGUAGE_DataKinds
{-# LANGUAGE DataKinds #-}
#endif
{-# LANGUAGE
    DefaultSignatures
  , DeriveGeneric
  , FlexibleContexts
  , GADTs
  , TypeFamilies
  , TypeOperators
  , UndecidableInstances #-}
{- |
Copyright   :  (c) Andy Sonnenburg 2013
License     :  BSD3
Maintainer  :  andy22286@gmail.com
-}
module Data.Tuple.ITuple
       (
#ifdef LANGUAGE_DataKinds
         List (..)
#else
         Nil, (:|)
#endif
       , ITuple (..)
       , ListRepDefault
       , toTupleDefault
       , fromTupleDefault
       , Tuple (..)
       , Field1, Field2, Field3, Field4, Field5, Field6, Field7, Field8, Field9
       ) where

import Data.Functor.Identity

import GHC.Generics

import Type.List
import Type.Nat

class ITuple t where
#ifdef LANGUAGE_DataKinds
  type ListRep t :: List *
#else
  type ListRep t
#endif
  toTuple :: t -> Tuple (ListRep t)
  fromTuple :: Tuple (ListRep t) -> t

#ifdef FEATURE_TypeFamilyDefaults
  type ListRep t = ListRepDefault t
#endif
  default toTuple :: (Generic t, GITuple (Rep t)) => t -> Tuple (ListRepDefault t)
  toTuple = toTupleDefault
  default fromTuple :: (Generic t, GITuple (Rep t)) => Tuple (ListRepDefault t) -> t
  fromTuple = fromTupleDefault

type ListRepDefault t = GCons (Rep t) Nil

toTupleDefault :: (Generic t, GITuple (Rep t)) => t -> Tuple (ListRepDefault t)
toTupleDefault = flip gcons U . from

fromTupleDefault :: (Generic t, GITuple (Rep t)) => Tuple (ListRepDefault t) -> t
fromTupleDefault = guncons $ unnil . to

infixr 5 :*
data Tuple xs where
  U :: Tuple Nil
  (:*) :: x -> Tuple xs -> Tuple (x :| xs)

instance ITuple (Tuple xs) where
  type ListRep (Tuple xs) = xs
  toTuple = id
  fromTuple = id

instance ITuple (Identity a) where
  type ListRep (Identity a) = a :| Nil
  toTuple = (:* U) . runIdentity
  fromTuple = uncons $ unnil . Identity

#ifndef FEATURE_AssociatedTypeFamilyNonClassParameters
#ifdef LANGUAGE_DataKinds
type family GCons (t :: * -> *) (xs :: List *) :: List *
#else
type family GCons (t :: * -> *) xs
#endif
#endif
class GITuple t where
#ifdef FEATURE_AssociatedTypeFamilyNonClassParameters
#ifdef LANGUAGE_DataKinds
  type GCons t xs :: List *
#else
  type GCons t xs
#endif
#endif
  gcons :: t p -> Tuple xs -> Tuple (GCons t xs)
  guncons :: (t p -> Tuple xs -> r) -> Tuple (GCons t xs) -> r

#ifndef FEATURE_AssociatedTypeFamilyNonClassParameters
type instance GCons U1 xs = xs
#endif
instance GITuple U1 where
#ifdef FEATURE_AssociatedTypeFamilyNonClassParameters
  type GCons U1 xs = xs
#endif
  gcons = flip const
  guncons = ($ U1)

#ifndef FEATURE_AssociatedTypeFamilyNonClassParameters
type instance GCons (K1 i c) xs = c :| xs
#endif
instance GITuple (K1 i c) where
#ifdef FEATURE_AssociatedTypeFamilyNonClassParameters
  type GCons (K1 i c) xs = c :| xs
#endif
  gcons = (:*) . unK1
  guncons f = uncons $ \ c ys -> f (K1 c) ys

#ifndef FEATURE_AssociatedTypeFamilyNonClassParameters
type instance GCons (M1 i c f) xs = GCons f xs
#endif
instance GITuple f => GITuple (M1 i c f) where
#ifdef FEATURE_AssociatedTypeFamilyNonClassParameters
  type GCons (M1 i c f) xs = GCons f xs
#endif
  gcons = gcons . unM1
  guncons f = guncons $ f . M1

#ifndef FEATURE_AssociatedTypeFamilyNonClassParameters
type instance GCons (a :*: b) xs = GCons a (GCons b xs)
#endif
instance (GITuple a, GITuple b) => GITuple (a :*: b) where
#ifdef FEATURE_AssociatedTypeFamilyNonClassParameters
  type GCons (a :*: b) xs = GCons a (GCons b xs)
#endif
  gcons (a :*: b) = gcons a . gcons b
  guncons f = guncons $ \ a -> guncons $ \ b -> f $ a :*: b

instance ITuple ()
#ifndef FEATURE_TypeFamilyDefaults
  where type ListRep () = ListRepDefault ()
#endif
instance ITuple (a, b)
#ifndef FEATURE_TypeFamilyDefaults
  where type ListRep (a, b) = ListRepDefault (a, b)
#endif
instance ITuple (a, b, c)
#ifndef FEATURE_TypeFamilyDefaults
  where type ListRep (a, b, c) = ListRepDefault (a, b, c)
#endif
instance ITuple (a, b, c, d)
#ifndef FEATURE_TypeFamilyDefaults
  where type ListRep (a, b, c, d) = ListRepDefault (a, b, c, d)
#endif
instance ITuple (a, b, c, d, e)
#ifndef FEATURE_TypeFamilyDefaults
  where type ListRep (a, b, c, d, e) = ListRepDefault (a, b, c, d, e)
#endif
instance ITuple (a, b, c, d, e, f)
#ifndef FEATURE_TypeFamilyDefaults
  where type ListRep (a, b, c, d, e, f) = ListRepDefault (a, b, c, d, e, f)
#endif
instance ITuple (a, b, c, d, e, f, g)
#ifndef FEATURE_TypeFamilyDefaults
  where type ListRep (a, b, c, d, e, f, g) = ListRepDefault (a, b, c, d, e, f, g)
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

uncons :: (a -> Tuple as -> r) -> Tuple (a :| as) -> r
uncons f (a :* as) = f a as

unnil :: r -> Tuple Nil -> r
unnil r U = r
