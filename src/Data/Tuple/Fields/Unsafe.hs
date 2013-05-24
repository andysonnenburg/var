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
  , TypeOperators
  , TypeFamilies
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
       , sizeOf
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

import Control.Monad
import Control.Monad.Prim

import Data.Prim.Array
import Data.Proxy

import GHC.Exts (Any)
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

  size :: t a -> Int
  readElemOff :: MutableArray s Any -> Int -> Prim s a
  writeElemOff :: MutableArray s Any -> Int -> a -> Prim s ()

#ifdef FEATURE_TypeFamilyDefaults
  type ListRep a = GListRep (Rep a)
#endif

  default size :: (Generic a, GFields (Rep a)) => t a -> Int
  size = gsize . reproxyRep
  {-# INLINE size #-}

  default readElemOff :: ( Generic a
                         , GFields (Rep a)
                         ) => MutableArray s Any -> Int -> Prim s a
  readElemOff array = liftM to . greadElemOff array
  {-# INLINE readElemOff #-}

  default writeElemOff :: ( Generic a
                          , GFields (Rep a)
                          ) => MutableArray s Any -> Int -> a -> Prim s ()
  writeElemOff array i = gwriteElemOff array i . from
  {-# INLINE writeElemOff #-}

sizeOf :: Fields a => a -> Int
sizeOf a = size (proxy a)
{-# INLINE sizeOf #-}

class GFields a where
#ifdef LANGUAGE_DataKinds
  type GListRep a :: List *
#else
  type GListRep a
#endif
  gsize :: t (a p) -> Int
  greadElemOff :: MutableArray s Any -> Int -> Prim s (a p)
  gwriteElemOff :: MutableArray s Any -> Int -> a p -> Prim s ()

instance GFields U1 where
  type GListRep U1 = Nil
  gsize _ = 0
  {-# INLINE gsize #-}
  greadElemOff _ _ = return U1
  {-# INLINE greadElemOff #-}
  gwriteElemOff _ _ _ = return ()
  {-# INLINE gwriteElemOff #-}

instance GFields (K1 i c) where
  type GListRep (K1 i c) = c :| Nil
  gsize _ = 1
  {-# INLINE gsize #-}
  greadElemOff array = liftM (K1 . unsafeCoerce) . readArray array
  {-# INLINE greadElemOff #-}
  gwriteElemOff array i = writeArray array i . unsafeCoerce . unK1
  {-# INLINE gwriteElemOff #-}

instance GFields f => GFields (M1 i c f) where
  type GListRep (M1 i c f) = GListRep f
  gsize = gsize . reproxyM1
  {-# INLINE gsize #-}
  greadElemOff array = liftM M1 . greadElemOff array
  {-# INLINE greadElemOff #-}
  gwriteElemOff array i = gwriteElemOff array i . unM1
  {-# INLINE gwriteElemOff #-}

instance (GFields a, GFields b) => GFields (a :*: b) where
  type GListRep (a :*: b) = Concat (GListRep a) (GListRep b)
  gsize a = gsize (reproxyFst a) + gsize (reproxySnd a)
  {-# INLINE gsize #-}
  greadElemOff array i = do
    a <- greadElemOff array i
    b <- greadElemOff array (i + gsizeOf a)
    return $ a :*: b
  {-# INLINE greadElemOff #-}
  gwriteElemOff array i (a :*: b) = do
    gwriteElemOff array i a
    gwriteElemOff array (i + gsizeOf a) b
  {-# INLINE gwriteElemOff #-}

gsizeOf :: GFields a => a p -> Int
gsizeOf = gsize . proxy
{-# INLINE gsizeOf #-}

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
