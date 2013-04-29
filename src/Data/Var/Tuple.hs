{-# LANGUAGE
    DefaultSignatures
  , FlexibleContexts
  , FlexibleInstances
  , FunctionalDependencies
  , MultiParamTypeClasses
  , Rank2Types
  , TypeOperators
  , UndecidableInstances #-}
{- |
Copyright   :  (c) Andy Sonnenburg 2013
License     :  BSD3
Maintainer  :  andy22286@gmail.com
-}
module Data.Var.Tuple
       ( FixedSize
       , sizeOf
       , Heterogeneous (..)
       , forM
       ) where

import Control.Monad (liftM, liftM2)

import Data.Proxy

import GHC.Generics

import Prelude hiding (mapM)

sizeOf :: FixedSize a => a -> Int
sizeOf = size . proxy

class FixedSize a where
  size :: t a -> Int
  default size :: (Generic a, GFixedSize (Rep a)) => t a -> Int
  size = gsize . reproxyRep

class GFixedSize a where
  gsize :: t (a p) -> Int

instance GFixedSize U1 where
  gsize _ = 0

instance GFixedSize (K1 i c) where
  gsize _ = 1

instance GFixedSize f => GFixedSize (M1 i c f) where
  gsize = gsize . reproxyM1

instance (GFixedSize f, GFixedSize g) => GFixedSize (f :*: g) where
  gsize a = gsize (reproxyL a) + gsize (reproxyR a)

instance FixedSize ()
instance FixedSize (a, b)
instance FixedSize (a, b, c)
instance FixedSize (a, b, c, d)
instance FixedSize (a, b, c, d, e)
instance FixedSize (a, b, c, d, e, f)
instance FixedSize (a, b, c, d, e, f, g)

reproxyRep :: t a -> Proxy (Rep a p)
reproxyRep = reproxy

reproxyM1 :: t (M1 i c f p) -> Proxy (f p)
reproxyM1 = reproxy

reproxyL :: t ((f :*: g) p) -> Proxy (f p)
reproxyL = reproxy

reproxyR :: t ((f :*: g) p) -> Proxy (g p)
reproxyR = reproxy

class Heterogeneous f a b | f a -> b, b -> f a where
  mapM :: Monad m => (forall c . c -> m (f c)) -> a -> m b

  default mapM :: ( Generic a
                  , Generic b
                  , GHeterogeneous f (Rep a) (Rep b)
                  , Monad m
                  ) => (forall c . c -> m (f c)) -> a -> m b
  mapM f = liftM to . gmapM f . from

forM :: (Heterogeneous f a b, Monad m) => a -> (forall c . c -> m (f c)) -> m b
forM = flip mapM

class GHeterogeneous f a b | f a -> b, b -> f a where
  gmapM :: Monad m => (forall c . c -> m (f c)) -> a p -> m (b p)

instance GHeterogeneous f U1 U1 where
  gmapM _ U1 = return U1

instance GHeterogeneous f (K1 i c) (K1 i (f c)) where
  gmapM f = liftM K1 . f . unK1

instance GHeterogeneous f a b => GHeterogeneous f (M1 i c a) (M1 i c b) where
  gmapM f = liftM M1 . gmapM f . unM1

instance ( GHeterogeneous f a a'
         , GHeterogeneous f b b'
         ) => GHeterogeneous f (a :+: b) (a' :+: b') where
  gmapM f (L1 a) = liftM L1 (gmapM f a)
  gmapM f (R1 a) = liftM R1 (gmapM f a)

instance ( GHeterogeneous f a a'
         , GHeterogeneous f b b'
         ) => GHeterogeneous f (a :*: b) (a' :*: b') where
  gmapM f (a :*: b) = liftM2 (:*:) (gmapM f a) (gmapM f b)

instance Heterogeneous t () ()
instance Heterogeneous t (a, b) (t a, t b)
instance Heterogeneous t (a, b, c) (t a, t b, t c)
instance Heterogeneous t (a, b, c, d) (t a, t b, t c, t d)
instance Heterogeneous t (a, b, c, d, e) (t a, t b, t c, t d, t e)
instance Heterogeneous t (a, b, c, d, e, f) (t a, t b, t c, t d, t e, t f)
instance Heterogeneous t (a, b, c, d, e, f, g) (t a, t b, t c, t d, t e, t f, t g)
