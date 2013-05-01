{-# LANGUAGE
    CPP
  , DefaultSignatures
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
module Data.Var.Traversable
       ( Kleisli (..)
       , Empty (..)
       , Wrap (..)
       , Traversable (..)
       , for
       ) where

import Control.Applicative

#ifdef DEPRECATED_Par0
import qualified Data.Traversable as Traversable
#endif

import GHC.Generics

class Kleisli f a b where
  runKleisli :: f m -> a -> m b

newtype Empty m = Empty (forall b . m b)

instance Kleisli Empty a b where
  runKleisli (Empty m) = const m

newtype Wrap w m = Wrap (forall a . a -> m (w a))

instance Kleisli (Wrap w) a (w a) where
  runKleisli (Wrap f) = f

class Traversable f a b | f a -> b, f b -> a where
  traverse :: Applicative m => f m -> a -> m b

  default traverse :: ( Generic a
                      , Generic b
                      , GTraversable f (Rep a) (Rep b)
                      , Applicative m
                  ) => f m -> a -> m b
  traverse f = fmap to . gtraverse f . from

for :: (Traversable f a b, Applicative m) => a -> f m -> m b
for = flip traverse

class GTraversable f a b | f a -> b, f b -> a where
  gtraverse :: Applicative m => f m -> a p -> m (b p)

instance GTraversable f U1 U1 where
  gtraverse _ U1 = pure U1

#ifdef DEPRECATED_Par0
instance Kleisli f c c' => GTraversable f (K1 i c) (K1 i c') where
  gtraverse f = fmap K1 . runKleisli f . unK1
#else
instance Traversable f c c' => GTraversable f (Rec0 c) (Rec0 c') where
  gtraverse f = fmap K1 . traverse f . unK1

instance Kleisli f c c' => GTraversable f (Par0 c) (Par0 c') where
  gtraverse f = fmap K1 . runKleisli f . unK1
#endif

instance GTraversable f a b => GTraversable f (M1 i c a) (M1 i c b) where
  gtraverse f = fmap M1 . gtraverse f . unM1

instance ( GTraversable f a a'
         , GTraversable f b b'
         ) => GTraversable f (a :+: b) (a' :+: b') where
  gtraverse f (L1 a) = L1 <$> gtraverse f a
  gtraverse f (R1 a) = R1 <$> gtraverse f a

instance ( GTraversable f a a'
         , GTraversable f b b'
         ) => GTraversable f (a :*: b) (a' :*: b') where
  gtraverse f (a :*: b) = (:*:) <$> gtraverse f a <*> gtraverse f b

instance Traversable k Bool Bool
instance Kleisli k Char Char => Traversable k Char Char
instance Kleisli k Double Double => Traversable k Double Double
instance Kleisli k Float Float => Traversable k Float Float
instance Kleisli k Int Int => Traversable k Int Int
instance Traversable k Ordering Ordering
instance Kleisli k a a' => Traversable k (Maybe a) (Maybe a')
instance ( Kleisli k a a'
         , Kleisli k b b'
         ) => Traversable k (Either a b) (Either a' b')
#ifdef DEPRECATED_Par0
instance Kleisli k a a' => Traversable k [a] [a'] where
  traverse f = Traversable.traverse (runKleisli f)
#else
instance Kleisli k a a' => Traversable k [a] [a']
#endif
instance Traversable k () ()
instance ( Kleisli k a a'
         , Kleisli k b b'
         ) => Traversable k (a, b) (a', b')
instance ( Kleisli k a a'
         , Kleisli k b b'
         , Kleisli k c c'
         ) => Traversable k (a, b, c) (a', b', c')
instance ( Kleisli k a a'
         , Kleisli k b b'
         , Kleisli k c c'
         , Kleisli k d d'
         ) => Traversable k (a, b, c, d) (a', b', c', d')
instance ( Kleisli k a a'
         , Kleisli k b b'
         , Kleisli k c c'
         , Kleisli k d d'
         , Kleisli k e e'
         ) => Traversable k (a, b, c, d, e) (a', b', c', d', e')
instance ( Kleisli k a a'
         , Kleisli k b b'
         , Kleisli k c c'
         , Kleisli k d d'
         , Kleisli k e e'
         , Kleisli k f f'
         ) => Traversable k (a, b, c, d, e, f) (a', b', c', d', e', f')
instance ( Kleisli k a a'
         , Kleisli k b b'
         , Kleisli k c c'
         , Kleisli k d d'
         , Kleisli k e e'
         , Kleisli k f f'
         , Kleisli k g g'
         ) => Traversable k (a, b, c, d, e, f, g) (a', b', c', d', e', f', g')
