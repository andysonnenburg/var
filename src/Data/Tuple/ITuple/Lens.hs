{-# LANGUAGE CPP #-}
#ifdef LANGUAGE_DataKinds
{-# LANGUAGE DataKinds #-}
#endif
{-# LANGUAGE GADTs, TypeOperators #-}
{- |
Copyright   :  (c) Andy Sonnenburg 2013
License     :  BSD3
Maintainer  :  andy22286@gmail.com
-}
module Data.Tuple.ITuple.Lens
       ( _1, _2, _3, _4, _5, _6, _7, _8, _9, tuple, _head, _tail
       ) where

import Control.Applicative

import Data.Tuple.ITuple

_1 :: ( Functor f
      , ITuple s
      , ListRep s ~ (a :| as)
      , ITuple t
      , ListRep t ~ (b :| as)
      ) => (a -> f b) -> s -> f t -- ^
_1 = tuple._head

_2 :: ( Functor f
      , ITuple s
      , ListRep s ~ (a1 :| a :| as)
      , ITuple t
      , ListRep t ~ (a1 :| b :| as)
      ) => (a -> f b) -> s -> f t -- ^
_2 = tuple._tail._head

_3 :: ( Functor f
      , ITuple s
      , ListRep s ~ (a1 :| a2 :| a :| as)
      , ITuple t
      , ListRep t ~ (a1 :| a2 :| b :| as)
      ) => (a -> f b) -> s -> f t -- ^
_3 = tuple._tail._tail._head

_4 :: ( Functor f
      , ITuple s
      , ListRep s ~ (a1 :| a2 :| a3 :| a :| as)
      , ITuple t
      , ListRep t ~ (a1 :| a2 :| a3 :| b :| as)
      ) => (a -> f b) -> s -> f t -- ^
_4 = tuple._tail._tail._tail._head

_5 :: ( Functor f
      , ITuple s
      , ListRep s ~ (a1 :| a2 :| a3 :| a4 :| a :| as)
      , ITuple t
      , ListRep t ~ (a1 :| a2 :| a3 :| a4 :| b :| as)
      ) => (a -> f b) -> s -> f t -- ^
_5 = tuple._tail._tail._tail._tail._head

_6 :: ( Functor f
      , ITuple s
      , ListRep s ~ (a1 :| a2 :| a3 :| a4 :| a5 :| a :| as)
      , ITuple t
      , ListRep t ~ (a1 :| a2 :| a3 :| a4 :| a5 :| b :| as)
      ) => (a -> f b) -> s -> f t -- ^
_6 = tuple._tail._tail._tail._tail._tail._head

_7 :: ( Functor f
      , ITuple s
      , ListRep s ~ (a1 :| a2 :| a3 :| a4 :| a5 :| a6 :| a :| as)
      , ITuple t
      , ListRep t ~ (a1 :| a2 :| a3 :| a4 :| a5 :| a6 :| b :| as)
      ) => (a -> f b) -> s -> f t -- ^
_7 = tuple._tail._tail._tail._tail._tail._tail._head

_8 :: ( Functor f
      , ITuple s
      , ListRep s ~ (a1 :| a2 :| a3 :| a4 :| a5 :| a6 :| a7 :| a :| as)
      , ITuple t
      , ListRep t ~ (a1 :| a2 :| a3 :| a4 :| a5 :| a6 :| a7 :| b :| as)
      ) => (a -> f b) -> s -> f t -- ^
_8 = tuple._tail._tail._tail._tail._tail._tail._tail._head

_9 :: ( Functor f
      , ITuple s
      , ListRep s ~ (a1 :| a2 :| a3 :| a4 :| a5 :| a6 :| a7 :| a8 :| a :| as)
      , ITuple t
      , ListRep t ~ (a1 :| a2 :| a3 :| a4 :| a5 :| a6 :| a7 :| a8 :| b :| as)
      ) => (a -> f b) -> s -> f t -- ^
_9 = tuple._tail._tail._tail._tail._tail._tail._tail._tail._head

tuple :: ( Functor f
         , ITuple s
         , ITuple t
         ) => (Tuple (ListRep s) -> f (Tuple (ListRep t))) -> s -> f t
tuple f = fmap fromTuple . f . toTuple

_head :: Functor f => (x -> f y) -> Tuple (x :| xs) -> f (Tuple (y :| xs))
_head f (x :* xs) = (:* xs) <$> f x

_tail :: Functor f =>
         (Tuple xs -> f (Tuple ys)) ->
         Tuple (x :| xs) -> f (Tuple (x :| ys))
_tail f (x :* xs) = (x :*) <$> f xs
