{-# LANGUAGE CPP #-}
#ifdef LANGUAGE_DataKinds
{-# LANGUAGE DataKinds, PolyKinds #-}
#else
{-# LANGUAGE EmptyDataDecls #-}
#endif
{-# LANGUAGE TypeFamilies, TypeOperators #-}
{- |
Copyright   :  (c) Andy Sonnenburg 2013
License     :  BSD3
Maintainer  :  andy22286@gmail.com
-}
module Type.List
       (
#ifdef LANGUAGE_DataKinds
         List (..)
#else
         Nil, (:|)
#endif
       , Concat
       , Find
       ) where

import Type.Nat


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
type family Find (n :: Nat) (xs :: List k) :: k
#else
type family Find (n :: Nat) (xs :: List *)
#endif
#else
type family Find n xs
#endif
type instance Find Z (x :| xs) = x
type instance Find (S n) (x :| xs) = Find n xs
