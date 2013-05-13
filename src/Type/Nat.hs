{-# LANGUAGE CPP #-}
#ifdef LANGUAGE_DataKinds
{-# LANGUAGE DataKinds, KindSignatures #-}
#else
{-# LANGUAGE EmptyDataDecls #-}
#endif
{- |
Copyright   :  (c) Andy Sonnenburg 2013
License     :  BSD3
Maintainer  :  andy22286@gmail.com
-}
module Type.Nat
       (
#ifdef LANGUAGE_DataKinds
         Nat (..)
#else
         Z, S
#endif
       , N0, N1, N2, N3, N4, N5, N6, N7, N8
       ) where

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
type N7 = S N6
type N8 = S N7
