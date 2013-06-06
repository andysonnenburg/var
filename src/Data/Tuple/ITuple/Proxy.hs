{-# LANGUAGE CPP #-}
#ifdef LANGUAGE_DataKinds
{-# LANGUAGE DataKinds #-}
#endif
{-# LANGUAGE TypeOperators #-}
{- |
Copyright   :  (c) Andy Sonnenburg 2013
License     :  BSD3
Maintainer  :  andy22286@gmail.com
-}
module Data.Tuple.ITuple.Proxy
       ( proxyListRep
       , reproxyHead
       , reproxyTail
       , reproxyField1
       , reproxyField2
       , reproxyField3
       , reproxyField4
       , reproxyField5
       , reproxyField6
       , reproxyField7
       , reproxyField8
       ) where

import Data.Proxy
import Data.Tuple.ITuple

proxyListRep :: ITuple t => t -> Proxy (ListRep t)
proxyListRep _ = Proxy

reproxyHead :: t (x :| xs) -> Proxy x
reproxyHead = reproxy

reproxyTail :: t (x :| xs) -> Proxy xs
reproxyTail = reproxy

reproxyField1 :: t a -> Proxy (Field1 a)
reproxyField1 = reproxy

reproxyField2 :: t a -> Proxy (Field2 a)
reproxyField2 = reproxy

reproxyField3 :: t a -> Proxy (Field3 a)
reproxyField3 = reproxy

reproxyField4 :: t a -> Proxy (Field4 a)
reproxyField4 = reproxy

reproxyField5 :: t a -> Proxy (Field5 a)
reproxyField5 = reproxy

reproxyField6 :: t a -> Proxy (Field6 a)
reproxyField6 = reproxy

reproxyField7 :: t a -> Proxy (Field7 a)
reproxyField7 = reproxy

reproxyField8 :: t a -> Proxy (Field8 a)
reproxyField8 = reproxy
