{- |
Copyright   :  (c) Andy Sonnenburg 2013
License     :  BSD3
Maintainer  :  andy22286@gmail.com
-}
module Data.Tuple.ITuple.Proxy
       ( reproxyField1
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
