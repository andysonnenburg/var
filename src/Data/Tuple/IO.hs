{-# LANGUAGE
    CPP
  , DeriveDataTypeable
  , FlexibleInstances
  , FlexibleContexts
  , MultiParamTypeClasses
  , TypeFamilies
  , UndecidableInstances #-}
{- |
Copyright   :  (c) Andy Sonnenburg 2013
License     :  BSD3
Maintainer  :  andy22286@gmail.com
-}
module Data.Tuple.IO
       ( module Data.Tuple.MTuple
       , IOTuple
       , IOUTuple
       ) where

#ifdef MODULE_Control_Monad_ST_Safe
import Control.Monad.ST.Safe (RealWorld)
#else
import Control.Monad.ST (RealWorld)
#endif

import Data.ByteArraySlice
import Data.Tuple.Array
import Data.Tuple.ByteArray
import Data.Tuple.Fields
import Data.Tuple.MTuple
import Data.Typeable (Typeable)

newtype IOTuple a =
  IOTuple { unIOTuple :: ArrayTuple RealWorld a
          } deriving (Eq, Typeable)

instance Fields t => MTuple IOTuple t IO where
  thawTuple = fmap IOTuple . thawTuple
  freezeTuple = freezeTuple . unIOTuple

instance (Fields t, a ~ Field1 t) => MField1 IOTuple t a IO where
  read1 = read1 . unIOTuple
  write1 = write1 . unIOTuple

instance (Fields t, a ~ Field2 t) => MField2 IOTuple t a IO where
  read2 = read2 . unIOTuple
  write2 = write2 . unIOTuple

instance (Fields t, a ~ Field3 t) => MField3 IOTuple t a IO where
  read3 = read3 . unIOTuple
  write3 = write3 . unIOTuple

instance (Fields t, a ~ Field4 t) => MField4 IOTuple t a IO where
  read4 = read4 . unIOTuple
  write4 = write4 . unIOTuple

instance (Fields t, a ~ Field5 t) => MField5 IOTuple t a IO where
  read5 = read5 . unIOTuple
  write5 = write5 . unIOTuple

instance (Fields t, a ~ Field6 t) => MField6 IOTuple t a IO where
  read6 = read6 . unIOTuple
  write6 = write6 . unIOTuple

instance (Fields t, a ~ Field7 t) => MField7 IOTuple t a IO where
  read7 = read7 . unIOTuple
  write7 = write7 . unIOTuple

instance (Fields t, a ~ Field8 t) => MField8 IOTuple t a IO where
  read8 = read8 . unIOTuple
  write8 = write8 . unIOTuple

instance (Fields t, a ~ Field9 t) => MField9 IOTuple t a IO where
  read9 = read9 . unIOTuple
  write9 = write9 . unIOTuple

newtype IOUTuple a =
  IOUTuple { unIOUTuple :: ByteArrayTuple RealWorld a
           } deriving (Eq, Typeable)

instance (Fields t, ByteArraySlice t) => MTuple IOUTuple t IO where
  thawTuple = fmap IOUTuple . thawTuple
  freezeTuple = freezeTuple . unIOUTuple

instance ( Fields t
         , ByteArraySlice t
         , a ~ Field1 t
         , ByteArraySlice a
         ) => MField1 IOUTuple t a IO where
  read1 = read1 . unIOUTuple
  write1 = write1 . unIOUTuple

instance ( Fields t
         , ByteArraySlice t
         , ByteArraySlice (Field1 t)
         , a ~ Field2 t
         , ByteArraySlice a
         ) => MField2 IOUTuple t a IO where
  read2 = read2 . unIOUTuple
  write2 = write2 . unIOUTuple

instance ( Fields t
         , ByteArraySlice t
         , ByteArraySlice (Field1 t)
         , ByteArraySlice (Field2 t)
         , a ~ Field3 t
         , ByteArraySlice a
         ) => MField3 IOUTuple t a IO where
  read3 = read3 . unIOUTuple
  write3 = write3 . unIOUTuple

instance ( Fields t
         , ByteArraySlice t
         , ByteArraySlice (Field1 t)
         , ByteArraySlice (Field2 t)
         , ByteArraySlice (Field3 t)
         , a ~ Field4 t
         , ByteArraySlice a
         ) => MField4 IOUTuple t a IO where
  read4 = read4 . unIOUTuple
  write4 = write4 . unIOUTuple

instance ( Fields t
         , ByteArraySlice t
         , ByteArraySlice (Field1 t)
         , ByteArraySlice (Field2 t)
         , ByteArraySlice (Field3 t)
         , ByteArraySlice (Field4 t)
         , a ~ Field5 t
         , ByteArraySlice a
         ) => MField5 IOUTuple t a IO where
  read5 = read5 . unIOUTuple
  write5 = write5 . unIOUTuple

instance ( Fields t
         , ByteArraySlice t
         , ByteArraySlice (Field1 t)
         , ByteArraySlice (Field2 t)
         , ByteArraySlice (Field3 t)
         , ByteArraySlice (Field4 t)
         , ByteArraySlice (Field5 t)
         , a ~ Field6 t
         , ByteArraySlice a
         ) => MField6 IOUTuple t a IO where
  read6 = read6 . unIOUTuple
  write6 = write6 . unIOUTuple

instance ( Fields t
         , ByteArraySlice t
         , ByteArraySlice (Field1 t)
         , ByteArraySlice (Field2 t)
         , ByteArraySlice (Field3 t)
         , ByteArraySlice (Field4 t)
         , ByteArraySlice (Field5 t)
         , ByteArraySlice (Field6 t)
         , a ~ Field7 t
         , ByteArraySlice a
         ) => MField7 IOUTuple t a IO where
  read7 = read7 . unIOUTuple
  write7 = write7 . unIOUTuple

instance ( Fields t
         , ByteArraySlice t
         , ByteArraySlice (Field1 t)
         , ByteArraySlice (Field2 t)
         , ByteArraySlice (Field3 t)
         , ByteArraySlice (Field4 t)
         , ByteArraySlice (Field5 t)
         , ByteArraySlice (Field6 t)
         , ByteArraySlice (Field7 t)
         , a ~ Field8 t
         , ByteArraySlice a
         ) => MField8 IOUTuple t a IO where
  read8 = read8 . unIOUTuple
  write8 = write8 . unIOUTuple

instance ( Fields t
         , ByteArraySlice t
         , ByteArraySlice (Field1 t)
         , ByteArraySlice (Field2 t)
         , ByteArraySlice (Field3 t)
         , ByteArraySlice (Field4 t)
         , ByteArraySlice (Field5 t)
         , ByteArraySlice (Field6 t)
         , ByteArraySlice (Field7 t)
         , ByteArraySlice (Field8 t)
         , a ~ Field9 t
         , ByteArraySlice a
         ) => MField9 IOUTuple t a IO where
  read9 = read9 . unIOUTuple
  write9 = write9 . unIOUTuple
