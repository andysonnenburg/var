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
module Data.Tuple.ST
       ( module Data.Tuple.MTuple
       , STTuple
       , STUTuple
       ) where

#ifdef MODULE_Control_Monad_ST_Safe
import Control.Monad.ST.Safe
import qualified Control.Monad.ST.Lazy.Safe as Lazy
#else
import Control.Monad.ST
import qualified Control.Monad.ST.Lazy as Lazy
#endif

import Data.ByteArraySlice
import Data.Tuple.Array
import Data.Tuple.ByteArray
import Data.Tuple.Fields
import Data.Tuple.MTuple
import Data.Typeable (Typeable)

newtype STTuple s a =
  STTuple { unSTTuple :: ArrayTuple s a
          } deriving (Eq, Typeable)

instance Fields t => MTuple (STTuple s) t (ST s) where
  thawTuple = fmap STTuple . thawTuple
  freezeTuple = freezeTuple . unSTTuple

instance (Fields t, a ~ Field1 t) => MField1 (STTuple s) t a (ST s) where
  read1 = read1 . unSTTuple
  write1 = write1 . unSTTuple

instance (Fields t, a ~ Field2 t) => MField2 (STTuple s) t a (ST s) where
  read2 = read2 . unSTTuple
  write2 = write2 . unSTTuple

instance (Fields t, a ~ Field3 t) => MField3 (STTuple s) t a (ST s) where
  read3 = read3 . unSTTuple
  write3 = write3 . unSTTuple

instance (Fields t, a ~ Field4 t) => MField4 (STTuple s) t a (ST s) where
  read4 = read4 . unSTTuple
  write4 = write4 . unSTTuple

instance (Fields t, a ~ Field5 t) => MField5 (STTuple s) t a (ST s) where
  read5 = read5 . unSTTuple
  write5 = write5 . unSTTuple

instance (Fields t, a ~ Field6 t) => MField6 (STTuple s) t a (ST s) where
  read6 = read6 . unSTTuple
  write6 = write6 . unSTTuple

instance (Fields t, a ~ Field7 t) => MField7 (STTuple s) t a (ST s) where
  read7 = read7 . unSTTuple
  write7 = write7 . unSTTuple

instance (Fields t, a ~ Field8 t) => MField8 (STTuple s) t a (ST s) where
  read8 = read8 . unSTTuple
  write8 = write8 . unSTTuple

instance (Fields t, a ~ Field9 t) => MField9 (STTuple s) t a (ST s) where
  read9 = read9 . unSTTuple
  write9 = write9 . unSTTuple

newtype STUTuple s a =
  STUTuple { unSTUTuple :: ByteArrayTuple s a
           } deriving (Eq, Typeable)

instance (Fields t, ByteArraySlice t) => MTuple (STUTuple s) t (ST s) where
  thawTuple = fmap STUTuple . thawTuple
  freezeTuple = freezeTuple . unSTUTuple

instance ( Fields t
         , ByteArraySlice t
         , a ~ Field1 t
         , ByteArraySlice a
         ) => MField1 (STUTuple s) t a (ST s) where
  read1 = read1 . unSTUTuple
  write1 = write1 . unSTUTuple

instance ( Fields t
         , ByteArraySlice t
         , ByteArraySlice (Field1 t)
         , a ~ Field2 t
         , ByteArraySlice a
         ) => MField2 (STUTuple s) t a (ST s) where
  read2 = read2 . unSTUTuple
  write2 = write2 . unSTUTuple

instance ( Fields t
         , ByteArraySlice t
         , ByteArraySlice (Field1 t)
         , ByteArraySlice (Field2 t)
         , a ~ Field3 t
         , ByteArraySlice a
         ) => MField3 (STUTuple s) t a (ST s) where
  read3 = read3 . unSTUTuple
  write3 = write3 . unSTUTuple

instance ( Fields t
         , ByteArraySlice t
         , ByteArraySlice (Field1 t)
         , ByteArraySlice (Field2 t)
         , ByteArraySlice (Field3 t)
         , a ~ Field4 t
         , ByteArraySlice a
         ) => MField4 (STUTuple s) t a (ST s) where
  read4 = read4 . unSTUTuple
  write4 = write4 . unSTUTuple

instance ( Fields t
         , ByteArraySlice t
         , ByteArraySlice (Field1 t)
         , ByteArraySlice (Field2 t)
         , ByteArraySlice (Field3 t)
         , ByteArraySlice (Field4 t)
         , a ~ Field5 t
         , ByteArraySlice a
         ) => MField5 (STUTuple s) t a (ST s) where
  read5 = read5 . unSTUTuple
  write5 = write5 . unSTUTuple

instance ( Fields t
         , ByteArraySlice t
         , ByteArraySlice (Field1 t)
         , ByteArraySlice (Field2 t)
         , ByteArraySlice (Field3 t)
         , ByteArraySlice (Field4 t)
         , ByteArraySlice (Field5 t)
         , a ~ Field6 t
         , ByteArraySlice a
         ) => MField6 (STUTuple s) t a (ST s) where
  read6 = read6 . unSTUTuple
  write6 = write6 . unSTUTuple

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
         ) => MField7 (STUTuple s) t a (ST s) where
  read7 = read7 . unSTUTuple
  write7 = write7 . unSTUTuple

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
         ) => MField8 (STUTuple s) t a (ST s) where
  read8 = read8 . unSTUTuple
  write8 = write8 . unSTUTuple

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
         ) => MField9 (STUTuple s) t a (ST s) where
  read9 = read9 . unSTUTuple
  write9 = write9 . unSTUTuple

instance Fields t => MTuple (STTuple s) t (Lazy.ST s) where
  thawTuple = fmap STTuple . thawTuple
  freezeTuple = freezeTuple . unSTTuple

instance (Fields t, a ~ Field1 t) => MField1 (STTuple s) t a (Lazy.ST s) where
  read1 = read1 . unSTTuple
  write1 = write1 . unSTTuple

instance (Fields t, a ~ Field2 t) => MField2 (STTuple s) t a (Lazy.ST s) where
  read2 = read2 . unSTTuple
  write2 = write2 . unSTTuple

instance (Fields t, a ~ Field3 t) => MField3 (STTuple s) t a (Lazy.ST s) where
  read3 = read3 . unSTTuple
  write3 = write3 . unSTTuple

instance (Fields t, a ~ Field4 t) => MField4 (STTuple s) t a (Lazy.ST s) where
  read4 = read4 . unSTTuple
  write4 = write4 . unSTTuple

instance (Fields t, a ~ Field5 t) => MField5 (STTuple s) t a (Lazy.ST s) where
  read5 = read5 . unSTTuple
  write5 = write5 . unSTTuple

instance (Fields t, a ~ Field6 t) => MField6 (STTuple s) t a (Lazy.ST s) where
  read6 = read6 . unSTTuple
  write6 = write6 . unSTTuple

instance (Fields t, a ~ Field7 t) => MField7 (STTuple s) t a (Lazy.ST s) where
  read7 = read7 . unSTTuple
  write7 = write7 . unSTTuple

instance (Fields t, a ~ Field8 t) => MField8 (STTuple s) t a (Lazy.ST s) where
  read8 = read8 . unSTTuple
  write8 = write8 . unSTTuple

instance (Fields t, a ~ Field9 t) => MField9 (STTuple s) t a (Lazy.ST s) where
  read9 = read9 . unSTTuple
  write9 = write9 . unSTTuple

instance (Fields t, ByteArraySlice t) => MTuple (STUTuple s) t (Lazy.ST s) where
  thawTuple = fmap STUTuple . thawTuple
  freezeTuple = freezeTuple . unSTUTuple

instance ( Fields t
         , ByteArraySlice t
         , a ~ Field1 t
         , ByteArraySlice a
         ) => MField1 (STUTuple s) t a (Lazy.ST s) where
  read1 = read1 . unSTUTuple
  write1 = write1 . unSTUTuple

instance ( Fields t
         , ByteArraySlice t
         , ByteArraySlice (Field1 t)
         , a ~ Field2 t
         , ByteArraySlice a
         ) => MField2 (STUTuple s) t a (Lazy.ST s) where
  read2 = read2 . unSTUTuple
  write2 = write2 . unSTUTuple

instance ( Fields t
         , ByteArraySlice t
         , ByteArraySlice (Field1 t)
         , ByteArraySlice (Field2 t)
         , a ~ Field3 t
         , ByteArraySlice a
         ) => MField3 (STUTuple s) t a (Lazy.ST s) where
  read3 = read3 . unSTUTuple
  write3 = write3 . unSTUTuple

instance ( Fields t
         , ByteArraySlice t
         , ByteArraySlice (Field1 t)
         , ByteArraySlice (Field2 t)
         , ByteArraySlice (Field3 t)
         , a ~ Field4 t
         , ByteArraySlice a
         ) => MField4 (STUTuple s) t a (Lazy.ST s) where
  read4 = read4 . unSTUTuple
  write4 = write4 . unSTUTuple

instance ( Fields t
         , ByteArraySlice t
         , ByteArraySlice (Field1 t)
         , ByteArraySlice (Field2 t)
         , ByteArraySlice (Field3 t)
         , ByteArraySlice (Field4 t)
         , a ~ Field5 t
         , ByteArraySlice a
         ) => MField5 (STUTuple s) t a (Lazy.ST s) where
  read5 = read5 . unSTUTuple
  write5 = write5 . unSTUTuple

instance ( Fields t
         , ByteArraySlice t
         , ByteArraySlice (Field1 t)
         , ByteArraySlice (Field2 t)
         , ByteArraySlice (Field3 t)
         , ByteArraySlice (Field4 t)
         , ByteArraySlice (Field5 t)
         , a ~ Field6 t
         , ByteArraySlice a
         ) => MField6 (STUTuple s) t a (Lazy.ST s) where
  read6 = read6 . unSTUTuple
  write6 = write6 . unSTUTuple

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
         ) => MField7 (STUTuple s) t a (Lazy.ST s) where
  read7 = read7 . unSTUTuple
  write7 = write7 . unSTUTuple

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
         ) => MField8 (STUTuple s) t a (Lazy.ST s) where
  read8 = read8 . unSTUTuple
  write8 = write8 . unSTUTuple

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
         ) => MField9 (STUTuple s) t a (Lazy.ST s) where
  read9 = read9 . unSTUTuple
  write9 = write9 . unSTUTuple
