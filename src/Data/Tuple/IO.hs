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
       , ArraySlice
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
import Data.Tuple.ITuple
import Data.Tuple.MTuple
import Data.Typeable (Typeable)

newtype IOTuple a =
  IOTuple { unIOTuple :: ArrayTuple RealWorld a
          } deriving (Eq, Typeable)

instance ( ITuple t
         , ArraySlice (Tuple (ListRep t))
         ) => MTuple IOTuple t IO where
  thawTuple = fmap IOTuple . thawTuple
  freezeTuple = freezeTuple . unIOTuple

instance ( ITuple t
         , ArraySlice (Tuple (ListRep t))
         ) => MField1 IOTuple t IO where
  read1 = read1 . unIOTuple
  write1 = write1 . unIOTuple

instance ( ITuple t
         , ArraySlice (Tuple (ListRep t))
         ) => MField2 IOTuple t IO where
  read2 = read2 . unIOTuple
  write2 = write2 . unIOTuple

instance ( ITuple t
         , ArraySlice (Tuple (ListRep t))
         ) => MField3 IOTuple t IO where
  read3 = read3 . unIOTuple
  write3 = write3 . unIOTuple

instance ( ITuple t
         , ArraySlice (Tuple (ListRep t))
         ) => MField4 IOTuple t IO where
  read4 = read4 . unIOTuple
  write4 = write4 . unIOTuple

instance ( ITuple t
         , ArraySlice (Tuple (ListRep t))
         ) => MField5 IOTuple t IO where
  read5 = read5 . unIOTuple
  write5 = write5 . unIOTuple

instance ( ITuple t
         , ArraySlice (Tuple (ListRep t))
         ) => MField6 IOTuple t IO where
  read6 = read6 . unIOTuple
  write6 = write6 . unIOTuple

instance ( ITuple t
         , ArraySlice (Tuple (ListRep t))
         ) => MField7 IOTuple t IO where
  read7 = read7 . unIOTuple
  write7 = write7 . unIOTuple

instance ( ITuple t
         , ArraySlice (Tuple (ListRep t))
         ) => MField8 IOTuple t IO where
  read8 = read8 . unIOTuple
  write8 = write8 . unIOTuple

instance ( ITuple t
         , ArraySlice (Tuple (ListRep t))
         ) => MField9 IOTuple t IO where
  read9 = read9 . unIOTuple
  write9 = write9 . unIOTuple

newtype IOUTuple a =
  IOUTuple { unIOUTuple :: ByteArrayTuple RealWorld a
           } deriving (Eq, Typeable)

instance ( ITuple t
         , ByteArraySlice (Tuple (ListRep t))
         ) => MTuple IOUTuple t IO where
  thawTuple = fmap IOUTuple . thawTuple
  freezeTuple = freezeTuple . unIOUTuple

instance ( ITuple t
         , ByteArraySlice (Tuple (ListRep t))
         , ByteArraySlice (Field1 t)
         ) => MField1 IOUTuple t IO where
  read1 = read1 . unIOUTuple
  write1 = write1 . unIOUTuple

instance ( ITuple t
         , ByteArraySlice (Tuple (ListRep t))
         , ByteArraySlice (Field1 t)
         , ByteArraySlice (Field2 t)
         ) => MField2 IOUTuple t IO where
  read2 = read2 . unIOUTuple
  write2 = write2 . unIOUTuple

instance ( ITuple t
         , ByteArraySlice (Tuple (ListRep t))
         , ByteArraySlice (Field1 t)
         , ByteArraySlice (Field2 t)
         , ByteArraySlice (Field3 t)
         ) => MField3 IOUTuple t IO where
  read3 = read3 . unIOUTuple
  write3 = write3 . unIOUTuple

instance ( ITuple t
         , ByteArraySlice (Tuple (ListRep t))
         , ByteArraySlice (Field1 t)
         , ByteArraySlice (Field2 t)
         , ByteArraySlice (Field3 t)
         , ByteArraySlice (Field4 t)
         ) => MField4 IOUTuple t IO where
  read4 = read4 . unIOUTuple
  write4 = write4 . unIOUTuple

instance ( ITuple t
         , ByteArraySlice (Tuple (ListRep t))
         , ByteArraySlice (Field1 t)
         , ByteArraySlice (Field2 t)
         , ByteArraySlice (Field3 t)
         , ByteArraySlice (Field4 t)
         , ByteArraySlice (Field5 t)
         ) => MField5 IOUTuple t IO where
  read5 = read5 . unIOUTuple
  write5 = write5 . unIOUTuple

instance ( ITuple t
         , ByteArraySlice (Tuple (ListRep t))
         , ByteArraySlice (Field1 t)
         , ByteArraySlice (Field2 t)
         , ByteArraySlice (Field3 t)
         , ByteArraySlice (Field4 t)
         , ByteArraySlice (Field5 t)
         , ByteArraySlice (Field6 t)
         ) => MField6 IOUTuple t IO where
  read6 = read6 . unIOUTuple
  write6 = write6 . unIOUTuple

instance ( ITuple t
         , ByteArraySlice (Tuple (ListRep t))
         , ByteArraySlice (Field1 t)
         , ByteArraySlice (Field2 t)
         , ByteArraySlice (Field3 t)
         , ByteArraySlice (Field4 t)
         , ByteArraySlice (Field5 t)
         , ByteArraySlice (Field6 t)
         , ByteArraySlice (Field7 t)
         ) => MField7 IOUTuple t IO where
  read7 = read7 . unIOUTuple
  write7 = write7 . unIOUTuple

instance ( ITuple t
         , ByteArraySlice (Tuple (ListRep t))
         , ByteArraySlice (Field1 t)
         , ByteArraySlice (Field2 t)
         , ByteArraySlice (Field3 t)
         , ByteArraySlice (Field4 t)
         , ByteArraySlice (Field5 t)
         , ByteArraySlice (Field6 t)
         , ByteArraySlice (Field7 t)
         , ByteArraySlice (Field8 t)
         ) => MField8 IOUTuple t IO where
  read8 = read8 . unIOUTuple
  write8 = write8 . unIOUTuple

instance ( ITuple t
         , ByteArraySlice (Tuple (ListRep t))
         , ByteArraySlice (Field1 t)
         , ByteArraySlice (Field2 t)
         , ByteArraySlice (Field3 t)
         , ByteArraySlice (Field4 t)
         , ByteArraySlice (Field5 t)
         , ByteArraySlice (Field6 t)
         , ByteArraySlice (Field7 t)
         , ByteArraySlice (Field8 t)
         , ByteArraySlice (Field9 t)
         ) => MField9 IOUTuple t IO where
  read9 = read9 . unIOUTuple
  write9 = write9 . unIOUTuple
