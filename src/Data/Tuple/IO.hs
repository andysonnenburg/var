{-# LANGUAGE
    CPP
  , DeriveDataTypeable
  , FlexibleInstances
  , MultiParamTypeClasses #-}
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
import Data.Tuple.MTuple
import Data.Typeable (Typeable)

newtype IOTuple a =
  IOTuple { unIOTuple :: ArrayTuple RealWorld a
          } deriving (Eq, Typeable)

instance MTuple IOTuple () IO where
  thawTuple = fmap IOTuple . thawTuple
  freezeTuple = freezeTuple . unIOTuple

instance MTuple IOTuple (a, b) IO where
  thawTuple = fmap IOTuple . thawTuple
  freezeTuple = freezeTuple . unIOTuple

instance MTuple IOTuple (a, b, c) IO where
  thawTuple = fmap IOTuple . thawTuple
  freezeTuple = freezeTuple . unIOTuple

instance MTuple IOTuple (a, b, c, d) IO where
  thawTuple = fmap IOTuple . thawTuple
  freezeTuple = freezeTuple . unIOTuple

instance MTuple IOTuple (a, b, c, d, e) IO where
  thawTuple = fmap IOTuple . thawTuple
  freezeTuple = freezeTuple . unIOTuple

instance MTuple IOTuple (a, b, c, d, e, f) IO where
  thawTuple = fmap IOTuple . thawTuple
  freezeTuple = freezeTuple . unIOTuple

instance MTuple IOTuple (a, b, c, d, e, f, g) IO where
  thawTuple = fmap IOTuple . thawTuple
  freezeTuple = freezeTuple . unIOTuple

instance MField1 IOTuple (a, b) a IO where
  read1 = read1 . unIOTuple
  write1 = write1 . unIOTuple

instance MField1 IOTuple (a, b, c) a IO where
  read1 = read1 . unIOTuple
  write1 = write1 . unIOTuple

instance MField1 IOTuple (a, b, c, d) a IO where
  read1 = read1 . unIOTuple
  write1 = write1 . unIOTuple

instance MField1 IOTuple (a, b, c, d, e) a IO where
  read1 = read1 . unIOTuple
  write1 = write1 . unIOTuple

instance MField1 IOTuple (a, b, c, d, e, f) a IO where
  read1 = read1 . unIOTuple
  write1 = write1 . unIOTuple

instance MField1 IOTuple (a, b, c, d, e, f, g) a IO where
  read1 = read1 . unIOTuple
  write1 = write1 . unIOTuple

instance MField2 IOTuple (a, b) b IO where
  read2 = read2 . unIOTuple
  write2 = write2 . unIOTuple

instance MField2 IOTuple (a, b, c) b IO where
  read2 = read2 . unIOTuple
  write2 = write2 . unIOTuple

instance MField2 IOTuple (a, b, c, d) b IO where
  read2 = read2 . unIOTuple
  write2 = write2 . unIOTuple

instance MField2 IOTuple (a, b, c, d, e) b IO where
  read2 = read2 . unIOTuple
  write2 = write2 . unIOTuple

instance MField2 IOTuple (a, b, c, d, e, f) b IO where
  read2 = read2 . unIOTuple
  write2 = write2 . unIOTuple

instance MField2 IOTuple (a, b, c, d, e, f, g) b IO where
  read2 = read2 . unIOTuple
  write2 = write2 . unIOTuple

instance MField3 IOTuple (a, b, c) c IO where
  read3 = read3 . unIOTuple
  write3 = write3 . unIOTuple

instance MField3 IOTuple (a, b, c, d) c IO where
  read3 = read3 . unIOTuple
  write3 = write3 . unIOTuple

instance MField3 IOTuple (a, b, c, d, e) c IO where
  read3 = read3 . unIOTuple
  write3 = write3 . unIOTuple

instance MField3 IOTuple (a, b, c, d, e, f) c IO where
  read3 = read3 . unIOTuple
  write3 = write3 . unIOTuple

instance MField3 IOTuple (a, b, c, d, e, f, g) c IO where
  read3 = read3 . unIOTuple
  write3 = write3 . unIOTuple

instance MField4 IOTuple (a, b, c, d) d IO where
  read4 = read4 . unIOTuple
  write4 = write4 . unIOTuple

instance MField4 IOTuple (a, b, c, d, e) d IO where
  read4 = read4 . unIOTuple
  write4 = write4 . unIOTuple

instance MField4 IOTuple (a, b, c, d, e, f) d IO where
  read4 = read4 . unIOTuple
  write4 = write4 . unIOTuple

instance MField4 IOTuple (a, b, c, d, e, f, g) d IO where
  read4 = read4 . unIOTuple
  write4 = write4 . unIOTuple

instance MField5 IOTuple (a, b, c, d, e) e IO where
  read5 = read5 . unIOTuple
  write5 = write5 . unIOTuple

instance MField5 IOTuple (a, b, c, d, e, f) e IO where
  read5 = read5 . unIOTuple
  write5 = write5 . unIOTuple

instance MField5 IOTuple (a, b, c, d, e, f, g) e IO where
  read5 = read5 . unIOTuple
  write5 = write5 . unIOTuple

instance MField6 IOTuple (a, b, c, d, e, f) f IO where
  read6 = read6 . unIOTuple
  write6 = write6 . unIOTuple

instance MField6 IOTuple (a, b, c, d, e, f, g) f IO where
  read6 = read6 . unIOTuple
  write6 = write6 . unIOTuple

instance MField7 IOTuple (a, b, c, d, e, f, g) g IO where
  read7 = read7 . unIOTuple
  write7 = write7 . unIOTuple

newtype IOUTuple a =
  IOUTuple { unIOUTuple :: ByteArrayTuple RealWorld a
           } deriving (Eq, Typeable)

instance MTuple IOUTuple () IO where
  thawTuple = fmap IOUTuple . thawTuple
  freezeTuple = freezeTuple . unIOUTuple

instance ( ByteArraySlice a
         , ByteArraySlice b
         ) => MTuple IOUTuple (a, b) IO where
  thawTuple = fmap IOUTuple . thawTuple
  freezeTuple = freezeTuple . unIOUTuple

instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         ) => MTuple IOUTuple (a, b, c) IO where
  thawTuple = fmap IOUTuple . thawTuple
  freezeTuple = freezeTuple . unIOUTuple

instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         , ByteArraySlice d
         ) => MTuple IOUTuple (a, b, c, d) IO where
  thawTuple = fmap IOUTuple . thawTuple
  freezeTuple = freezeTuple . unIOUTuple

instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         , ByteArraySlice d
         , ByteArraySlice e
         ) => MTuple IOUTuple (a, b, c, d, e) IO where
  thawTuple = fmap IOUTuple . thawTuple
  freezeTuple = freezeTuple . unIOUTuple

instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         , ByteArraySlice d
         , ByteArraySlice e
         , ByteArraySlice f
         ) => MTuple IOUTuple (a, b, c, d, e, f) IO where
  thawTuple = fmap IOUTuple . thawTuple
  freezeTuple = freezeTuple . unIOUTuple

instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         , ByteArraySlice d
         , ByteArraySlice e
         , ByteArraySlice f
         , ByteArraySlice g
         ) => MTuple IOUTuple (a, b, c, d, e, f, g) IO where
  thawTuple = fmap IOUTuple . thawTuple
  freezeTuple = freezeTuple . unIOUTuple

instance ( ByteArraySlice a
         , ByteArraySlice b
         ) => MField1 IOUTuple (a, b) a IO where
  read1 = read1 . unIOUTuple
  write1 = write1 . unIOUTuple

instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         ) => MField1 IOUTuple (a, b, c) a IO where
  read1 = read1 . unIOUTuple
  write1 = write1 . unIOUTuple

instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         , ByteArraySlice d
         ) => MField1 IOUTuple (a, b, c, d) a IO where
  read1 = read1 . unIOUTuple
  write1 = write1 . unIOUTuple

instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         , ByteArraySlice d
         , ByteArraySlice e
         ) => MField1 IOUTuple (a, b, c, d, e) a IO where
  read1 = read1 . unIOUTuple
  write1 = write1 . unIOUTuple

instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         , ByteArraySlice d
         , ByteArraySlice e
         , ByteArraySlice f
         ) => MField1 IOUTuple (a, b, c, d, e, f) a IO where
  read1 = read1 . unIOUTuple
  write1 = write1 . unIOUTuple

instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         , ByteArraySlice d
         , ByteArraySlice e
         , ByteArraySlice f
         , ByteArraySlice g
         ) => MField1 IOUTuple (a, b, c, d, e, f, g) a IO where
  read1 = read1 . unIOUTuple
  write1 = write1 . unIOUTuple

instance ( ByteArraySlice a
         , ByteArraySlice b
         ) => MField2 IOUTuple (a, b) b IO where
  read2 = read2 . unIOUTuple
  write2 = write2 . unIOUTuple

instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         ) => MField2 IOUTuple (a, b, c) b IO where
  read2 = read2 . unIOUTuple
  write2 = write2 . unIOUTuple

instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         , ByteArraySlice d
         ) => MField2 IOUTuple (a, b, c, d) b IO where
  read2 = read2 . unIOUTuple
  write2 = write2 . unIOUTuple

instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         , ByteArraySlice d
         , ByteArraySlice e
         ) => MField2 IOUTuple (a, b, c, d, e) b IO where
  read2 = read2 . unIOUTuple
  write2 = write2 . unIOUTuple

instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         , ByteArraySlice d
         , ByteArraySlice e
         , ByteArraySlice f
         ) => MField2 IOUTuple (a, b, c, d, e, f) b IO where
  read2 = read2 . unIOUTuple
  write2 = write2 . unIOUTuple

instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         , ByteArraySlice d
         , ByteArraySlice e
         , ByteArraySlice f
         , ByteArraySlice g
         ) => MField2 IOUTuple (a, b, c, d, e, f, g) b IO where
  read2 = read2 . unIOUTuple
  write2 = write2 . unIOUTuple

instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         ) => MField3 IOUTuple (a, b, c) c IO where
  read3 = read3 . unIOUTuple
  write3 = write3 . unIOUTuple

instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         , ByteArraySlice d
         ) => MField3 IOUTuple (a, b, c, d) c IO where
  read3 = read3 . unIOUTuple
  write3 = write3 . unIOUTuple

instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         , ByteArraySlice d
         , ByteArraySlice e
         ) => MField3 IOUTuple (a, b, c, d, e) c IO where
  read3 = read3 . unIOUTuple
  write3 = write3 . unIOUTuple

instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         , ByteArraySlice d
         , ByteArraySlice e
         , ByteArraySlice f
         ) => MField3 IOUTuple (a, b, c, d, e, f) c IO where
  read3 = read3 . unIOUTuple
  write3 = write3 . unIOUTuple

instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         , ByteArraySlice d
         , ByteArraySlice e
         , ByteArraySlice f
         , ByteArraySlice g
         ) => MField3 IOUTuple (a, b, c, d, e, f, g) c IO where
  read3 = read3 . unIOUTuple
  write3 = write3 . unIOUTuple

instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         , ByteArraySlice d
         ) => MField4 IOUTuple (a, b, c, d) d IO where
  read4 = read4 . unIOUTuple
  write4 = write4 . unIOUTuple

instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         , ByteArraySlice d
         , ByteArraySlice e
         ) => MField4 IOUTuple (a, b, c, d, e) d IO where
  read4 = read4 . unIOUTuple
  write4 = write4 . unIOUTuple

instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         , ByteArraySlice d
         , ByteArraySlice e
         , ByteArraySlice f
         ) => MField4 IOUTuple (a, b, c, d, e, f) d IO where
  read4 = read4 . unIOUTuple
  write4 = write4 . unIOUTuple

instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         , ByteArraySlice d
         , ByteArraySlice e
         , ByteArraySlice f
         , ByteArraySlice g
         ) => MField4 IOUTuple (a, b, c, d, e, f, g) d IO where
  read4 = read4 . unIOUTuple
  write4 = write4 . unIOUTuple

instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         , ByteArraySlice d
         , ByteArraySlice e
         ) => MField5 IOUTuple (a, b, c, d, e) e IO where
  read5 = read5 . unIOUTuple
  write5 = write5 . unIOUTuple

instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         , ByteArraySlice d
         , ByteArraySlice e
         , ByteArraySlice f
         ) => MField5 IOUTuple (a, b, c, d, e, f) e IO where
  read5 = read5 . unIOUTuple
  write5 = write5 . unIOUTuple

instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         , ByteArraySlice d
         , ByteArraySlice e
         , ByteArraySlice f
         , ByteArraySlice g
         ) => MField5 IOUTuple (a, b, c, d, e, f, g) e IO where
  read5 = read5 . unIOUTuple
  write5 = write5 . unIOUTuple

instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         , ByteArraySlice d
         , ByteArraySlice e
         , ByteArraySlice f
         ) => MField6 IOUTuple (a, b, c, d, e, f) f IO where
  read6 = read6 . unIOUTuple
  write6 = write6 . unIOUTuple

instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         , ByteArraySlice d
         , ByteArraySlice e
         , ByteArraySlice f
         , ByteArraySlice g
         ) => MField6 IOUTuple (a, b, c, d, e, f, g) f IO where
  read6 = read6 . unIOUTuple
  write6 = write6 . unIOUTuple

instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         , ByteArraySlice d
         , ByteArraySlice e
         , ByteArraySlice f
         , ByteArraySlice g
         ) => MField7 IOUTuple (a, b, c, d, e, f, g) g IO where
  read7 = read7 . unIOUTuple
  write7 = write7 . unIOUTuple
