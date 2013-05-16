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
import Data.Tuple.MTuple
import Data.Typeable (Typeable)

newtype STTuple s a =
  STTuple { unSTTuple :: ArrayTuple s a
          } deriving (Eq, Typeable)

instance MTuple (STTuple s) () (ST s) where
  thawTuple = fmap STTuple . thawTuple
  freezeTuple = freezeTuple . unSTTuple

instance MTuple (STTuple s) (a, b) (ST s) where
  thawTuple = fmap STTuple . thawTuple
  freezeTuple = freezeTuple . unSTTuple

instance MTuple (STTuple s) (a, b, c) (ST s) where
  thawTuple = fmap STTuple . thawTuple
  freezeTuple = freezeTuple . unSTTuple

instance MTuple (STTuple s) (a, b, c, d) (ST s) where
  thawTuple = fmap STTuple . thawTuple
  freezeTuple = freezeTuple . unSTTuple

instance MTuple (STTuple s) (a, b, c, d, e) (ST s) where
  thawTuple = fmap STTuple . thawTuple
  freezeTuple = freezeTuple . unSTTuple

instance MTuple (STTuple s) (a, b, c, d, e, f) (ST s) where
  thawTuple = fmap STTuple . thawTuple
  freezeTuple = freezeTuple . unSTTuple

instance MTuple (STTuple s) (a, b, c, d, e, f, g) (ST s) where
  thawTuple = fmap STTuple . thawTuple
  freezeTuple = freezeTuple . unSTTuple

instance MField1 (STTuple s) (a, b) a (ST s) where
  read1 = read1 . unSTTuple
  write1 = write1 . unSTTuple

instance MField1 (STTuple s) (a, b, c) a (ST s) where
  read1 = read1 . unSTTuple
  write1 = write1 . unSTTuple

instance MField1 (STTuple s) (a, b, c, d) a (ST s) where
  read1 = read1 . unSTTuple
  write1 = write1 . unSTTuple

instance MField1 (STTuple s) (a, b, c, d, e) a (ST s) where
  read1 = read1 . unSTTuple
  write1 = write1 . unSTTuple

instance MField1 (STTuple s) (a, b, c, d, e, f) a (ST s) where
  read1 = read1 . unSTTuple
  write1 = write1 . unSTTuple

instance MField1 (STTuple s) (a, b, c, d, e, f, g) a (ST s) where
  read1 = read1 . unSTTuple
  write1 = write1 . unSTTuple

instance MField2 (STTuple s) (a, b) b (ST s) where
  read2 = read2 . unSTTuple
  write2 = write2 . unSTTuple

instance MField2 (STTuple s) (a, b, c) b (ST s) where
  read2 = read2 . unSTTuple
  write2 = write2 . unSTTuple

instance MField2 (STTuple s) (a, b, c, d) b (ST s) where
  read2 = read2 . unSTTuple
  write2 = write2 . unSTTuple

instance MField2 (STTuple s) (a, b, c, d, e) b (ST s) where
  read2 = read2 . unSTTuple
  write2 = write2 . unSTTuple

instance MField2 (STTuple s) (a, b, c, d, e, f) b (ST s) where
  read2 = read2 . unSTTuple
  write2 = write2 . unSTTuple

instance MField2 (STTuple s) (a, b, c, d, e, f, g) b (ST s) where
  read2 = read2 . unSTTuple
  write2 = write2 . unSTTuple

instance MField3 (STTuple s) (a, b, c) c (ST s) where
  read3 = read3 . unSTTuple
  write3 = write3 . unSTTuple

instance MField3 (STTuple s) (a, b, c, d) c (ST s) where
  read3 = read3 . unSTTuple
  write3 = write3 . unSTTuple

instance MField3 (STTuple s) (a, b, c, d, e) c (ST s) where
  read3 = read3 . unSTTuple
  write3 = write3 . unSTTuple

instance MField3 (STTuple s) (a, b, c, d, e, f) c (ST s) where
  read3 = read3 . unSTTuple
  write3 = write3 . unSTTuple

instance MField3 (STTuple s) (a, b, c, d, e, f, g) c (ST s) where
  read3 = read3 . unSTTuple
  write3 = write3 . unSTTuple

instance MField4 (STTuple s) (a, b, c, d) d (ST s) where
  read4 = read4 . unSTTuple
  write4 = write4 . unSTTuple

instance MField4 (STTuple s) (a, b, c, d, e) d (ST s) where
  read4 = read4 . unSTTuple
  write4 = write4 . unSTTuple

instance MField4 (STTuple s) (a, b, c, d, e, f) d (ST s) where
  read4 = read4 . unSTTuple
  write4 = write4 . unSTTuple

instance MField4 (STTuple s) (a, b, c, d, e, f, g) d (ST s) where
  read4 = read4 . unSTTuple
  write4 = write4 . unSTTuple

instance MField5 (STTuple s) (a, b, c, d, e) e (ST s) where
  read5 = read5 . unSTTuple
  write5 = write5 . unSTTuple

instance MField5 (STTuple s) (a, b, c, d, e, f) e (ST s) where
  read5 = read5 . unSTTuple
  write5 = write5 . unSTTuple

instance MField5 (STTuple s) (a, b, c, d, e, f, g) e (ST s) where
  read5 = read5 . unSTTuple
  write5 = write5 . unSTTuple

instance MField6 (STTuple s) (a, b, c, d, e, f) f (ST s) where
  read6 = read6 . unSTTuple
  write6 = write6 . unSTTuple

instance MField6 (STTuple s) (a, b, c, d, e, f, g) f (ST s) where
  read6 = read6 . unSTTuple
  write6 = write6 . unSTTuple

instance MField7 (STTuple s) (a, b, c, d, e, f, g) g (ST s) where
  read7 = read7 . unSTTuple
  write7 = write7 . unSTTuple

instance MTuple (STTuple s) () (Lazy.ST s) where
  thawTuple = fmap STTuple . thawTuple
  freezeTuple = freezeTuple . unSTTuple

instance MTuple (STTuple s) (a, b) (Lazy.ST s) where
  thawTuple = fmap STTuple . thawTuple
  freezeTuple = freezeTuple . unSTTuple

instance MTuple (STTuple s) (a, b, c) (Lazy.ST s) where
  thawTuple = fmap STTuple . thawTuple
  freezeTuple = freezeTuple . unSTTuple

instance MTuple (STTuple s) (a, b, c, d) (Lazy.ST s) where
  thawTuple = fmap STTuple . thawTuple
  freezeTuple = freezeTuple . unSTTuple

instance MTuple (STTuple s) (a, b, c, d, e) (Lazy.ST s) where
  thawTuple = fmap STTuple . thawTuple
  freezeTuple = freezeTuple . unSTTuple

instance MTuple (STTuple s) (a, b, c, d, e, f) (Lazy.ST s) where
  thawTuple = fmap STTuple . thawTuple
  freezeTuple = freezeTuple . unSTTuple

instance MTuple (STTuple s) (a, b, c, d, e, f, g) (Lazy.ST s) where
  thawTuple = fmap STTuple . thawTuple
  freezeTuple = freezeTuple . unSTTuple

instance MField1 (STTuple s) (a, b) a (Lazy.ST s) where
  read1 = read1 . unSTTuple
  write1 = write1 . unSTTuple

instance MField1 (STTuple s) (a, b, c) a (Lazy.ST s) where
  read1 = read1 . unSTTuple
  write1 = write1 . unSTTuple

instance MField1 (STTuple s) (a, b, c, d) a (Lazy.ST s) where
  read1 = read1 . unSTTuple
  write1 = write1 . unSTTuple

instance MField1 (STTuple s) (a, b, c, d, e) a (Lazy.ST s) where
  read1 = read1 . unSTTuple
  write1 = write1 . unSTTuple

instance MField1 (STTuple s) (a, b, c, d, e, f) a (Lazy.ST s) where
  read1 = read1 . unSTTuple
  write1 = write1 . unSTTuple

instance MField1 (STTuple s) (a, b, c, d, e, f, g) a (Lazy.ST s) where
  read1 = read1 . unSTTuple
  write1 = write1 . unSTTuple

instance MField2 (STTuple s) (a, b) b (Lazy.ST s) where
  read2 = read2 . unSTTuple
  write2 = write2 . unSTTuple

instance MField2 (STTuple s) (a, b, c) b (Lazy.ST s) where
  read2 = read2 . unSTTuple
  write2 = write2 . unSTTuple

instance MField2 (STTuple s) (a, b, c, d) b (Lazy.ST s) where
  read2 = read2 . unSTTuple
  write2 = write2 . unSTTuple

instance MField2 (STTuple s) (a, b, c, d, e) b (Lazy.ST s) where
  read2 = read2 . unSTTuple
  write2 = write2 . unSTTuple

instance MField2 (STTuple s) (a, b, c, d, e, f) b (Lazy.ST s) where
  read2 = read2 . unSTTuple
  write2 = write2 . unSTTuple

instance MField2 (STTuple s) (a, b, c, d, e, f, g) b (Lazy.ST s) where
  read2 = read2 . unSTTuple
  write2 = write2 . unSTTuple

instance MField3 (STTuple s) (a, b, c) c (Lazy.ST s) where
  read3 = read3 . unSTTuple
  write3 = write3 . unSTTuple

instance MField3 (STTuple s) (a, b, c, d) c (Lazy.ST s) where
  read3 = read3 . unSTTuple
  write3 = write3 . unSTTuple

instance MField3 (STTuple s) (a, b, c, d, e) c (Lazy.ST s) where
  read3 = read3 . unSTTuple
  write3 = write3 . unSTTuple

instance MField3 (STTuple s) (a, b, c, d, e, f) c (Lazy.ST s) where
  read3 = read3 . unSTTuple
  write3 = write3 . unSTTuple

instance MField3 (STTuple s) (a, b, c, d, e, f, g) c (Lazy.ST s) where
  read3 = read3 . unSTTuple
  write3 = write3 . unSTTuple

instance MField4 (STTuple s) (a, b, c, d) d (Lazy.ST s) where
  read4 = read4 . unSTTuple
  write4 = write4 . unSTTuple

instance MField4 (STTuple s) (a, b, c, d, e) d (Lazy.ST s) where
  read4 = read4 . unSTTuple
  write4 = write4 . unSTTuple

instance MField4 (STTuple s) (a, b, c, d, e, f) d (Lazy.ST s) where
  read4 = read4 . unSTTuple
  write4 = write4 . unSTTuple

instance MField4 (STTuple s) (a, b, c, d, e, f, g) d (Lazy.ST s) where
  read4 = read4 . unSTTuple
  write4 = write4 . unSTTuple

instance MField5 (STTuple s) (a, b, c, d, e) e (Lazy.ST s) where
  read5 = read5 . unSTTuple
  write5 = write5 . unSTTuple

instance MField5 (STTuple s) (a, b, c, d, e, f) e (Lazy.ST s) where
  read5 = read5 . unSTTuple
  write5 = write5 . unSTTuple

instance MField5 (STTuple s) (a, b, c, d, e, f, g) e (Lazy.ST s) where
  read5 = read5 . unSTTuple
  write5 = write5 . unSTTuple

instance MField6 (STTuple s) (a, b, c, d, e, f) f (Lazy.ST s) where
  read6 = read6 . unSTTuple
  write6 = write6 . unSTTuple

instance MField6 (STTuple s) (a, b, c, d, e, f, g) f (Lazy.ST s) where
  read6 = read6 . unSTTuple
  write6 = write6 . unSTTuple

instance MField7 (STTuple s) (a, b, c, d, e, f, g) g (Lazy.ST s) where
  read7 = read7 . unSTTuple
  write7 = write7 . unSTTuple

newtype STUTuple s a =
  STUTuple { unSTUTuple :: ByteArrayTuple s a
           } deriving (Eq, Typeable)

instance MTuple (STUTuple s) () (ST s) where
  thawTuple = fmap STUTuple . thawTuple
  freezeTuple = freezeTuple . unSTUTuple

instance ( ByteArraySlice a
         , ByteArraySlice b
         ) => MTuple (STUTuple s) (a, b) (ST s) where
  thawTuple = fmap STUTuple . thawTuple
  freezeTuple = freezeTuple . unSTUTuple

instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         ) => MTuple (STUTuple s) (a, b, c) (ST s) where
  thawTuple = fmap STUTuple . thawTuple
  freezeTuple = freezeTuple . unSTUTuple

instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         , ByteArraySlice d
         ) => MTuple (STUTuple s) (a, b, c, d) (ST s) where
  thawTuple = fmap STUTuple . thawTuple
  freezeTuple = freezeTuple . unSTUTuple

instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         , ByteArraySlice d
         , ByteArraySlice e
         ) => MTuple (STUTuple s) (a, b, c, d, e) (ST s) where
  thawTuple = fmap STUTuple . thawTuple
  freezeTuple = freezeTuple . unSTUTuple

instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         , ByteArraySlice d
         , ByteArraySlice e
         , ByteArraySlice f
         ) => MTuple (STUTuple s) (a, b, c, d, e, f) (ST s) where
  thawTuple = fmap STUTuple . thawTuple
  freezeTuple = freezeTuple . unSTUTuple

instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         , ByteArraySlice d
         , ByteArraySlice e
         , ByteArraySlice f
         , ByteArraySlice g
         ) => MTuple (STUTuple s) (a, b, c, d, e, f, g) (ST s) where
  thawTuple = fmap STUTuple . thawTuple
  freezeTuple = freezeTuple . unSTUTuple

instance ( ByteArraySlice a
         , ByteArraySlice b
         ) => MField1 (STUTuple s) (a, b) a (ST s) where
  read1 = read1 . unSTUTuple
  write1 = write1 . unSTUTuple

instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         ) => MField1 (STUTuple s) (a, b, c) a (ST s) where
  read1 = read1 . unSTUTuple
  write1 = write1 . unSTUTuple

instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         , ByteArraySlice d
         ) => MField1 (STUTuple s) (a, b, c, d) a (ST s) where
  read1 = read1 . unSTUTuple
  write1 = write1 . unSTUTuple

instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         , ByteArraySlice d
         , ByteArraySlice e
         ) => MField1 (STUTuple s) (a, b, c, d, e) a (ST s) where
  read1 = read1 . unSTUTuple
  write1 = write1 . unSTUTuple

instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         , ByteArraySlice d
         , ByteArraySlice e
         , ByteArraySlice f
         ) => MField1 (STUTuple s) (a, b, c, d, e, f) a (ST s) where
  read1 = read1 . unSTUTuple
  write1 = write1 . unSTUTuple

instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         , ByteArraySlice d
         , ByteArraySlice e
         , ByteArraySlice f
         , ByteArraySlice g
         ) => MField1 (STUTuple s) (a, b, c, d, e, f, g) a (ST s) where
  read1 = read1 . unSTUTuple
  write1 = write1 . unSTUTuple

instance ( ByteArraySlice a
         , ByteArraySlice b
         ) => MField2 (STUTuple s) (a, b) b (ST s) where
  read2 = read2 . unSTUTuple
  write2 = write2 . unSTUTuple

instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         ) => MField2 (STUTuple s) (a, b, c) b (ST s) where
  read2 = read2 . unSTUTuple
  write2 = write2 . unSTUTuple

instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         , ByteArraySlice d
         ) => MField2 (STUTuple s) (a, b, c, d) b (ST s) where
  read2 = read2 . unSTUTuple
  write2 = write2 . unSTUTuple

instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         , ByteArraySlice d
         , ByteArraySlice e
         ) => MField2 (STUTuple s) (a, b, c, d, e) b (ST s) where
  read2 = read2 . unSTUTuple
  write2 = write2 . unSTUTuple

instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         , ByteArraySlice d
         , ByteArraySlice e
         , ByteArraySlice f
         ) => MField2 (STUTuple s) (a, b, c, d, e, f) b (ST s) where
  read2 = read2 . unSTUTuple
  write2 = write2 . unSTUTuple

instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         , ByteArraySlice d
         , ByteArraySlice e
         , ByteArraySlice f
         , ByteArraySlice g
         ) => MField2 (STUTuple s) (a, b, c, d, e, f, g) b (ST s) where
  read2 = read2 . unSTUTuple
  write2 = write2 . unSTUTuple

instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         ) => MField3 (STUTuple s) (a, b, c) c (ST s) where
  read3 = read3 . unSTUTuple
  write3 = write3 . unSTUTuple

instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         , ByteArraySlice d
         ) => MField3 (STUTuple s) (a, b, c, d) c (ST s) where
  read3 = read3 . unSTUTuple
  write3 = write3 . unSTUTuple

instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         , ByteArraySlice d
         , ByteArraySlice e
         ) => MField3 (STUTuple s) (a, b, c, d, e) c (ST s) where
  read3 = read3 . unSTUTuple
  write3 = write3 . unSTUTuple

instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         , ByteArraySlice d
         , ByteArraySlice e
         , ByteArraySlice f
         ) => MField3 (STUTuple s) (a, b, c, d, e, f) c (ST s) where
  read3 = read3 . unSTUTuple
  write3 = write3 . unSTUTuple

instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         , ByteArraySlice d
         , ByteArraySlice e
         , ByteArraySlice f
         , ByteArraySlice g
         ) => MField3 (STUTuple s) (a, b, c, d, e, f, g) c (ST s) where
  read3 = read3 . unSTUTuple
  write3 = write3 . unSTUTuple

instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         , ByteArraySlice d
         ) => MField4 (STUTuple s) (a, b, c, d) d (ST s) where
  read4 = read4 . unSTUTuple
  write4 = write4 . unSTUTuple

instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         , ByteArraySlice d
         , ByteArraySlice e
         ) => MField4 (STUTuple s) (a, b, c, d, e) d (ST s) where
  read4 = read4 . unSTUTuple
  write4 = write4 . unSTUTuple

instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         , ByteArraySlice d
         , ByteArraySlice e
         , ByteArraySlice f
         ) => MField4 (STUTuple s) (a, b, c, d, e, f) d (ST s) where
  read4 = read4 . unSTUTuple
  write4 = write4 . unSTUTuple

instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         , ByteArraySlice d
         , ByteArraySlice e
         , ByteArraySlice f
         , ByteArraySlice g
         ) => MField4 (STUTuple s) (a, b, c, d, e, f, g) d (ST s) where
  read4 = read4 . unSTUTuple
  write4 = write4 . unSTUTuple

instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         , ByteArraySlice d
         , ByteArraySlice e
         ) => MField5 (STUTuple s) (a, b, c, d, e) e (ST s) where
  read5 = read5 . unSTUTuple
  write5 = write5 . unSTUTuple

instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         , ByteArraySlice d
         , ByteArraySlice e
         , ByteArraySlice f
         ) => MField5 (STUTuple s) (a, b, c, d, e, f) e (ST s) where
  read5 = read5 . unSTUTuple
  write5 = write5 . unSTUTuple

instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         , ByteArraySlice d
         , ByteArraySlice e
         , ByteArraySlice f
         , ByteArraySlice g
         ) => MField5 (STUTuple s) (a, b, c, d, e, f, g) e (ST s) where
  read5 = read5 . unSTUTuple
  write5 = write5 . unSTUTuple

instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         , ByteArraySlice d
         , ByteArraySlice e
         , ByteArraySlice f
         ) => MField6 (STUTuple s) (a, b, c, d, e, f) f (ST s) where
  read6 = read6 . unSTUTuple
  write6 = write6 . unSTUTuple

instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         , ByteArraySlice d
         , ByteArraySlice e
         , ByteArraySlice f
         , ByteArraySlice g
         ) => MField6 (STUTuple s) (a, b, c, d, e, f, g) f (ST s) where
  read6 = read6 . unSTUTuple
  write6 = write6 . unSTUTuple

instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         , ByteArraySlice d
         , ByteArraySlice e
         , ByteArraySlice f
         , ByteArraySlice g
         ) => MField7 (STUTuple s) (a, b, c, d, e, f, g) g (ST s) where
  read7 = read7 . unSTUTuple
  write7 = write7 . unSTUTuple


instance MTuple (STUTuple s) () (Lazy.ST s) where
  thawTuple = fmap STUTuple . thawTuple
  freezeTuple = freezeTuple . unSTUTuple

instance ( ByteArraySlice a
         , ByteArraySlice b
         ) => MTuple (STUTuple s) (a, b) (Lazy.ST s) where
  thawTuple = fmap STUTuple . thawTuple
  freezeTuple = freezeTuple . unSTUTuple

instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         ) => MTuple (STUTuple s) (a, b, c) (Lazy.ST s) where
  thawTuple = fmap STUTuple . thawTuple
  freezeTuple = freezeTuple . unSTUTuple

instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         , ByteArraySlice d
         ) => MTuple (STUTuple s) (a, b, c, d) (Lazy.ST s) where
  thawTuple = fmap STUTuple . thawTuple
  freezeTuple = freezeTuple . unSTUTuple

instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         , ByteArraySlice d
         , ByteArraySlice e
         ) => MTuple (STUTuple s) (a, b, c, d, e) (Lazy.ST s) where
  thawTuple = fmap STUTuple . thawTuple
  freezeTuple = freezeTuple . unSTUTuple

instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         , ByteArraySlice d
         , ByteArraySlice e
         , ByteArraySlice f
         ) => MTuple (STUTuple s) (a, b, c, d, e, f) (Lazy.ST s) where
  thawTuple = fmap STUTuple . thawTuple
  freezeTuple = freezeTuple . unSTUTuple

instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         , ByteArraySlice d
         , ByteArraySlice e
         , ByteArraySlice f
         , ByteArraySlice g
         ) => MTuple (STUTuple s) (a, b, c, d, e, f, g) (Lazy.ST s) where
  thawTuple = fmap STUTuple . thawTuple
  freezeTuple = freezeTuple . unSTUTuple

instance ( ByteArraySlice a
         , ByteArraySlice b
         ) => MField1 (STUTuple s) (a, b) a (Lazy.ST s) where
  read1 = read1 . unSTUTuple
  write1 = write1 . unSTUTuple

instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         ) => MField1 (STUTuple s) (a, b, c) a (Lazy.ST s) where
  read1 = read1 . unSTUTuple
  write1 = write1 . unSTUTuple

instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         , ByteArraySlice d
         ) => MField1 (STUTuple s) (a, b, c, d) a (Lazy.ST s) where
  read1 = read1 . unSTUTuple
  write1 = write1 . unSTUTuple

instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         , ByteArraySlice d
         , ByteArraySlice e
         ) => MField1 (STUTuple s) (a, b, c, d, e) a (Lazy.ST s) where
  read1 = read1 . unSTUTuple
  write1 = write1 . unSTUTuple

instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         , ByteArraySlice d
         , ByteArraySlice e
         , ByteArraySlice f
         ) => MField1 (STUTuple s) (a, b, c, d, e, f) a (Lazy.ST s) where
  read1 = read1 . unSTUTuple
  write1 = write1 . unSTUTuple

instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         , ByteArraySlice d
         , ByteArraySlice e
         , ByteArraySlice f
         , ByteArraySlice g
         ) => MField1 (STUTuple s) (a, b, c, d, e, f, g) a (Lazy.ST s) where
  read1 = read1 . unSTUTuple
  write1 = write1 . unSTUTuple

instance ( ByteArraySlice a
         , ByteArraySlice b
         ) => MField2 (STUTuple s) (a, b) b (Lazy.ST s) where
  read2 = read2 . unSTUTuple
  write2 = write2 . unSTUTuple

instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         ) => MField2 (STUTuple s) (a, b, c) b (Lazy.ST s) where
  read2 = read2 . unSTUTuple
  write2 = write2 . unSTUTuple

instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         , ByteArraySlice d
         ) => MField2 (STUTuple s) (a, b, c, d) b (Lazy.ST s) where
  read2 = read2 . unSTUTuple
  write2 = write2 . unSTUTuple

instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         , ByteArraySlice d
         , ByteArraySlice e
         ) => MField2 (STUTuple s) (a, b, c, d, e) b (Lazy.ST s) where
  read2 = read2 . unSTUTuple
  write2 = write2 . unSTUTuple

instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         , ByteArraySlice d
         , ByteArraySlice e
         , ByteArraySlice f
         ) => MField2 (STUTuple s) (a, b, c, d, e, f) b (Lazy.ST s) where
  read2 = read2 . unSTUTuple
  write2 = write2 . unSTUTuple

instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         , ByteArraySlice d
         , ByteArraySlice e
         , ByteArraySlice f
         , ByteArraySlice g
         ) => MField2 (STUTuple s) (a, b, c, d, e, f, g) b (Lazy.ST s) where
  read2 = read2 . unSTUTuple
  write2 = write2 . unSTUTuple

instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         ) => MField3 (STUTuple s) (a, b, c) c (Lazy.ST s) where
  read3 = read3 . unSTUTuple
  write3 = write3 . unSTUTuple

instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         , ByteArraySlice d
         ) => MField3 (STUTuple s) (a, b, c, d) c (Lazy.ST s) where
  read3 = read3 . unSTUTuple
  write3 = write3 . unSTUTuple

instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         , ByteArraySlice d
         , ByteArraySlice e
         ) => MField3 (STUTuple s) (a, b, c, d, e) c (Lazy.ST s) where
  read3 = read3 . unSTUTuple
  write3 = write3 . unSTUTuple

instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         , ByteArraySlice d
         , ByteArraySlice e
         , ByteArraySlice f
         ) => MField3 (STUTuple s) (a, b, c, d, e, f) c (Lazy.ST s) where
  read3 = read3 . unSTUTuple
  write3 = write3 . unSTUTuple

instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         , ByteArraySlice d
         , ByteArraySlice e
         , ByteArraySlice f
         , ByteArraySlice g
         ) => MField3 (STUTuple s) (a, b, c, d, e, f, g) c (Lazy.ST s) where
  read3 = read3 . unSTUTuple
  write3 = write3 . unSTUTuple

instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         , ByteArraySlice d
         ) => MField4 (STUTuple s) (a, b, c, d) d (Lazy.ST s) where
  read4 = read4 . unSTUTuple
  write4 = write4 . unSTUTuple

instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         , ByteArraySlice d
         , ByteArraySlice e
         ) => MField4 (STUTuple s) (a, b, c, d, e) d (Lazy.ST s) where
  read4 = read4 . unSTUTuple
  write4 = write4 . unSTUTuple

instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         , ByteArraySlice d
         , ByteArraySlice e
         , ByteArraySlice f
         ) => MField4 (STUTuple s) (a, b, c, d, e, f) d (Lazy.ST s) where
  read4 = read4 . unSTUTuple
  write4 = write4 . unSTUTuple

instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         , ByteArraySlice d
         , ByteArraySlice e
         , ByteArraySlice f
         , ByteArraySlice g
         ) => MField4 (STUTuple s) (a, b, c, d, e, f, g) d (Lazy.ST s) where
  read4 = read4 . unSTUTuple
  write4 = write4 . unSTUTuple

instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         , ByteArraySlice d
         , ByteArraySlice e
         ) => MField5 (STUTuple s) (a, b, c, d, e) e (Lazy.ST s) where
  read5 = read5 . unSTUTuple
  write5 = write5 . unSTUTuple

instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         , ByteArraySlice d
         , ByteArraySlice e
         , ByteArraySlice f
         ) => MField5 (STUTuple s) (a, b, c, d, e, f) e (Lazy.ST s) where
  read5 = read5 . unSTUTuple
  write5 = write5 . unSTUTuple

instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         , ByteArraySlice d
         , ByteArraySlice e
         , ByteArraySlice f
         , ByteArraySlice g
         ) => MField5 (STUTuple s) (a, b, c, d, e, f, g) e (Lazy.ST s) where
  read5 = read5 . unSTUTuple
  write5 = write5 . unSTUTuple

instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         , ByteArraySlice d
         , ByteArraySlice e
         , ByteArraySlice f
         ) => MField6 (STUTuple s) (a, b, c, d, e, f) f (Lazy.ST s) where
  read6 = read6 . unSTUTuple
  write6 = write6 . unSTUTuple

instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         , ByteArraySlice d
         , ByteArraySlice e
         , ByteArraySlice f
         , ByteArraySlice g
         ) => MField6 (STUTuple s) (a, b, c, d, e, f, g) f (Lazy.ST s) where
  read6 = read6 . unSTUTuple
  write6 = write6 . unSTUTuple

instance ( ByteArraySlice a
         , ByteArraySlice b
         , ByteArraySlice c
         , ByteArraySlice d
         , ByteArraySlice e
         , ByteArraySlice f
         , ByteArraySlice g
         ) => MField7 (STUTuple s) (a, b, c, d, e, f, g) g (Lazy.ST s) where
  read7 = read7 . unSTUTuple
  write7 = write7 . unSTUTuple
