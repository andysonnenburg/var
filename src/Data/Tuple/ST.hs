{-# LANGUAGE
    CPP
  , DeriveDataTypeable
  , FlexibleInstances
  , MultiParamTypeClasses #-}
module Data.Tuple.ST
       ( module Data.Tuple.MTuple
       , STTuple
       ) where

#ifdef MODULE_Control_Monad_ST_Safe
import Control.Monad.ST.Safe
import qualified Control.Monad.ST.Lazy.Safe as Lazy
#else
import Control.Monad.ST
import qualified Control.Monad.ST.Lazy as Lazy
#endif

import Data.Tuple.Array
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
