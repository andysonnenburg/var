{-# LANGUAGE CPP, DeriveDataTypeable #-}
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
