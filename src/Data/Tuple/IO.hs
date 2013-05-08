{-# LANGUAGE CPP, DeriveDataTypeable #-}
module Data.Tuple.IO
       ( module Data.Tuple.MTuple
       , IOTuple
       ) where

#ifdef MODULE_Control_Monad_ST_Safe
import Control.Monad.ST.Safe (RealWorld)
#else
import Control.Monad.ST (RealWorld)
#endif

import Data.Tuple.Array
import Data.Tuple.MTuple
import Data.Typeable (Typeable)

newtype IOTuple a =
  IOTuple { unIOTuple :: ArrayTuple RealWorld a
          } deriving (Eq, Typeable)
