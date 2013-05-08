{-# LANGUAGE
    DefaultSignatures
  , DeriveDataTypeable
  , FlexibleContexts
  , FlexibleInstances
  , MagicHash
  , MultiParamTypeClasses
  , TypeFamilies
  , UnboxedTuples #-}
module Data.Tuple.Array
       ( ArrayTuple
       , Tuple
       ) where

import Control.Monad.Prim.Class

import Data.Proxy
import Data.Tuple.MTuple
import Data.Typeable (Typeable)

import GHC.Exts hiding (readArray#, writeArray#)
import qualified GHC.Exts
import GHC.Generics

data ArrayTuple s a = ArrayTuple (MutableArray# s Any) deriving Typeable

instance Eq (ArrayTuple s a) where
  ArrayTuple a == ArrayTuple b = sameMutableArray# a b

instance (Tuple t, MonadPrim m, s ~ World m) => MTuple (ArrayTuple s) t m where
  thawTuple a = liftPrim $ \ s -> case newArray# (sizeOf# a) undefined s of
    (# s', array #) -> case writeArray# array 0# a s' of
      s'' -> (# s'', ArrayTuple array #)
  freezeTuple (ArrayTuple array) = liftPrim $ readArray# array 0#

class Tuple a where
  size# :: t a -> Int#
  readArray# :: MutableArray# s Any -> Int# -> State# s -> (# State# s, a #)
  writeArray# :: MutableArray# s Any -> Int# -> a -> State# s -> State# s

  default size# :: (Generic a, GTuple (Rep a)) => t a -> Int#
  size# a = gsize# (reproxyRep a)
  {-# INLINE size# #-}

  default readArray# :: ( Generic a
                        , GTuple (Rep a)
                        ) => MutableArray# s Any -> Int# -> State# s -> (# State# s, a #)
  readArray# array i s = case greadArray# array i s of
    (# s', a #) -> (# s', to a #)
  {-# INLINE readArray# #-}

  default writeArray# :: ( Generic a
                         , GTuple (Rep a)
                         ) => MutableArray# s Any -> Int# -> a -> State# s -> State# s
  writeArray# array i a = gwriteArray# array i (from a)
  {-# INLINE writeArray# #-}

sizeOf# :: Tuple a => a -> Int#
sizeOf# a = size# (proxy a)
{-# INLINE sizeOf# #-}

class GTuple a where
  gsize# :: t (a p) -> Int#
  greadArray# :: MutableArray# s Any -> Int# -> State# s -> (# State# s, a p #)
  gwriteArray# :: MutableArray# s Any -> Int# -> a p -> State# s -> State# s
