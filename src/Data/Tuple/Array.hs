{-# LANGUAGE
    CPP
  , DeriveDataTypeable
  , FlexibleInstances
  , MagicHash
  , MultiParamTypeClasses #-}
#ifdef LANGUAGE_Trustworthy
{-# LANGUAGE Trustworthy #-}
#endif
{-# LANGUAGE
    TypeFamilies
  , UnboxedTuples
  , UndecidableInstances #-}
{- |
Copyright   :  (c) Andy Sonnenburg 2013
License     :  BSD3
Maintainer  :  andy22286@gmail.com
-}
module Data.Tuple.Array
       ( ArrayTuple
       ) where

import Control.Monad.Prim.Class

import Data.Tuple.Class
import Data.Tuple.MTuple
import Data.Typeable (Typeable)

import GHC.Exts hiding (readArray#, writeArray#)
import qualified GHC.Exts

import Unsafe.Coerce (unsafeCoerce)

data ArrayTuple s a = ArrayTuple (MutableArray# s Any) deriving Typeable

instance Eq (ArrayTuple s a) where
  ArrayTuple a == ArrayTuple b = sameMutableArray# a b

instance (MonadPrim m, s ~ World m, Tuple t) => MTuple (ArrayTuple s) t m where
  thawTuple a = liftPrim $ \ s -> case newArray# (sizeOf# a) undefined s of
    (# s', array #) -> case writeArray# array 0# a s' of
      s'' -> (# s'', ArrayTuple array #)
  freezeTuple (ArrayTuple array) = liftPrim $ readArray# array 0#

instance ( MonadPrim m
         , s ~ World m
         , Tuple t
         , a ~ Field1 t
         ) => MField1 (ArrayTuple s) t a m where
  read1 = unsafeRead 0#
  write1 = unsafeWrite 0#

instance ( MonadPrim m
         , s ~ World m
         , Tuple t
         , a ~ Field2 t
         ) => MField2 (ArrayTuple s) t a m where
  read2 = unsafeRead 1#
  write2 = unsafeWrite 1#

instance ( MonadPrim m
         , s ~ World m
         , Tuple t
         , a ~ Field3 t
         ) => MField3 (ArrayTuple s) t a m where
  read3 = unsafeRead 2#
  write3 = unsafeWrite 2#

instance ( MonadPrim m
         , s ~ World m
         , Tuple t
         , a ~ Field4 t
         ) => MField4 (ArrayTuple s) t a m where
  read4 = unsafeRead 3#
  write4 = unsafeWrite 3#

instance ( MonadPrim m
         , s ~ World m
         , Tuple t
         , a ~ Field5 t
         ) => MField5 (ArrayTuple s) t a m where
  read5 = unsafeRead 4#
  write5 = unsafeWrite 4#

instance ( MonadPrim m
         , s ~ World m
         , Tuple t
         , a ~ Field6 t
         ) => MField6 (ArrayTuple s) t a m where
  read6 = unsafeRead 5#
  write6 = unsafeWrite 5#

instance ( MonadPrim m
         , s ~ World m
         , Tuple t
         , a ~ Field7 t
         ) => MField7 (ArrayTuple s) t a m where
  read7 = unsafeRead 6#
  write7 = unsafeWrite 6#

instance ( MonadPrim m
         , s ~ World m
         , Tuple t
         , a ~ Field8 t
         ) => MField8 (ArrayTuple s) t a m where
  read8 = unsafeRead 7#
  write8 = unsafeWrite 7#

instance ( MonadPrim m
         , s ~ World m
         , Tuple t
         , a ~ Field9 t
         ) => MField9 (ArrayTuple s) t a m where
  read9 = unsafeRead 8#
  write9 = unsafeWrite 8#

unsafeRead :: MonadPrim m => Int# -> ArrayTuple (World m) t -> m a
unsafeRead i (ArrayTuple array) = liftPrim $ \ s ->
  case GHC.Exts.readArray# array i s of
    (# s', a #) -> (# s', unsafeCoerce a #)

unsafeWrite :: MonadPrim m => Int# -> ArrayTuple (World m) t -> a -> m ()
unsafeWrite i (ArrayTuple array) a = liftPrim $ \ s ->
  case GHC.Exts.writeArray# array i (unsafeCoerce a) s of
    s' -> (# s', () #)
