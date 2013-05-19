{-# LANGUAGE CPP #-}
#ifdef FEATURE_MultiParamDefaultSignatures
{-# LANGUAGE DefaultSignatures #-}
#endif
{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses #-}
{- |
Copyright   :  (c) Andy Sonnenburg 2013
License     :  BSD3
Maintainer  :  andy22286@gmail.com
-}
module Data.Tuple.MTuple
       (
         -- * Tuples
         MTuple (..)
       , MField1 (..)
       , MField2 (..)
       , MField3 (..)
       , MField4 (..)
       , MField5 (..)
       , MField6 (..)
       , MField7 (..)
       , MField8 (..)
       , MField9 (..)
       ) where

#ifdef FEATURE_MultiParamDefaultSignatures
import Control.Monad.Trans.Class
#endif

class Monad m => MTuple var t m where
  thawTuple :: t -> m (var t)
  freezeTuple :: var t -> m t

#ifdef FEATURE_MultiParamDefaultSignatures
  default thawTuple :: (MonadTrans t', MTuple var t m) => t -> t' m (var t)
  thawTuple = lift . thawTuple

  default freezeTuple :: (MonadTrans t', MTuple var t m) => var t -> t' m t
  freezeTuple = lift . freezeTuple
#endif

class MTuple var t m => MField1 var t a m | t -> a where
  read1 :: var t -> m a
  write1 :: var t -> a -> m ()
  modify1 :: var t -> (a -> a) -> m ()
  modify1' :: var t -> (a -> a) -> m ()

#ifdef FEATURE_MultiParamDefaultSignatures
  default read1 :: (MonadTrans t', MField1 var t a m) => var t -> t' m a
  read1 = lift . read1

  default write1 :: (MonadTrans t', MField1 var t a m) => var t -> a -> t' m ()
  write1 var = lift . write1 var
#endif

  modify1 var f = write1 var . f =<< read1 var

  modify1' var f = read1 var >>= \ a -> write1 var $! f a

class MTuple var t m => MField2 var t a m | t -> a where
  read2 :: var t -> m a
  write2 :: var t -> a -> m ()
  modify2 :: var t -> (a -> a) -> m ()
  modify2' :: var t -> (a -> a) -> m ()

#ifdef FEATURE_MultiParamDefaultSignatures
  default read2 :: (MonadTrans t', MField2 var t a m) => var t -> t' m a
  read2 = lift . read2

  default write2 :: (MonadTrans t', MField2 var t a m) => var t -> a -> t' m ()
  write2 var = lift . write2 var
#endif

  modify2 var f = write2 var . f =<< read2 var

  modify2' var f = read2 var >>= \ a -> write2 var $! f a

class MTuple var t m => MField3 var t a m | t -> a where
  read3 :: var t -> m a
  write3 :: var t -> a -> m ()
  modify3 :: var t -> (a -> a) -> m ()
  modify3' :: var t -> (a -> a) -> m ()

#ifdef FEATURE_MultiParamDefaultSignatures
  default read3 :: (MonadTrans t', MField3 var t a m) => var t -> t' m a
  read3 = lift . read3

  default write3 :: (MonadTrans t', MField3 var t a m) => var t -> a -> t' m ()
  write3 var = lift . write3 var
#endif

  modify3 var f = write3 var . f =<< read3 var

  modify3' var f = read3 var >>= \ a -> write3 var $! f a

class MTuple var t m => MField4 var t a m | t -> a where
  read4 :: var t -> m a
  write4 :: var t -> a -> m ()
  modify4 :: var t -> (a -> a) -> m ()
  modify4' :: var t -> (a -> a) -> m ()

#ifdef FEATURE_MultiParamDefaultSignatures
  default read4 :: (MonadTrans t', MField4 var t a m) => var t -> t' m a
  read4 = lift . read4

  default write4 :: (MonadTrans t', MField4 var t a m) => var t -> a -> t' m ()
  write4 var = lift . write4 var
#endif

  modify4 var f = write4 var . f =<< read4 var

  modify4' var f = read4 var >>= \ a -> write4 var $! f a

class MTuple var t m => MField5 var t a m | t -> a where
  read5 :: var t -> m a
  write5 :: var t -> a -> m ()
  modify5 :: var t -> (a -> a) -> m ()
  modify5' :: var t -> (a -> a) -> m ()

#ifdef FEATURE_MultiParamDefaultSignatures
  default read5 :: (MonadTrans t', MField5 var t a m) => var t -> t' m a
  read5 = lift . read5

  default write5 :: (MonadTrans t', MField5 var t a m) => var t -> a -> t' m ()
  write5 var = lift . write5 var
#endif

  modify5 var f = write5 var . f =<< read5 var

  modify5' var f = read5 var >>= \ a -> write5 var $! f a

class MTuple var t m => MField6 var t a m | t -> a where
  read6 :: var t -> m a
  write6 :: var t -> a -> m ()
  modify6 :: var t -> (a -> a) -> m ()
  modify6' :: var t -> (a -> a) -> m ()

#ifdef FEATURE_MultiParamDefaultSignatures
  default read6 :: (MonadTrans t', MField6 var t a m) => var t -> t' m a
  read6 = lift . read6

  default write6 :: (MonadTrans t', MField6 var t a m) => var t -> a -> t' m ()
  write6 var = lift . write6 var
#endif

  modify6 var f = write6 var . f =<< read6 var

  modify6' var f = read6 var >>= \ a -> write6 var $! f a

class MTuple var t m => MField7 var t a m | t -> a where
  read7 :: var t -> m a
  write7 :: var t -> a -> m ()
  modify7 :: var t -> (a -> a) -> m ()
  modify7' :: var t -> (a -> a) -> m ()

#ifdef FEATURE_MultiParamDefaultSignatures
  default read7 :: (MonadTrans t', MField7 var t a m) => var t -> t' m a
  read7 = lift . read7

  default write7 :: (MonadTrans t', MField7 var t a m) => var t -> a -> t' m ()
  write7 var = lift . write7 var
#endif

  modify7 var f = write7 var . f =<< read7 var

  modify7' var f = read7 var >>= \ a -> write7 var $! f a

class MTuple var t m => MField8 var t a m | t -> a where
  read8 :: var t -> m a
  write8 :: var t -> a -> m ()
  modify8 :: var t -> (a -> a) -> m ()
  modify8' :: var t -> (a -> a) -> m ()

#ifdef FEATURE_MultiParamDefaultSignatures
  default read8 :: (MonadTrans t', MField8 var t a m) => var t -> t' m a
  read8 = lift . read8

  default write8 :: (MonadTrans t', MField8 var t a m) => var t -> a -> t' m ()
  write8 var = lift . write8 var
#endif

  modify8 var f = write8 var . f =<< read8 var

  modify8' var f = read8 var >>= \ a -> write8 var $! f a

class MTuple var t m => MField9 var t a m | t -> a where
  read9 :: var t -> m a
  write9 :: var t -> a -> m ()
  modify9 :: var t -> (a -> a) -> m ()
  modify9' :: var t -> (a -> a) -> m ()

#ifdef FEATURE_MultiParamDefaultSignatures
  default read9 :: (MonadTrans t', MField9 var t a m) => var t -> t' m a
  read9 = lift . read9

  default write9 :: (MonadTrans t', MField9 var t a m) => var t -> a -> t' m ()
  write9 var = lift . write9 var
#endif

  modify9 var f = write9 var . f =<< read9 var

  modify9' var f = read9 var >>= \ a -> write9 var $! f a
