{-# LANGUAGE CPP #-}
#ifdef LANGUAGE_DefaultSignatures
{-# LANGUAGE DefaultSignatures #-}
#endif
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{- |
Copyright   :  (c) Andy Sonnenburg 2013
License     :  BSD3
Maintainer  :  andy22286@gmail.com
-}
module Data.Var.Class
       ( Var (..)
       ) where

#ifdef MODULE_Control_Monad_ST_Safe
import Control.Monad.ST.Safe
import qualified Control.Monad.ST.Lazy.Safe as Lazy
#else
import Control.Monad.ST
import qualified Control.Monad.ST.Lazy as Lazy
#endif
#ifdef LANGUAGE_DefaultSignatures
import Control.Monad.Trans.Class
#endif

import Data.IORef
import Data.IOVar
import Data.STRef
import qualified Data.STRef.Lazy as Lazy
import Data.STVar

class Monad m => Var var a m where
  newVar :: a -> m (var a)
  readVar :: var a -> m a
  writeVar :: var a -> a -> m ()
  modifyVar :: var a -> (a -> a) -> m ()
  modifyVar' :: var a -> (a -> a) -> m ()

#ifdef LANGUAGE_DefaultSignatures
  default newVar :: (MonadTrans t, Var var a m) => a -> t m (var a)
  newVar = lift . newVar

  default readVar :: (MonadTrans t, Var var a m) => var a -> t m a
  readVar = lift . readVar

  default writeVar :: (MonadTrans t, Var var a m) => var a -> a -> t m ()
  writeVar var = lift . writeVar var
#endif

  modifyVar var f = readVar var >>= writeVar var . f

  modifyVar' var f = readVar var >>= \ a -> writeVar var $! f a

instance Var IOVar a IO where
  newVar = newIORef
  readVar = readIORef
  writeVar = writeIORef
  modifyVar = modifyIORef
#ifdef FUNCTION_Data_IORef_modifyRef_
  modifyVar' = modifyIORef'
#endif

instance Var (STVar s) a (ST s) where
  newVar = newSTRef
  readVar = readSTRef
  writeVar = writeSTRef
  modifyVar = modifySTRef
#ifdef FUNCTION_Data_STRef_modifyRef_
  modifyVar' = modifySTRef'
#endif

instance Var (STVar s) a (Lazy.ST s) where
  newVar = Lazy.newSTRef
  readVar = Lazy.readSTRef
  writeVar = Lazy.writeSTRef
  modifyVar = Lazy.modifySTRef
