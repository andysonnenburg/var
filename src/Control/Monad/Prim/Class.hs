{-# LANGUAGE CPP, MagicHash, TypeFamilies, UnboxedTuples #-}
{- |
Copyright   :  (c) Andy Sonnenburg 2013
License     :  BSD3
Maintainer  :  andy22286@gmail.com
-}
module Control.Monad.Prim.Class (MonadPrim (..)) where

#ifdef MODULE_Control_Monad_ST_Safe
import Control.Monad.ST.Lazy.Safe (strictToLazyST)
import qualified Control.Monad.ST.Lazy.Safe as Lazy
#else
import Control.Monad.ST.Lazy (strictToLazyST)
import qualified Control.Monad.ST.Lazy as Lazy
#endif

import GHC.Exts
import GHC.IO (IO (IO))
import GHC.ST (ST (ST))

class Monad m => MonadPrim m where
  type World m
  prim :: (State# (World m) -> (# State# (World m), a #)) -> m a

instance MonadPrim (ST s) where
  type World (ST s) = s
  prim = ST

instance MonadPrim (Lazy.ST s) where
  type World (Lazy.ST s) = s
  prim = strictToLazyST . prim

instance MonadPrim IO where
  type World IO = RealWorld
  prim = IO
