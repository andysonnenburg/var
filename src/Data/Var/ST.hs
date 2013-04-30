{-# LANGUAGE
    CPP
  , DeriveDataTypeable
  , FlexibleContexts
  , FlexibleInstances
  , MultiParamTypeClasses #-}
{- |
Copyright   :  (c) Andy Sonnenburg 2013
License     :  BSD3
Maintainer  :  andy22286@gmail.com
-}
module Data.Var.ST
       ( module Data.Var.Class
       , STVar
       , STUVar
       , STAVar
       , newSTAVars
       ) where

#ifdef MODULE_Control_Monad_ST_Safe
import Control.Monad.ST.Safe
import qualified Control.Monad.ST.Lazy.Safe as Lazy
#else
import Control.Monad.ST
import qualified Control.Monad.ST.Lazy as Lazy
#endif

import Data.STVar
import Data.Var.Array
import Data.Var.ByteArray
import Data.Var.Class
import Data.Typeable

newtype STUVar s a =
  STUVar { unSTUVar :: ByteArrayVar s a
         } deriving (Eq, Typeable)
{- ^
a value of type @'STUVar' s a@ is a mutable variable in state thread @s@,
containing an unboxed value of type @a@
-}

instance ByteArrayElem a => Var (STUVar s) a (ST s) where
  newVar = fmap STUVar . newVar
  readVar = readVar . unSTUVar
  writeVar = writeVar . unSTUVar

instance ByteArrayElem a => Var (STUVar s) a (Lazy.ST s) where
  newVar = fmap STUVar . newVar
  readVar = readVar . unSTUVar
  writeVar = writeVar . unSTUVar

newtype STAVar s a = STAVar { unSTAVar :: ArrayVar s a }
{- ^
a value of type @'STAVar' s a@ is a mutable variable in state thread @s@,
containing a value of type @a@
-}

instance Var (STAVar s) a (ST s) where
  newVar = fmap STAVar . newVar
  readVar = readVar . unSTAVar
  writeVar = writeVar . unSTAVar

instance Var (STAVar s) a (Lazy.ST s) where
  newVar = fmap STAVar . newVar
  readVar = readVar . unSTAVar
  writeVar = writeVar . unSTAVar

newSTAVars :: Traversable (STAVar s) as refs => as -> ST s refs
newSTAVars = newVars STAVar
