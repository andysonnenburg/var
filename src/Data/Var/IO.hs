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
module Data.Var.IO
       ( module Data.Var.Class
       , IOVar
       , IOUVar
       , IOAVar
       , newIOAVars
       ) where

#ifdef MODULE_Control_Monad_ST_Safe
import Control.Monad.ST.Safe (RealWorld)
#else
import Control.Monad.ST (RealWorld)
#endif

import Data.IOVar
import Data.Var.Array
import Data.Var.ByteArray
import Data.Var.Class
import Data.Typeable

{- |
A mutable variable containing an unboxed value of type @a@ in the 'IO' monad
-}
newtype IOUVar a =
  IOUVar { unIOUVar :: ByteArrayVar RealWorld a
         } deriving (Eq, Typeable)

instance ByteArrayElem a => Var IOUVar a IO where
  newVar = fmap IOUVar . newVar
  readVar = readVar . unIOUVar
  writeVar = writeVar . unIOUVar

{- |
A mutable variable in the 'IO' monad
-}
newtype IOAVar a = IOAVar { unIOAVar :: ArrayVar RealWorld a }

instance Var IOAVar a IO where
  newVar = fmap IOAVar . newVar
  readVar = readVar . unIOAVar
  writeVar = writeVar . unIOAVar

newIOAVars :: ( Traversable Empty as refs
              , Traversable (Wrap IOAVar) as refs
              ) => as -> IO refs
newIOAVars = newVars IOAVar
