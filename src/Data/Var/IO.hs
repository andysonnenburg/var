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
       , IORef
       , IOURef
       , IOARef
       , newIOARefs
       , FixedSize
       , Heterogeneous
       ) where

#ifdef MODULE_Control_Monad_ST_Safe
import Control.Monad.ST.Safe (RealWorld)
#else
import Control.Monad.ST (RealWorld)
#endif

import Data.Int
import Data.IORef
import Data.Var.Array
import Data.Var.ByteArray
import Data.Var.Class
import Data.Var.Tuple
import Data.Typeable
import Data.Word

import Foreign.Ptr
import Foreign.StablePtr

{- |
A mutable variable containing an unboxed value of type @a@ in the 'IO' monad
-}
newtype IOURef a =
  IOURef { unIOURef :: ByteArrayVar RealWorld a
         } deriving (Eq, Typeable)

instance Var IOURef Bool IO where
  newVar = fmap IOURef . newVar
  readVar = readVar . unIOURef
  writeVar = writeVar . unIOURef

instance Var IOURef Char IO where
  newVar = fmap IOURef . newVar
  readVar = readVar . unIOURef
  writeVar = writeVar . unIOURef

instance Var IOURef Int IO where
  newVar = fmap IOURef . newVar
  readVar = readVar . unIOURef
  writeVar = writeVar . unIOURef

instance Var IOURef Word IO where
  newVar = fmap IOURef . newVar
  readVar = readVar . unIOURef
  writeVar = writeVar . unIOURef

instance Var IOURef (Ptr a) IO where
  newVar = fmap IOURef . newVar
  readVar = readVar . unIOURef
  writeVar = writeVar . unIOURef

instance Var IOURef (FunPtr a) IO where
  newVar = fmap IOURef . newVar
  readVar = readVar . unIOURef
  writeVar = writeVar . unIOURef

instance Var IOURef Float IO where
  newVar = fmap IOURef . newVar
  readVar = readVar . unIOURef
  writeVar = writeVar . unIOURef

instance Var IOURef Double IO where
  newVar = fmap IOURef . newVar
  readVar = readVar . unIOURef
  writeVar = writeVar . unIOURef

instance Var IOURef (StablePtr a) IO where
  newVar = fmap IOURef . newVar
  readVar = readVar . unIOURef
  writeVar = writeVar . unIOURef

instance Var IOURef Int8 IO where
  newVar = fmap IOURef . newVar
  readVar = readVar . unIOURef
  writeVar = writeVar . unIOURef

instance Var IOURef Int16 IO where
  newVar = fmap IOURef . newVar
  readVar = readVar . unIOURef
  writeVar = writeVar . unIOURef

instance Var IOURef Int32 IO where
  newVar = fmap IOURef . newVar
  readVar = readVar . unIOURef
  writeVar = writeVar . unIOURef

instance Var IOURef Int64 IO where
  newVar = fmap IOURef . newVar
  readVar = readVar . unIOURef
  writeVar = writeVar . unIOURef

instance Var IOURef Word8 IO where
  newVar = fmap IOURef . newVar
  readVar = readVar . unIOURef
  writeVar = writeVar . unIOURef

instance Var IOURef Word16 IO where
  newVar = fmap IOURef . newVar
  readVar = readVar . unIOURef
  writeVar = writeVar . unIOURef

instance Var IOURef Word32 IO where
  newVar = fmap IOURef . newVar
  readVar = readVar . unIOURef
  writeVar = writeVar . unIOURef

instance Var IOURef Word64 IO where
  newVar = fmap IOURef . newVar
  readVar = readVar . unIOURef
  writeVar = writeVar . unIOURef

newtype IOARef a = IOARef { unIOARef :: ArrayVar RealWorld a }

instance Var IOARef a IO where
  newVar = fmap IOARef . newVar
  readVar = readVar . unIOARef
  writeVar = writeVar . unIOARef

newIOARefs :: (FixedSize as, Heterogeneous IOARef as refs) => as -> IO refs
newIOARefs = newVars IOARef
