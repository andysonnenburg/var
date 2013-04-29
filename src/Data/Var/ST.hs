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
       , STRef
       , STURef
       , STARef
       , newSTARefs
       , FixedSize
       , Heterogeneous
       ) where

#ifdef MODULE_Control_Monad_ST_Safe
import Control.Monad.ST.Safe
import qualified Control.Monad.ST.Lazy.Safe as Lazy
#else
import Control.Monad.ST
import qualified Control.Monad.ST.Lazy as Lazy
#endif

import Data.Int
import Data.Var.Array
import Data.Var.ByteArray
import Data.Var.Class
import Data.Var.Tuple
import Data.STRef
import Data.Typeable
import Data.Word

import Foreign.Ptr
import Foreign.StablePtr

newtype STURef s a =
  STURef { unSTURef :: ByteArrayVar s a
         } deriving (Eq, Typeable)
{- ^
a value of type @'STURef' s a@ is a mutable variable in state thread @s@,
containing an unboxed value of type @a@
-}

instance Var (STURef s) Bool (ST s) where
  newVar = fmap STURef . newVar
  readVar = readVar . unSTURef
  writeVar = writeVar . unSTURef

instance Var (STURef s) Char (ST s) where
  newVar = fmap STURef . newVar
  readVar = readVar . unSTURef
  writeVar = writeVar . unSTURef

instance Var (STURef s) Int (ST s) where
  newVar = fmap STURef . newVar
  readVar = readVar . unSTURef
  writeVar = writeVar . unSTURef

instance Var (STURef s) Word (ST s) where
  newVar = fmap STURef . newVar
  readVar = readVar . unSTURef
  writeVar = writeVar . unSTURef

instance Var (STURef s) (Ptr a) (ST s) where
  newVar = fmap STURef . newVar
  readVar = readVar . unSTURef
  writeVar = writeVar . unSTURef

instance Var (STURef s) (FunPtr a) (ST s) where
  newVar = fmap STURef . newVar
  readVar = readVar . unSTURef
  writeVar = writeVar . unSTURef

instance Var (STURef s) Float (ST s) where
  newVar = fmap STURef . newVar
  readVar = readVar . unSTURef
  writeVar = writeVar . unSTURef

instance Var (STURef s) Double (ST s) where
  newVar = fmap STURef . newVar
  readVar = readVar . unSTURef
  writeVar = writeVar . unSTURef

instance Var (STURef s) (StablePtr a) (ST s) where
  newVar = fmap STURef . newVar
  readVar = readVar . unSTURef
  writeVar = writeVar . unSTURef

instance Var (STURef s) Int8 (ST s) where
  newVar = fmap STURef . newVar
  readVar = readVar . unSTURef
  writeVar = writeVar . unSTURef

instance Var (STURef s) Int16 (ST s) where
  newVar = fmap STURef . newVar
  readVar = readVar . unSTURef
  writeVar = writeVar . unSTURef

instance Var (STURef s) Int32 (ST s) where
  newVar = fmap STURef . newVar
  readVar = readVar . unSTURef
  writeVar = writeVar . unSTURef

instance Var (STURef s) Int64 (ST s) where
  newVar = fmap STURef . newVar
  readVar = readVar . unSTURef
  writeVar = writeVar . unSTURef

instance Var (STURef s) Word8 (ST s) where
  newVar = fmap STURef . newVar
  readVar = readVar . unSTURef
  writeVar = writeVar . unSTURef

instance Var (STURef s) Word16 (ST s) where
  newVar = fmap STURef . newVar
  readVar = readVar . unSTURef
  writeVar = writeVar . unSTURef

instance Var (STURef s) Word32 (ST s) where
  newVar = fmap STURef . newVar
  readVar = readVar . unSTURef
  writeVar = writeVar . unSTURef

instance Var (STURef s) Word64 (ST s) where
  newVar = fmap STURef . newVar
  readVar = readVar . unSTURef
  writeVar = writeVar . unSTURef

instance Var (STURef s) Bool (Lazy.ST s) where
  newVar = fmap STURef . newVar
  readVar = readVar . unSTURef
  writeVar = writeVar . unSTURef

instance Var (STURef s) Char (Lazy.ST s) where
  newVar = fmap STURef . newVar
  readVar = readVar . unSTURef
  writeVar = writeVar . unSTURef

instance Var (STURef s) Int (Lazy.ST s) where
  newVar = fmap STURef . newVar
  readVar = readVar . unSTURef
  writeVar = writeVar . unSTURef

instance Var (STURef s) Word (Lazy.ST s) where
  newVar = fmap STURef . newVar
  readVar = readVar . unSTURef
  writeVar = writeVar . unSTURef

instance Var (STURef s) (Ptr a) (Lazy.ST s) where
  newVar = fmap STURef . newVar
  readVar = readVar . unSTURef
  writeVar = writeVar . unSTURef

instance Var (STURef s) (FunPtr a) (Lazy.ST s) where
  newVar = fmap STURef . newVar
  readVar = readVar . unSTURef
  writeVar = writeVar . unSTURef

instance Var (STURef s) Float (Lazy.ST s) where
  newVar = fmap STURef . newVar
  readVar = readVar . unSTURef
  writeVar = writeVar . unSTURef

instance Var (STURef s) Double (Lazy.ST s) where
  newVar = fmap STURef . newVar
  readVar = readVar . unSTURef
  writeVar = writeVar . unSTURef

instance Var (STURef s) (StablePtr a) (Lazy.ST s) where
  newVar = fmap STURef . newVar
  readVar = readVar . unSTURef
  writeVar = writeVar . unSTURef

instance Var (STURef s) Int8 (Lazy.ST s) where
  newVar = fmap STURef . newVar
  readVar = readVar . unSTURef
  writeVar = writeVar . unSTURef

instance Var (STURef s) Int16 (Lazy.ST s) where
  newVar = fmap STURef . newVar
  readVar = readVar . unSTURef
  writeVar = writeVar . unSTURef

instance Var (STURef s) Int32 (Lazy.ST s) where
  newVar = fmap STURef . newVar
  readVar = readVar . unSTURef
  writeVar = writeVar . unSTURef

instance Var (STURef s) Int64 (Lazy.ST s) where
  newVar = fmap STURef . newVar
  readVar = readVar . unSTURef
  writeVar = writeVar . unSTURef

instance Var (STURef s) Word8 (Lazy.ST s) where
  newVar = fmap STURef . newVar
  readVar = readVar . unSTURef
  writeVar = writeVar . unSTURef

instance Var (STURef s) Word16 (Lazy.ST s) where
  newVar = fmap STURef . newVar
  readVar = readVar . unSTURef
  writeVar = writeVar . unSTURef

instance Var (STURef s) Word32 (Lazy.ST s) where
  newVar = fmap STURef . newVar
  readVar = readVar . unSTURef
  writeVar = writeVar . unSTURef

instance Var (STURef s) Word64 (Lazy.ST s) where
  newVar = fmap STURef . newVar
  readVar = readVar . unSTURef
  writeVar = writeVar . unSTURef

newtype STARef s a = STARef { unSTARef :: ArrayVar s a }

instance Var (STARef s) a (ST s) where
  newVar = fmap STARef . newVar
  readVar = readVar . unSTARef
  writeVar = writeVar . unSTARef

instance Var (STARef s) a (Lazy.ST s) where
  newVar = fmap STARef . newVar
  readVar = readVar . unSTARef
  writeVar = writeVar . unSTARef

newSTARefs :: (FixedSize as, Heterogeneous (STARef s) as refs) => as -> ST s refs
newSTARefs = newVars STARef
