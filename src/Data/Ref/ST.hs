{-# LANGUAGE
    CPP
  , DeriveDataTypeable
  , MultiParamTypeClasses #-}
{- |
Copyright   :  (c) Andy Sonnenburg 2013
License     :  BSD3
Maintainer  :  andy22286@gmail.com
-}
module Data.Ref.ST
       ( module Data.Ref.Class
       , STRef
       , STURef
       ) where

#ifdef MODULE_Control_Monad_ST_Safe
import Control.Monad.ST.Safe
import qualified Control.Monad.ST.Lazy.Safe as Lazy
#else
import Control.Monad.ST
import qualified Control.Monad.ST.Lazy as Lazy
#endif

import Data.Int
import Data.Ref.ByteArray
import Data.Ref.Class
import Data.STRef
import Data.Typeable
import Data.Word

import Foreign.Ptr
import Foreign.StablePtr

newtype STURef s a =
  STURef { unSTURef :: ByteArrayRef s a
         } deriving (Eq, Typeable)
{- ^
a value of type @'STURef' s a@ is a mutable variable in state thread @s@,
containing an unboxed value of type @a@
-}

instance Ref (STURef s) Bool (ST s) where
  newRef = fmap STURef . newRef
  readRef = readRef . unSTURef
  writeRef = writeRef . unSTURef

instance Ref (STURef s) Char (ST s) where
  newRef = fmap STURef . newRef
  readRef = readRef . unSTURef
  writeRef = writeRef . unSTURef

instance Ref (STURef s) Int (ST s) where
  newRef = fmap STURef . newRef
  readRef = readRef . unSTURef
  writeRef = writeRef . unSTURef

instance Ref (STURef s) Word (ST s) where
  newRef = fmap STURef . newRef
  readRef = readRef . unSTURef
  writeRef = writeRef . unSTURef

instance Ref (STURef s) (Ptr a) (ST s) where
  newRef = fmap STURef . newRef
  readRef = readRef . unSTURef
  writeRef = writeRef . unSTURef

instance Ref (STURef s) (FunPtr a) (ST s) where
  newRef = fmap STURef . newRef
  readRef = readRef . unSTURef
  writeRef = writeRef . unSTURef

instance Ref (STURef s) Float (ST s) where
  newRef = fmap STURef . newRef
  readRef = readRef . unSTURef
  writeRef = writeRef . unSTURef

instance Ref (STURef s) Double (ST s) where
  newRef = fmap STURef . newRef
  readRef = readRef . unSTURef
  writeRef = writeRef . unSTURef

instance Ref (STURef s) (StablePtr a) (ST s) where
  newRef = fmap STURef . newRef
  readRef = readRef . unSTURef
  writeRef = writeRef . unSTURef

instance Ref (STURef s) Int8 (ST s) where
  newRef = fmap STURef . newRef
  readRef = readRef . unSTURef
  writeRef = writeRef . unSTURef

instance Ref (STURef s) Int16 (ST s) where
  newRef = fmap STURef . newRef
  readRef = readRef . unSTURef
  writeRef = writeRef . unSTURef

instance Ref (STURef s) Int32 (ST s) where
  newRef = fmap STURef . newRef
  readRef = readRef . unSTURef
  writeRef = writeRef . unSTURef

instance Ref (STURef s) Int64 (ST s) where
  newRef = fmap STURef . newRef
  readRef = readRef . unSTURef
  writeRef = writeRef . unSTURef

instance Ref (STURef s) Word8 (ST s) where
  newRef = fmap STURef . newRef
  readRef = readRef . unSTURef
  writeRef = writeRef . unSTURef

instance Ref (STURef s) Word16 (ST s) where
  newRef = fmap STURef . newRef
  readRef = readRef . unSTURef
  writeRef = writeRef . unSTURef

instance Ref (STURef s) Word32 (ST s) where
  newRef = fmap STURef . newRef
  readRef = readRef . unSTURef
  writeRef = writeRef . unSTURef

instance Ref (STURef s) Word64 (ST s) where
  newRef = fmap STURef . newRef
  readRef = readRef . unSTURef
  writeRef = writeRef . unSTURef

instance Ref (STURef s) Bool (Lazy.ST s) where
  newRef = fmap STURef . newRef
  readRef = readRef . unSTURef
  writeRef = writeRef . unSTURef

instance Ref (STURef s) Char (Lazy.ST s) where
  newRef = fmap STURef . newRef
  readRef = readRef . unSTURef
  writeRef = writeRef . unSTURef

instance Ref (STURef s) Int (Lazy.ST s) where
  newRef = fmap STURef . newRef
  readRef = readRef . unSTURef
  writeRef = writeRef . unSTURef

instance Ref (STURef s) Word (Lazy.ST s) where
  newRef = fmap STURef . newRef
  readRef = readRef . unSTURef
  writeRef = writeRef . unSTURef

instance Ref (STURef s) (Ptr a) (Lazy.ST s) where
  newRef = fmap STURef . newRef
  readRef = readRef . unSTURef
  writeRef = writeRef . unSTURef

instance Ref (STURef s) (FunPtr a) (Lazy.ST s) where
  newRef = fmap STURef . newRef
  readRef = readRef . unSTURef
  writeRef = writeRef . unSTURef

instance Ref (STURef s) Float (Lazy.ST s) where
  newRef = fmap STURef . newRef
  readRef = readRef . unSTURef
  writeRef = writeRef . unSTURef

instance Ref (STURef s) Double (Lazy.ST s) where
  newRef = fmap STURef . newRef
  readRef = readRef . unSTURef
  writeRef = writeRef . unSTURef

instance Ref (STURef s) (StablePtr a) (Lazy.ST s) where
  newRef = fmap STURef . newRef
  readRef = readRef . unSTURef
  writeRef = writeRef . unSTURef

instance Ref (STURef s) Int8 (Lazy.ST s) where
  newRef = fmap STURef . newRef
  readRef = readRef . unSTURef
  writeRef = writeRef . unSTURef

instance Ref (STURef s) Int16 (Lazy.ST s) where
  newRef = fmap STURef . newRef
  readRef = readRef . unSTURef
  writeRef = writeRef . unSTURef

instance Ref (STURef s) Int32 (Lazy.ST s) where
  newRef = fmap STURef . newRef
  readRef = readRef . unSTURef
  writeRef = writeRef . unSTURef

instance Ref (STURef s) Int64 (Lazy.ST s) where
  newRef = fmap STURef . newRef
  readRef = readRef . unSTURef
  writeRef = writeRef . unSTURef

instance Ref (STURef s) Word8 (Lazy.ST s) where
  newRef = fmap STURef . newRef
  readRef = readRef . unSTURef
  writeRef = writeRef . unSTURef

instance Ref (STURef s) Word16 (Lazy.ST s) where
  newRef = fmap STURef . newRef
  readRef = readRef . unSTURef
  writeRef = writeRef . unSTURef

instance Ref (STURef s) Word32 (Lazy.ST s) where
  newRef = fmap STURef . newRef
  readRef = readRef . unSTURef
  writeRef = writeRef . unSTURef

instance Ref (STURef s) Word64 (Lazy.ST s) where
  newRef = fmap STURef . newRef
  readRef = readRef . unSTURef
  writeRef = writeRef . unSTURef
