{-# LANGUAGE
    DeriveDataTypeable
  , MultiParamTypeClasses #-}
module Data.Ref.ST
       ( module Data.Ref.Class
       , STRef
       , STURef
       ) where

import Control.Monad.ST.Safe

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
a value of type @STURef s a@ is an unboxed mutable variable in state thread @s@,
containing a value of type @a@
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
