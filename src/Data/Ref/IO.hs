{-# LANGUAGE
    CPP
  , DeriveDataTypeable
  , MultiParamTypeClasses #-}
module Data.Ref.IO
       ( module Data.Ref.Class
       , IORef
       , IOURef
       ) where

#ifdef MODULE_Control_Monad_ST_Safe
import Control.Monad.ST.Safe (RealWorld)
#else
import Control.Monad.ST (RealWorld)
#endif

import Data.Int
import Data.IORef
import Data.Ref.ByteArray
import Data.Ref.Class
import Data.Typeable
import Data.Word

import Foreign.Ptr
import Foreign.StablePtr

-- | An unboxed mutable variable in the 'IO' monad
newtype IOURef a =
  IOURef { unIOURef :: ByteArrayRef RealWorld a
         } deriving (Eq, Typeable)

instance Ref IOURef Bool IO where
  newRef = fmap IOURef . newRef
  readRef = readRef . unIOURef
  writeRef = writeRef . unIOURef

instance Ref IOURef Char IO where
  newRef = fmap IOURef . newRef
  readRef = readRef . unIOURef
  writeRef = writeRef . unIOURef

instance Ref IOURef Int IO where
  newRef = fmap IOURef . newRef
  readRef = readRef . unIOURef
  writeRef = writeRef . unIOURef

instance Ref IOURef Word IO where
  newRef = fmap IOURef . newRef
  readRef = readRef . unIOURef
  writeRef = writeRef . unIOURef

instance Ref IOURef (Ptr a) IO where
  newRef = fmap IOURef . newRef
  readRef = readRef . unIOURef
  writeRef = writeRef . unIOURef

instance Ref IOURef (FunPtr a) IO where
  newRef = fmap IOURef . newRef
  readRef = readRef . unIOURef
  writeRef = writeRef . unIOURef

instance Ref IOURef Float IO where
  newRef = fmap IOURef . newRef
  readRef = readRef . unIOURef
  writeRef = writeRef . unIOURef

instance Ref IOURef Double IO where
  newRef = fmap IOURef . newRef
  readRef = readRef . unIOURef
  writeRef = writeRef . unIOURef

instance Ref IOURef (StablePtr a) IO where
  newRef = fmap IOURef . newRef
  readRef = readRef . unIOURef
  writeRef = writeRef . unIOURef

instance Ref IOURef Int8 IO where
  newRef = fmap IOURef . newRef
  readRef = readRef . unIOURef
  writeRef = writeRef . unIOURef

instance Ref IOURef Int16 IO where
  newRef = fmap IOURef . newRef
  readRef = readRef . unIOURef
  writeRef = writeRef . unIOURef

instance Ref IOURef Int32 IO where
  newRef = fmap IOURef . newRef
  readRef = readRef . unIOURef
  writeRef = writeRef . unIOURef

instance Ref IOURef Int64 IO where
  newRef = fmap IOURef . newRef
  readRef = readRef . unIOURef
  writeRef = writeRef . unIOURef

instance Ref IOURef Word8 IO where
  newRef = fmap IOURef . newRef
  readRef = readRef . unIOURef
  writeRef = writeRef . unIOURef

instance Ref IOURef Word16 IO where
  newRef = fmap IOURef . newRef
  readRef = readRef . unIOURef
  writeRef = writeRef . unIOURef

instance Ref IOURef Word32 IO where
  newRef = fmap IOURef . newRef
  readRef = readRef . unIOURef
  writeRef = writeRef . unIOURef

instance Ref IOURef Word64 IO where
  newRef = fmap IOURef . newRef
  readRef = readRef . unIOURef
  writeRef = writeRef . unIOURef
