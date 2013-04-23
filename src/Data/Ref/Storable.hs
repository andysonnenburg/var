{-# LANGUAGE
    DeriveDataTypeable
  , FlexibleInstances
  , MultiParamTypeClasses #-}
module Data.Ref.Storable
       ( module Data.Ref.Class
       , StorableRef
       ) where

import Data.Ref.Class
import Data.Data

import Foreign.ForeignPtr.Safe
import Foreign.Storable

newtype StorableRef a =
  StorableRef { unStorableRef :: ForeignPtr a
              } deriving (Show, Eq, Ord, Typeable)

instance Typeable a => Data (StorableRef a) where
  toConstr _ = error "Data.Data.toConstr(StorableRef)"
  gunfold _ _ = error "Data.Data.gunfold(StorableRef)"
  dataTypeOf _ = mkNoRepType "Data.Ref.Storable.StorableRef"

instance Storable a => Ref StorableRef a IO where
  newRef a = do
    ptr <- mallocForeignPtr
    withForeignPtr ptr $ flip poke a
    return (StorableRef ptr)
  readRef = flip withForeignPtr peek . unStorableRef
  writeRef (StorableRef ptr) = withForeignPtr ptr . flip poke
