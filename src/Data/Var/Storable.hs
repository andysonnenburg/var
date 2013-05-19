{-# LANGUAGE
    DeriveDataTypeable
  , FlexibleInstances
  , MultiParamTypeClasses #-}
{- |
Copyright   :  (c) Andy Sonnenburg 2013
License     :  BSD3
Maintainer  :  andy22286@gmail.com
-}
module Data.Var.Storable
       ( module Data.Var.Class
       , StorableVar
       ) where

import Data.Data
import Data.Var.Class

import Foreign.ForeignPtr.Safe
import Foreign.Storable

newtype StorableVar a =
  StorableVar { unStorableVar :: ForeignPtr a
              } deriving (Show, Eq, Ord, Typeable)

instance Typeable a => Data (StorableVar a) where
  toConstr _ = error "Data.Data.toConstr(StorableVar)"
  gunfold _ _ = error "Data.Data.gunfold(StorableVar)"
  dataTypeOf _ = mkNoRepType "Data.Var.Storable.StorableVar"

instance Storable a => Var StorableVar a IO where
  newVar a = do
    ptr <- mallocForeignPtr
    withForeignPtr ptr $ flip poke a
    return $ StorableVar ptr
  readVar = flip withForeignPtr peek . unStorableVar
  writeVar (StorableVar ptr) = withForeignPtr ptr . flip poke
