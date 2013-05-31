{-# LANGUAGE CPP #-}
#ifdef LANGUAGE_Trustworthy
{-# LANGUAGE Trustworthy #-}
#endif
{- |
Copyright   :  (c) Andy Sonnenburg 2013
License     :  BSD3
Maintainer  :  andy22286@gmail.com
-}
module Data.TVar
       ( STM
       , TVar
       , newTVar
       , readTVar
       , writeTVar
       ) where

#ifdef MODULE_GHC_Conc_Sync
import GHC.Conc.Sync
#else
import GHC.Conc
#endif
