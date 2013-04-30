{-# LANGUAGE
    CPP
  , DefaultSignatures
  , FlexibleContexts
  , FlexibleInstances
  , FunctionalDependencies
  , MagicHash
  , MultiParamTypeClasses
  , Rank2Types
  , TypeOperators
  , UndecidableInstances #-}
#ifdef LANGUAGE_Trustworthy
{-# LANGUAGE Trustworthy #-}
#endif
{-# LANGUAGE TypeFamilies, UnboxedTuples #-}
{- |
Copyright   :  (c) Andy Sonnenburg 2013
License     :  BSD3
Maintainer  :  andy22286@gmail.com
-}
module Data.Var.Array
       ( ArrayVar
       , newVars
       , Traversable
       ) where

import Control.Monad (liftM, liftM2)
import qualified Control.Monad as Monad
import Control.Monad.Prim.Class

import Data.Var.ByteArray
import Data.Var.Class

import GHC.Exts
import GHC.Generics

import Prelude hiding (mapM)

import Unsafe.Coerce (unsafeCoerce)

data ArrayVar s a = ArrayVar {-# UNPACK #-} !(MutableArray s Any) {-# UNPACK #-} !Int

instance (MonadPrim m, s ~ World m) => Var (ArrayVar s) a m where
  newVar a = do
    array <- newArray 1# undefined
    writeArray array 0 (unsafeCoerce a)
    return $ ArrayVar array 0
  {-# INLINE newVar #-}
  readVar (ArrayVar array i) = liftM unsafeCoerce $ readArray array i
  {-# INLINE readVar #-}
  writeVar (ArrayVar array i) = writeArray array i . unsafeCoerce
  {-# INLINE writeVar #-}

newVars :: ( Traversable w as vars
           , MonadPrim m
           , Var (ByteArrayVar (World m)) Int m
           ) => (forall a . ArrayVar (World m) a -> w a) -> as -> m vars
newVars f as = do
  array <- newArray (sizeOf# as) undefined
  i <- newByteArrayVar 0
  forM as $ \ a -> do
    i' <- readVar i
    writeArray array i' (unsafeCoerce a)
    modifyVar' i (+ 1)
    return $! f (ArrayVar array i')
{-# INLINE newVars #-}

data MutableArray s a = MutableArray (MutableArray# s a)

newArray :: MonadPrim m => Int# -> a -> m (MutableArray (World m) a)
newArray n a = liftPrim $ \ s -> case newArray# n a s of
  (# s', array #) -> (# s', MutableArray array #)
{-# INLINE newArray #-}

readArray :: MonadPrim m => MutableArray (World m) a -> Int -> m a
readArray (MutableArray array) (I# i) = liftPrim $ readArray# array i
{-# INLINE readArray #-}

writeArray :: MonadPrim m => MutableArray (World m) a -> Int -> a -> m ()
writeArray (MutableArray array) (I# i) a = liftPrim $ \ s ->
  case writeArray# array i a s of
    s' -> (# s', () #)
{-# INLINE writeArray #-}

class Traversable w a b | a -> w b, b -> w a where
  sizeOf# :: a -> Int#
  mapM :: Monad m => (forall c . c -> m (w c)) -> a -> m b

  default sizeOf# :: (Generic a, GTraversable w (Rep a) (Rep b)) => a -> Int#
  sizeOf# a = gsizeOf# (from a)
  {-# INLINE sizeOf# #-}

  default mapM :: ( Generic a
                  , Generic b
                  , GTraversable w (Rep a) (Rep b)
                  , Monad m
                  ) => (forall c . c -> m (w c)) -> a -> m b
  mapM f = liftM to . gmapM f . from
  {-# INLINE mapM #-}

forM :: (Traversable w a b, Monad m) => a -> (forall c . c -> m (w c)) -> m b
forM = flip mapM

class GTraversable w a b | a -> w b, b -> w a where
  gsizeOf# :: a p -> Int#
  gmapM :: Monad m => (forall c . c -> m (w c)) -> a p -> m (b p)

instance GTraversable w U1 U1 where
  gsizeOf# _ = 0#
  gmapM _ U1 = return U1

instance GTraversable w (K1 i c) (K1 i (w c)) where
  gsizeOf# _ = 1#
  gmapM f = liftM K1 . f . unK1

instance GTraversable w a b => GTraversable w (M1 i c a) (M1 i c b) where
  gsizeOf# a = gsizeOf# (unM1 a)
  gmapM f = liftM M1 . gmapM f . unM1

instance ( GTraversable w a a'
         , GTraversable w b b'
         ) => GTraversable w (a :+: b) (a' :+: b') where
  gsizeOf# (L1 a) = gsizeOf# a
  gsizeOf# (R1 a) = gsizeOf# a
  gmapM f (L1 a) = liftM L1 (gmapM f a)
  gmapM f (R1 a) = liftM R1 (gmapM f a)

instance ( GTraversable w a a'
         , GTraversable w b b'
         ) => GTraversable w (a :*: b) (a' :*: b') where
  gsizeOf# (a :*: b) = gsizeOf# a +# gsizeOf# b
  gmapM f (a :*: b) = liftM2 (:*:) (gmapM f a) (gmapM f b)

instance Traversable w [a] [w a] where
  sizeOf# a = case length a of I# i -> i
  mapM f = Monad.mapM f

instance Traversable w (Maybe a) (Maybe (w a))
instance Traversable w (Either a b) (Either (w a) (w b))
instance Traversable w () ()
instance Traversable w (a, b) (w a, w b)
instance Traversable w (a, b, c) (w a, w b, w c)
instance Traversable w (a, b, c, d) (w a, w b, w c, w d)
instance Traversable w (a, b, c, d, e) (w a, w b, w c, w d, w e)
instance Traversable w (a, b, c, d, e, f) (w a, w b, w c, w d, w e, w f)
instance Traversable w (a, b, c, d, e, f, g) (w a, w b, w c, w d, w e, w f, w g)
