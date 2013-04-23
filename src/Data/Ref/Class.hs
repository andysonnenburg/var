{-# LANGUAGE CPP #-}
#ifdef LANGUAGE_DefaultSignatures
{-# LANGUAGE DefaultSignatures #-}
#endif
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Data.Ref.Class
       ( Ref (..)
       ) where

#ifdef MODULE_Control_Monad_ST_Safe
import Control.Monad.ST.Safe
import qualified Control.Monad.ST.Lazy.Safe as Lazy
#else
import Control.Monad.ST
import qualified Control.Monad.ST.Lazy as Lazy
#endif
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Error
import Control.Monad.Trans.Identity
import Control.Monad.Trans.List
import Control.Monad.Trans.Maybe
import qualified Control.Monad.Trans.RWS.Lazy as Lazy
import qualified Control.Monad.Trans.RWS.Strict as Strict
import Control.Monad.Trans.Reader
import qualified Control.Monad.Trans.State.Lazy as Lazy
import qualified Control.Monad.Trans.State.Strict as Strict
import qualified Control.Monad.Trans.Writer.Lazy as Lazy
import qualified Control.Monad.Trans.Writer.Strict as Strict

import Data.IORef
import Data.Monoid (Monoid)
import Data.STRef
import qualified Data.STRef.Lazy as Lazy

class Monad m => Ref ref a m where
  newRef :: a -> m (ref a)
  readRef :: ref a -> m a
  writeRef :: ref a -> a -> m ()
  modifyRef :: ref a -> (a -> a) -> m ()
  modifyRef' :: ref a -> (a -> a) -> m ()

#ifdef LANGUAGE_DefaultSignatures
  default newRef :: (MonadTrans t, Ref ref a m) => a -> t m (ref a)
  newRef = lift . newRef

  default readRef :: (MonadTrans t, Ref ref a m) => ref a -> t m a
  readRef = lift . readRef

  default writeRef :: (MonadTrans t, Ref ref a m) => ref a -> a -> t m ()
  writeRef ref = lift . writeRef ref
#endif

  modifyRef ref f = readRef ref >>= writeRef ref . f

  modifyRef' ref f = readRef ref >>= \ a -> writeRef ref $! f a

instance Ref IORef a IO where
  newRef = newIORef
  readRef = readIORef
  writeRef = writeIORef
  modifyRef = modifyIORef
#ifdef FUNCTION_strict_modifyRef
  modifyRef' = modifyIORef'
#endif

instance Ref (STRef s) a (ST s) where
  newRef = newSTRef
  readRef = readSTRef
  writeRef = writeSTRef
  modifyRef = modifySTRef
#ifdef FUNCTION_strict_modifyRef
  modifyRef' = modifySTRef'
#endif

instance Ref (Lazy.STRef s) a (Lazy.ST s) where
  newRef = Lazy.newSTRef
  readRef = Lazy.readSTRef
  writeRef = Lazy.writeSTRef
  modifyRef = Lazy.modifySTRef

instance Ref ref a m => Ref ref a (ContT r m) where
#ifndef LANGUAGE_DefaultSignatures
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef ref = lift . writeRef ref
#endif
  modifyRef ref = lift . modifyRef ref
  modifyRef' ref = lift . modifyRef' ref

instance (Ref ref a m, Error e) => Ref ref a (ErrorT e m) where
#ifndef LANGUAGE_DefaultSignatures
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef ref = lift . writeRef ref
#endif
  modifyRef ref = lift . modifyRef ref
  modifyRef' ref = lift . modifyRef' ref

instance Ref ref a m => Ref ref a (IdentityT m) where
#ifndef LANGUAGE_DefaultSignatures
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef ref = lift . writeRef ref
#endif
  modifyRef ref = lift . modifyRef ref
  modifyRef' ref = lift . modifyRef' ref

instance Ref ref a m => Ref ref a (ListT m) where
#ifndef LANGUAGE_DefaultSignatures
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef ref = lift . writeRef ref
#endif
  modifyRef ref = lift . modifyRef ref
  modifyRef' ref = lift . modifyRef' ref

instance Ref ref a m => Ref ref a (MaybeT m) where
#ifndef LANGUAGE_DefaultSignatures
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef ref = lift . writeRef ref
#endif
  modifyRef ref = lift . modifyRef ref
  modifyRef' ref = lift . modifyRef' ref

instance (Ref ref a m, Monoid w) => Ref ref a (Lazy.RWST r w s m) where
#ifndef LANGUAGE_DefaultSignatures
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef ref = lift . writeRef ref
#endif
  modifyRef ref = lift . modifyRef ref
  modifyRef' ref = lift . modifyRef' ref

instance (Ref ref a m, Monoid w) => Ref ref a (Strict.RWST r w s m) where
#ifndef LANGUAGE_DefaultSignatures
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef ref = lift . writeRef ref
#endif
  modifyRef ref = lift . modifyRef ref
  modifyRef' ref = lift . modifyRef' ref

instance Ref ref a m => Ref ref a (ReaderT r m) where
#ifndef LANGUAGE_DefaultSignatures
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef ref = lift . writeRef ref
#endif
  modifyRef ref = lift . modifyRef ref
  modifyRef' ref = lift . modifyRef' ref

instance Ref ref a m => Ref ref a (Lazy.StateT s m) where
#ifndef LANGUAGE_DefaultSignatures
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef ref = lift . writeRef ref
#endif
  modifyRef ref = lift . modifyRef ref
  modifyRef' ref = lift . modifyRef' ref

instance Ref ref a m => Ref ref a (Strict.StateT s m) where
#ifndef LANGUAGE_DefaultSignatures
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef ref = lift . writeRef ref
#endif
  modifyRef ref = lift . modifyRef ref
  modifyRef' ref = lift . modifyRef' ref

instance (Ref ref a m, Monoid w) => Ref ref a (Lazy.WriterT w m) where
#ifndef LANGUAGE_DefaultSignatures
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef ref = lift . writeRef ref
#endif
  modifyRef ref = lift . modifyRef ref
  modifyRef' ref = lift . modifyRef' ref

instance (Ref ref a m, Monoid w) => Ref ref a (Strict.WriterT w m) where
#ifndef LANGUAGE_DefaultSignatures
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef ref = lift . writeRef ref
#endif
  modifyRef ref = lift . modifyRef ref
  modifyRef' ref = lift . modifyRef' ref
