{-# LANGUAGE CPP, DeriveGeneric, TypeFamilies, TypeOperators #-}
module Main (main) where

import Control.Applicative
import Control.DeepSeq
import Control.Monad
import Control.Monad.ST
import Criterion
import Criterion.Main (defaultMain)

import Data.ByteArraySlice.Unsafe
import Data.Tuple.Fields.Unsafe
import Data.Tuple.ST
import Data.Word

import GHC.Generics

main :: IO ()
main = defaultMain
       [ bench "uninlined" $ nf uninlined 10000
       , bench "inlined" $ nf inlined 10000
       ]

data Uninlined
  = Uninlined
    {-# UNPACK #-} !Char
    {-# UNPACK #-} !Int
    {-# UNPACK #-} !Word
    {-# UNPACK #-} !Float
    {-# UNPACK #-} !Double deriving Generic

instance ByteArraySlice Uninlined
instance Fields Uninlined
#ifndef FEATURE_TypeFamilyDefaults
  where type ListRep Uninlined = Char :| Int :| Word :| Float :| Double :| Nil
#endif
instance NFData Uninlined

uninlined :: Int -> [Uninlined]
uninlined n = runST $ do
  vs <- replicateM n $
        newSTUTuple $ Uninlined 'a' n (fromIntegral n) (fromIntegral n) (fromIntegral n)
  let n' = n + 1
  forM_ vs $ \ v -> do
    write1 v 'b'
    write2 v n'
    write3 v (fromIntegral n')
    write4 v (fromIntegral n')
    write5 v (fromIntegral n')
  forM vs $ \ v ->
    Uninlined
    <$> read1 v
    <*> read2 v
    <*> read3 v
    <*> read4 v
    <*> read5 v

data Inlined
  = Inlined
    {-# UNPACK #-} !Char
    {-# UNPACK #-} !Int
    {-# UNPACK #-} !Word
    {-# UNPACK #-} !Float
    {-# UNPACK #-} !Double deriving Generic

instance ByteArraySlice Inlined where
  {-# INLINE plusByteSize #-}
  {-# INLINE readByteOff #-}
  {-# INLINE writeByteOff #-}
#ifndef FEATURE_InlineDefaultMethods
  plusByteSize = plusByteSizeDefault
  readByteOff = readByteOffDefault
  writeByteOff = writeByteOffDefault
#endif
instance Fields Inlined
#ifndef FEATURE_TypeFamilyDefaults
  where type ListRep Inlined = Char :| Int :| Word :| Float :| Double :| Nil
#endif
instance NFData Inlined

inlined :: Int -> [Inlined]
inlined n = runST $ do
  vs <- replicateM n $
        newSTUTuple $ Inlined 'a' n (fromIntegral n) (fromIntegral n) (fromIntegral n)
  let n' = n + 1
  forM_ vs $ \ v -> do
    write1 v 'b'
    write2 v n'
    write3 v (fromIntegral n')
    write4 v (fromIntegral n')
    write5 v (fromIntegral n')
  forM vs $ \ v ->
    Inlined
    <$> read1 v
    <*> read2 v
    <*> read3 v
    <*> read4 v
    <*> read5 v

newSTUTuple :: (ByteArraySlice a, Fields a) => a -> ST s (STUTuple s a)
newSTUTuple = thawTuple
