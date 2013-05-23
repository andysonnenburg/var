{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Main (main) where

import Control.Applicative
import Control.Monad.ST

import Data.ByteArraySlice
import Data.Tuple.IO
import Data.Tuple.ST
import Data.Tuple.Storable
import Data.Var.IO
import Data.Var.ST
import Data.Var.Storable

import Foreign.Storable

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck.Monadic

varLaws var a = do
  a' <- run $ do
    writeVar var a
    readVar var
  assert $ a == a'

prop_IOUVar (a :: Int, b) = monadicIO $ do
  var <- run $ newIOUVar a
  varLaws var b

newIOUVar :: ByteArraySlice a => a -> IO (IOUVar a)
newIOUVar = newVar

prop_STUVar (a :: Int, b) = monadicST $ do
  var <- run $ newSTUVar a
  varLaws var b

newSTUVar :: ByteArraySlice a => a -> ST s (STUVar s a)
newSTUVar = newVar

prop_StorableVar (a :: Int, b) = monadicIO $ do
  var <- run $ newStorableVar a
  varLaws var b

newStorableVar :: Storable a => a -> IO (StorableVar a)
newStorableVar = newVar

tupleLaws tuple (a, b, c, d, e) = do
  writeOnly1 tuple a
  a' <- run $ read1 tuple
  assert $ a == a'
  writeOnly2 tuple b
  b' <- run $ read2 tuple
  assert $ b == b'
  writeOnly3 tuple c
  c' <- run $ read3 tuple
  assert $ c == c'
  writeOnly4 tuple d
  d' <- run $ read4 tuple
  assert $ d == d'
  writeOnly5 tuple e
  e' <- run $ read5 tuple
  assert $ e == e'

writeOnly1 t a = do
  xs <- run $ readAllBut1 t
  run $ write1 t a
  xs' <- run $ readAllBut1 t
  assert $ xs == xs'

readAllBut1 t = (,,,) <$> read2 t <*> read3 t <*> read4 t <*> read5 t

writeOnly2 t a = do
  xs <- run $ readAllBut2 t
  run $ write2 t a
  xs' <- run $ readAllBut2 t
  assert $ xs == xs'

readAllBut2 t = (,,,) <$> read1 t <*> read3 t <*> read4 t <*> read5 t

writeOnly3 t a = do
  xs <- run $ readAllBut3 t
  run $ write3 t a
  xs' <- run $ readAllBut3 t
  assert $ xs == xs'

readAllBut3 t = (,,,) <$> read1 t <*> read2 t <*> read4 t <*> read5 t

writeOnly4 t a = do
  xs <- run $ readAllBut4 t
  run $ write4 t a
  xs' <- run $ readAllBut4 t
  assert $ xs == xs'

readAllBut4 t = (,,,) <$> read1 t <*> read2 t <*> read3 t <*> read5 t

writeOnly5 t a = do
  xs <- run $ readAllBut5 t
  run $ write5 t a
  xs' <- run $ readAllBut5 t
  assert $ xs == xs'

readAllBut5 t = (,,,) <$> read1 t <*> read2 t <*> read3 t <*> read4 t

prop_IOTuple (a :: Tuple5', b) = monadicIO $ do
  tuple <- run $ thawIOTuple5 a
  tupleLaws tuple b

thawIOTuple5 :: (a, b, c, d, e) -> IO (IOTuple (a, b, c, d, e))
thawIOTuple5 = thawTuple

prop_IOUTuple (a :: Tuple5', b) = monadicIO $ do
  tuple <- run $ thawIOUTuple5 a
  tupleLaws tuple b

thawIOUTuple5 :: ( ByteArraySlice a
                 , ByteArraySlice b
                 , ByteArraySlice c
                 , ByteArraySlice d
                 , ByteArraySlice e
                 ) => (a, b, c, d, e) -> IO (IOUTuple (a, b, c, d, e))
thawIOUTuple5 = thawTuple

prop_STTuple (a :: Tuple5', b) = monadicST $ do
  tuple <- run $ thawSTTuple5 a
  tupleLaws tuple b

thawSTTuple5 :: (a, b, c, d, e) -> ST s (STTuple s (a, b, c, d, e))
thawSTTuple5 = thawTuple

prop_STUTuple (a :: Tuple5', b) = monadicST $ do
  tuple <- run $ thawSTUTuple5 a
  tupleLaws tuple b

thawSTUTuple5 :: ( ByteArraySlice a
                 , ByteArraySlice b
                 , ByteArraySlice c
                 , ByteArraySlice d
                 , ByteArraySlice e
                 ) => (a, b, c, d, e) -> ST s (STUTuple s (a, b, c, d, e))
thawSTUTuple5 = thawTuple

prop_StorableTuple (a :: Tuple5'', b) = monadicIO $ do
  tuple <- run $ thawStorableTuple5 a
  tupleLaws tuple b

thawStorableTuple5 :: ( Storable a
                      , Storable b
                      , Storable c
                      , Storable d
                      , Storable e
                      ) => (a, b, c, d, e) -> IO (StorableTuple (a, b, c, d, e))
thawStorableTuple5 = thawTuple

type Tuple5' = (Bool, (Bool, Int), Int, Bool, Int)

type Tuple5'' = (Bool, Char, Int, Bool, Int)

main :: IO ()
main = defaultMain
       [ testProperty "IOUVar" prop_IOUVar
       , testProperty "STUVar" prop_STUVar
       , testProperty "StorableVar" prop_StorableVar
       , testProperty "IOTuple" prop_IOTuple
       , testProperty "IOUTuple" prop_IOUTuple
       , testProperty "STTuple" prop_STTuple
       , testProperty "STUTuple" prop_STUTuple
       , testProperty "StorableTuple" prop_StorableTuple
       ]
