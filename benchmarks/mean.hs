module Main (main) where

import Control.Applicative
import Control.Monad
import Control.Monad.ST
import Criterion
import Criterion.Main (defaultMain)

import Data.Tuple.ST
import Data.Var.ST

main :: IO ()
main = defaultMain
       [ bench "control mean" $ nf (controlMean . enumFromTo 1) 1000
       , bench "boxed tuple mean" $ nf (boxedTupleMean . enumFromTo 1) 1000
       , bench "unboxed tuple mean" $ nf (unboxedTupleMean . enumFromTo 1) 1000
       , bench "boxed var mean" $ nf (boxedVarMean . enumFromTo 1) 1000
       , bench "unboxed var mean" $ nf (unboxedVarMean . enumFromTo 1) 1000
       ]

controlMean :: [Double] -> Double
controlMean = go 0 0
  where
    go :: Double -> Int -> [Double] -> Double
    go s l [] = s / fromIntegral l
    go s l (x:xs) = go (s + x) (l + 1) xs

boxedTupleMean :: [Double] -> Double
boxedTupleMean xs = runST $ do
  var <- thawTuple (0, 0) :: ST s (STTuple s (Double, Int))
  forM_ xs $ \ x -> do
    modify1' var (+ x)
    modify2' var (+ 1)
  (s, l) <- freezeTuple var
  return $ s / fromIntegral l

unboxedTupleMean :: [Double] -> Double
unboxedTupleMean xs = runST $ do
  var <- thawTuple (0, 0) :: ST s (STUTuple s (Double, Int))
  forM_ xs $ \ x -> do
    modify1' var (+ x)
    modify2' var (+ 1)
  (s, l) <- freezeTuple var
  return $ s / fromIntegral l

boxedVarMean :: [Double] -> Double
boxedVarMean xs = runST $ do
  s <- newVar 0 :: ST s (STVar s Double)
  l <- newVar 0 :: ST s (STVar s Int)
  forM_ xs $ \ x -> do
    modifyVar' s (+ x)
    modifyVar' l (+ 1)
  (/) <$> readVar s <*> (fromIntegral <$> readVar l)

unboxedVarMean :: [Double] -> Double
unboxedVarMean xs = runST $ do
  s <- newVar 0 :: ST s (STUVar s Double)
  l <- newVar 0 :: ST s (STUVar s Int)
  forM_ xs $ \ x -> do
    modifyVar' s (+ x)
    modifyVar' l (+ 1)
  (/) <$> readVar s <*> (fromIntegral <$> readVar l)
