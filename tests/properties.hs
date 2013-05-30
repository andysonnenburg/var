{-# LANGUAGE
    FlexibleContexts
  , GADTs
  , Rank2Types
  , ScopedTypeVariables
  , StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Main (main) where

import Control.Applicative
import Control.Monad.ST
import qualified Control.Monad.ST.Lazy as Lazy

import Data.ByteArraySlice
import Data.Int
import Data.Tuple.IO
import Data.Tuple.ST
import Data.Tuple.Storable
import Data.Typeable (Typeable, typeOf)
import Data.Var.IO
import Data.Var.ST
import Data.Var.Storable
import Data.Word

import Foreign.Storable

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Monadic

main :: IO ()
main = defaultMain
       [ testProperty "IOVar" prop_IOVar
       , testProperty "IOUVar" prop_IOUVar
       , testProperty "STVar" prop_STVar
       , testProperty "LazySTVar" prop_LazySTVar
       , testProperty "STUVar" prop_STUVar
       , testProperty "LazySTUVar" prop_LazySTUVar
       , testProperty "StorableVar" prop_StorableVar
       , testProperty "IOTuple" prop_IOTuple
       , testProperty "IOUTuple" prop_IOUTuple
       , testProperty "STTuple" prop_STTuple
       , testProperty "LazySTTuple" prop_LazySTTuple
       , testProperty "STUTuple" prop_STUTuple
       , testProperty "LazySTUTuple" prop_LazySTUTuple
       , testProperty "StorableTuple" prop_StorableTuple
       , testProperty "StorableTuple'" (prop_StorableTuple' ::
                                           Int16 -> Int16 ->
                                           Float -> Float ->
                                           Double -> Double ->
                                           Float -> Float ->
                                           Word16 -> Word16 ->
                                           Property)
       ]

varWriteRead var a = do
  a' <- run $ do
    writeVar var a
    readVar var
  assert $ a == a'

prop_IOVar (a :: Integer, b) = monadicIO $ do
  var <- run $ newIOVar a
  varWriteRead var b

newIOVar :: a -> IO (IOVar a)
newIOVar = newVar

prop_IOUVar (SomeByteArraySlice2 a a') = monadicIO $ do
  var <- run $ newIOUVar a
  varWriteRead var a'

newIOUVar :: ByteArraySlice a => a -> IO (IOUVar a)
newIOUVar = newVar

prop_STVar (a :: Integer, b) = monadicST $ do
  var <- run $ newSTVar a
  varWriteRead var b

newSTVar :: a -> ST s (STVar s a)
newSTVar = newVar

prop_LazySTVar (a :: Integer, b) = monadicLazyST $ do
  var <- run $ newLazySTVar a
  varWriteRead var b

newLazySTVar :: a -> Lazy.ST s (STVar s a)
newLazySTVar = newVar

prop_STUVar (SomeByteArraySlice2 a a') = monadicST $ do
  var <- run $ newSTUVar a
  varWriteRead var a'

newSTUVar :: ByteArraySlice a => a -> ST s (STUVar s a)
newSTUVar = newVar

prop_LazySTUVar (SomeByteArraySlice2 a a') = monadicLazyST $ do
  var <- run $ newLazySTUVar a
  varWriteRead var a'

newLazySTUVar :: ByteArraySlice a => a -> Lazy.ST s (STUVar s a)
newLazySTUVar = newVar

prop_StorableVar (SomeStorable2 a a') = monadicIO $ do
  var <- run $ newStorableVar a
  varWriteRead var a'

newStorableVar :: Storable a => a -> IO (StorableVar a)
newStorableVar = newVar

tupleWriteRead tuple (a, b, c, d, e) = do
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

tupleThawFreeze thaw xs = do
  tuple <- run $ thaw xs
  xs' <- run $ freezeTuple tuple
  assert $ xs == xs'

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

prop_IOTuple (a :: IntegerTuple, b) = monadicIO $ do
  tuple <- run $ thawIOTuple a
  tupleWriteRead tuple b
  tupleThawFreeze thawIOTuple a

thawIOTuple :: MTuple IOTuple a IO => a -> IO (IOTuple a)
thawIOTuple = thawTuple

prop_IOUTuple (SomeByteArraySlice2 a a',
               SomeByteArraySlice2 b b',
               SomeByteArraySlice2 c c',
               SomeByteArraySlice2 d d',
               SomeByteArraySlice2 e e') = monadicIO $ do
  let xs = (a, b, c, d, e)
  tuple <- run $ thawIOUTuple xs
  tupleWriteRead tuple (a', b', c', d', e')
  tupleThawFreeze thawIOUTuple xs

thawIOUTuple :: MTuple IOUTuple a IO => a -> IO (IOUTuple a)
thawIOUTuple = thawTuple

prop_STTuple (a :: IntegerTuple, b) = monadicST $ do
  tuple <- run $ thawSTTuple a
  tupleWriteRead tuple b
  tupleThawFreeze thawSTTuple a

thawSTTuple :: MTuple (STTuple s) a (ST s) => a -> ST s (STTuple s a)
thawSTTuple = thawTuple

prop_LazySTTuple (a :: IntegerTuple, b) = monadicLazyST $ do
  tuple <- run $ thawLazySTTuple a
  tupleWriteRead tuple b
  tupleThawFreeze thawLazySTTuple a

thawLazySTTuple :: MTuple (STTuple s) a (Lazy.ST s) => a -> Lazy.ST s (STTuple s a)
thawLazySTTuple = thawTuple

prop_STUTuple (SomeByteArraySlice2 a a',
               SomeByteArraySlice2 b b',
               SomeByteArraySlice2 c c',
               SomeByteArraySlice2 d d',
               SomeByteArraySlice2 e e')  = monadicST $ do
  let xs = (a, b, c, d, e)
  tuple <- run $ thawSTUTuple xs
  tupleWriteRead tuple (a', b', c', d', e')
  tupleThawFreeze thawSTUTuple xs

thawSTUTuple :: MTuple (STUTuple s) a (ST s) => a -> ST s (STUTuple s a)
thawSTUTuple = thawTuple

prop_LazySTUTuple (SomeByteArraySlice2 a a',
                   SomeByteArraySlice2 b b',
                   SomeByteArraySlice2 c c',
                   SomeByteArraySlice2 d d',
                   SomeByteArraySlice2 e e')  = monadicLazyST $ do
  let xs = (a, b, c, d, e)
  tuple <- run $ thawLazySTUTuple xs
  tupleWriteRead tuple (a', b', c', d', e')
  tupleThawFreeze thawLazySTUTuple xs

thawLazySTUTuple :: MTuple (STUTuple s) a (Lazy.ST s) => a -> Lazy.ST s (STUTuple s a)
thawLazySTUTuple = thawTuple

prop_StorableTuple (SomeStorable2 a a',
                    SomeStorable2 b b',
                    SomeStorable2 c c',
                    SomeStorable2 d d',
                    SomeStorable2 e e') = prop_StorableTuple' a a' b b' c c' d d' e e'  

prop_StorableTuple' a a' b b' c c' d d' e e' = monadicIO $ do
  let xs = (a, b, c, d, e)
  tuple <- run $ thawStorableTuple xs
  tupleWriteRead tuple (a', b', c', d', e')
  tupleThawFreeze thawStorableTuple xs

thawStorableTuple :: MTuple StorableTuple a IO => a -> IO (StorableTuple a)
thawStorableTuple = thawTuple

data SomeByteArraySlice2 where
  SomeByteArraySlice2 :: ( Show a
                         , Eq a
                         , Arbitrary a
                         , ByteArraySlice a
                         ) => a -> a -> SomeByteArraySlice2

instance Show SomeByteArraySlice2 where
  showsPrec p (SomeByteArraySlice2 a a') = showsPrec p (a, a')
  show (SomeByteArraySlice2 a a') = show (a, a')

instance Arbitrary SomeByteArraySlice2 where
  arbitrary = do
    (ByteArraySliceDict (Proxy :: Proxy a)) <- arbitrary
    (a, a') :: (a, a) <- arbitrary
    return $ SomeByteArraySlice2 a a'
  shrink (SomeByteArraySlice2 a a') =
    map (uncurry SomeByteArraySlice2) $ shrink (a, a')

data ByteArraySliceDict where
  ByteArraySliceDict :: ( Show a
                        , Eq a
                        , Arbitrary a
                        , ByteArraySlice a
                        ) => Proxy a -> ByteArraySliceDict

instance Arbitrary ByteArraySliceDict where
  arbitrary = do
    n <- size
    oneof $ if n <= 0 then leaf else leaf ++ branch
    where
      leaf =
        map pure
        [ ByteArraySliceDict (Proxy :: Proxy Bool)
        , ByteArraySliceDict (Proxy :: Proxy Char)
        , ByteArraySliceDict (Proxy :: Proxy Double)
        , ByteArraySliceDict (Proxy :: Proxy Float)
        , ByteArraySliceDict (Proxy :: Proxy Int)
        , ByteArraySliceDict (Proxy :: Proxy Int8)
        , ByteArraySliceDict (Proxy :: Proxy Int16)
        , ByteArraySliceDict (Proxy :: Proxy Int32)
        , ByteArraySliceDict (Proxy :: Proxy Int64)
        , ByteArraySliceDict (Proxy :: Proxy Word)
        , ByteArraySliceDict (Proxy :: Proxy Word8)
        , ByteArraySliceDict (Proxy :: Proxy Word16)
        , ByteArraySliceDict (Proxy :: Proxy Word32)
        , ByteArraySliceDict (Proxy :: Proxy Word64)
        ]
      branch =
        [ do
             let m = resize' (`div` 2) arbitrary
             ByteArraySliceDict (Proxy :: Proxy a) <- m
             ByteArraySliceDict (Proxy :: Proxy b) <- m
             return $ ByteArraySliceDict (Proxy :: Proxy (a, b))
        , do
             let m = resize' (`div` 3) arbitrary
             ByteArraySliceDict (Proxy :: Proxy a) <- m
             ByteArraySliceDict (Proxy :: Proxy b) <- m
             ByteArraySliceDict (Proxy :: Proxy c) <- m
             return $ ByteArraySliceDict (Proxy :: Proxy (a, b, c))
        , do
             let m = resize' (`div` 4) arbitrary
             ByteArraySliceDict (Proxy :: Proxy a) <- m
             ByteArraySliceDict (Proxy :: Proxy b) <- m
             ByteArraySliceDict (Proxy :: Proxy c) <- m
             ByteArraySliceDict (Proxy :: Proxy d) <- m
             return $ ByteArraySliceDict (Proxy :: Proxy (a, b, c, d))
        , do
             let m = resize' (`div` 5) arbitrary
             ByteArraySliceDict (Proxy :: Proxy a) <- m
             ByteArraySliceDict (Proxy :: Proxy b) <- m
             ByteArraySliceDict (Proxy :: Proxy c) <- m
             ByteArraySliceDict (Proxy :: Proxy d) <- m
             ByteArraySliceDict (Proxy :: Proxy e) <- m
             return $ ByteArraySliceDict (Proxy :: Proxy (a, b, c, d, e))
        ]

data SomeStorable2 where
  SomeStorable2 :: ( Show a
                   , Eq a
                   , Typeable a
                   , Arbitrary a
                   , Storable a
                   ) => a -> a -> SomeStorable2

instance Show SomeStorable2 where
  showsPrec p (SomeStorable2 a a') = showsPrec p ((a, typeOf a), (a', typeOf a'))
  show (SomeStorable2 a a') = show ((a, typeOf a), (a', typeOf a'))

instance Arbitrary SomeStorable2 where
  arbitrary = do
    StorableDict (Proxy :: Proxy a) <- arbitrary
    (a, a') :: (a, a) <- arbitrary
    return $ SomeStorable2 a a'
  shrink (SomeStorable2 a a') =
    map (uncurry SomeStorable2) $ shrink (a, a')

data StorableDict where
  StorableDict :: ( Show a
                  , Eq a
                  , Typeable a
                  , Arbitrary a
                  , Storable a
                  ) => Proxy a -> StorableDict

instance Arbitrary StorableDict where
  arbitrary =
    elements
    [ StorableDict (Proxy :: Proxy Bool)
    , StorableDict (Proxy :: Proxy Char)
    , StorableDict (Proxy :: Proxy Double)
    , StorableDict (Proxy :: Proxy Float)
    , StorableDict (Proxy :: Proxy Int)        
    , StorableDict (Proxy :: Proxy Int8)
    , StorableDict (Proxy :: Proxy Int16)
    , StorableDict (Proxy :: Proxy Int32)
    , StorableDict (Proxy :: Proxy Int64)
    , StorableDict (Proxy :: Proxy Word)
    , StorableDict (Proxy :: Proxy Word8)
    , StorableDict (Proxy :: Proxy Word16)
    , StorableDict (Proxy :: Proxy Word32)
    , StorableDict (Proxy :: Proxy Word64)
    ]

type IntegerTuple = (Integer, Integer, Integer, Integer, Integer)

monadicLazyST :: (forall s. PropertyM (Lazy.ST s) a) -> Property
monadicLazyST m = property (runLazySTGen (monadic' m))

runLazySTGen :: (forall s. Gen (Lazy.ST s a)) -> Gen a
runLazySTGen g = MkGen $ \r n -> Lazy.runST (unGen g r n)

size :: Gen Int
size = MkGen $ \ _ n -> n

resize' :: (Int -> Int) -> Gen a -> Gen a
resize' f m = sized $ \ n -> resize (f n) m

data Proxy a = Proxy
