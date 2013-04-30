{- |
Copyright   :  (c) Andy Sonnenburg 2013
License     :  BSD3
Maintainer  :  andy22286@gmail.com
-}
module Data.IOVar
       ( IOVar
       ) where

import Data.IORef

-- |A mutable variable in the 'IO' monad
type IOVar = IORef
