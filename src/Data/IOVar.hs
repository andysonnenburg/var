module Data.IOVar
       ( IOVar
       ) where

import Data.IORef

-- |A mutable variable in the 'IO' monad
type IOVar = IORef
