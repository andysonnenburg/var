module Data.STVar
       ( STVar
       ) where

import Data.STRef

type STVar = STRef
{- ^
a value of type @'STVar' s a@ is a mutable variable in state thread @s@,
containing a value of type @a@
-}
