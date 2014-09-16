module Latest.Type
  ( Latest(..)
  ) where

import Data.Map.Strict (Map)

newtype Latest k v = Latest (Map k v)
