module Graph where

import Data.Vector hiding (empty)
import Data.IntMap hiding (empty)
import Data.Map    hiding (empty)

import Data
import Empty

data GraphA = GraphA (Vector (Ceil3, IntMap Double)) (Map Ceil3 Int)

-- |int | Ceil3 | IntMap
-- |1   | (,,)  | (,) (,) (,) ...
-- |2   | (,,)  | (,) (,) (,) ...
-- ...

instance Empty GraphA where
    empty = GraphA empty empty
