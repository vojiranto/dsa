module Empty where

import Data.Map     as M
import Data.IntMap  as IM
import Data.Vector  as V


class Empty a where
    empty :: a


instance Ord a => Empty (Map a b) where
    empty = M.empty


instance Empty (Vector a) where
    empty = V.empty


instance Empty (IntMap a) where
    empty = IM.empty
