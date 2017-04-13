{-#Language TypeFamilies#-}
module Empty where

import Data.Map     as M
import Data.IntMap  as IM
import Data.Vector  as V
import Data.List    as L


class Empty a where
    type Elem a

    empty :: a
    elems :: a -> [Elem a]
    null  :: a -> Bool

instance Ord a => Empty (Map a b) where
    type Elem (Map a b) = b

    empty = M.empty
    elems = M.elems
    null  = M.null

instance Empty (Vector a) where
    type Elem (Vector a) = a

    empty = V.empty
    elems = V.toList
    null  = V.null

instance Empty (IntMap a) where
    type Elem (IntMap a) = a

    empty = IM.empty
    elems = IM.elems
    null  = IM.null

instance Empty [a] where
    type Elem [a] = a

    empty = []
    elems = id
    null  = L.null
