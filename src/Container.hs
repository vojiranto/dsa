{-#Language TypeFamilies#-}
module Container where

import Data.Map     as M
import Data.IntMap  as IM
import Data.Vector  as V
import Data.List    as L


class Container a where
    type Elem a

    empty :: a
    elems :: a -> [Elem a]
    null  :: a -> Bool

instance Ord a => Container (Map a b) where
    type Elem (Map a b) = b

    empty = M.empty
    elems = M.elems
    null  = M.null

instance Container (Vector a) where
    type Elem (Vector a) = a

    empty = V.empty
    elems = V.toList
    null  = V.null

instance Container (IntMap a) where
    type Elem (IntMap a) = a

    empty = IM.empty
    elems = IM.elems
    null  = IM.null

instance Container [a] where
    type Elem [a] = a

    empty = []
    elems = id
    null  = L.null
