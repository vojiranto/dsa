{-# Language
    MultiWayIf #-}
module Graph where

import Prelude     as P     hiding (lookup)
import Data.Vector          hiding (empty, concatMap, zip)
import Data.IntMap as IM    hiding (empty)
import Data.Map    as M     hiding (empty)
import SymbolicImage

import Data
import Empty

data GraphA = GraphA
    (Vector (IntMap Double))        -- множества ребер
    (Map Ceil3 Int)                 -- множество вершин


formGraphA :: [(Ceil3, Ceil3, Double)] -> GraphA
formGraphA ls = GraphA verges apexes
  where
    verges = undefined
    apexes = M.fromList $ zip apexList [1..]

    apexList :: [Ceil3]
    apexList = myNub $ concatMap (\(c1, c2, _) -> [c1, c2]) ls

    vergeList :: [(Int, Int, Double)]
    vergeList = transformVerge <$> ls

    {-#INLINE indexOfApexe#-}
    indexOfApexe :: Ceil3 -> Int
    indexOfApexe x = case x`M.lookup`apexes of
        Just i -> i

    {-#INLINE transformVerge#-}
    transformVerge :: (Ceil3, Ceil3, Double) -> (Int, Int, Double)
    transformVerge (a, b, d) = (indexOfApexe a, indexOfApexe b, d)

instance Empty GraphA where
    empty = GraphA empty empty
