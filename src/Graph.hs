{-# Language
    MultiWayIf#-}
module Graph where

import Prelude     as P     hiding (lookup)
import Data.Vector as V     hiding (empty, concatMap, zip)
import Data.IntMap as IM    hiding (empty)
import Data.Map    as M     hiding (empty)
import Data.List   as L
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

    apexes :: Map Ceil3 Int
    apexes = M.fromList $ zip apexList [1..]

    apexList :: [Ceil3]
    apexList = myNub $ concatMap (\(c1, c2, _) -> [c1, c2]) ls

    vergeSet :: Vector (Int, IntMap Double)
    vergeSet = V.fromList $ set <$> vergeLists
      where
        set :: [(Int, Int, Double)] -> (Int, IntMap Double)
        set ls = (a, IM.fromList $ (\(_, a, d) -> (a, d)) <$> ls)
          where
            a = (\(a, _, _) -> a) $ L.head ls

    vergeLists :: [[(Int, Int, Double)]]
    vergeLists = groupBy (useFst (==)) $ sortBy (useFst compare) $
        transformVerge <$> ls

  --useFst :: (a -> b -> c) -> (a, _, _) -> (b, _, _) -> c
    useFst f (a1 ,_ , _) (a2, _, _) = f a1 a2

    {-#INLINE indexOfApexe#-}
    indexOfApexe :: Ceil3 -> Int
    indexOfApexe x = case x`M.lookup`apexes of
        Just i -> i

    {-#INLINE transformVerge#-}
    transformVerge :: (Ceil3, Ceil3, Double) -> (Int, Int, Double)
    transformVerge (a, b, d) = (indexOfApexe a, indexOfApexe b, d)

instance Empty GraphA where
    empty = GraphA empty empty
