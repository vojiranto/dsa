{-# Language
    MultiWayIf,
    ScopedTypeVariables,
    GADTs#-}
module Graph where

import Prelude     as P     hiding (lookup)
import Data.Vector as V     hiding (empty, concatMap, zip)
import Data.IntMap as IM    hiding (empty)
import Data.Map    as M     hiding (empty)
import Data.List   as L
import SymbolicImage

import Data
import Empty

data GraphA a = GraphA {
    verges :: Vector (IntMap Double), -- множества ребер
    apexes :: Map a Int               -- множество вершин
  }


-- Построение графа с обратной ориентацией рёбер.
formGraphA' :: Ord a => [(a, a, Double)] -> GraphA a
formGraphA' ls = formGraphA $ (\(c1, c2, d) -> (c2, c1, d)) <$> ls


-- Построение графа по списку рёбер.
formGraphA :: Ord a => [(a, a, Double)] -> GraphA a
formGraphA ls = GraphA verges apexes
  where
    verges = V.fromList $ vergeSet

    apexes = M.fromList $ zip apexList [0..]

    apexList = myNub $ concatMap (\(c1, c2, _) -> [c1, c2]) ls

    vergeSet :: [IntMap Double]
    vergeSet = fmap snd $ sortBy (useFst2 compare) $ set <$> vergeLists
      where
        set :: [(Int, Int, Double)] -> (Int, IntMap Double)
        set ls = (a, IM.fromList $ (\(_, a, d) -> (a, d)) <$> ls)
          where
            a = (\(a, _, _) -> a) $ L.head ls

    vergeLists :: [[(Int, Int, Double)]]
    vergeLists = groupBy (useFst (==)) $ sortBy (useFst compare) $
        transformVerge <$> ls

    {-#INLINE indexOfApexe#-}
    indexOfApexe x = case x`M.lookup`apexes of
        Just i -> i

    {-#INLINE transformVerge#-}
    transformVerge (a, b, d) = (indexOfApexe a, indexOfApexe b, d)


-- см. стр. 111.
formN :: GraphA a -> Vector Int
formN (GraphA v _) = (\(i, d) -> i)
    <$> L.minimumBy (\(_, d1) (_, d2) -> compare d1 d2)
    <$> IM.toList <$> v


iToList :: Vector a -> [(Int, a)]
iToList x = V.toList $ V.imap (\i a -> (i, a)) x


-- Для всех вершин, чей индекс входящих не равен нулю.
formS :: Vector Int -> [(Int, Int)]
formS x = zip (snd . L.head <$> s) (L.length <$> s)
  where
    s = groupBy (\(_, b1) (_, b2) -> b1 == b2) $
        sortBy (\(_, b1) (_, b2) -> compare b1 b2) $ iToList x

instance Ord a => Empty (GraphA a) where
    empty = GraphA empty empty

useFst f (a1 ,_ , _) (a2, _, _) = f a1 a2
useFst2 f (a1, _) (a2, _) = f a1 a2
