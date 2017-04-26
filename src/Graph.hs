{-# Language
    MultiWayIf,
    ScopedTypeVariables,
    TypeFamilies,
    GADTs#-}
module Graph where

import Prelude     as P     hiding (null, lookup)
import Data.Vector as V     hiding (null, empty, concatMap, zip)
import Data.IntMap as IM    hiding (null, elems, empty)
import Data.Map    as M     hiding (null, elems, empty)
import Data.IntSet as IS    hiding (null, elems, empty)
import Data.List   as L     hiding (null)
import Data.Function
import SymbolicImage

import Data
import Container

data GraphA a where
    GraphA :: Ord a => {
        verges :: Vector (IntMap Double), -- множества ребер
        apexes :: Map a Int               -- множество вершин
    } -> GraphA a


-- Построение графа с обратной ориентацией рёбер.
formGraphA' :: Ord a => [(a, a, Double)] -> GraphA a
formGraphA' ls = formGraphA $ (\(c1, c2, d) -> (c2, c1, d)) <$> ls


-- Построение графа по списку рёбер.
formGraphA :: forall a. Ord a => [(a, a, Double)] -> GraphA a
formGraphA ls = GraphA verges apexes
  where
    verges :: Vector (IntMap Double)
    verges = V.fromList $ vergeSet

    apexes :: Map a Int
    apexes = M.fromList $ zip apexList [0..]

    apexList :: [a]
    apexList = myNub $ concatMap (\(c1, c2, _) -> [c1, c2]) ls

    vergeSet :: [IntMap Double]
    vergeSet = fmap snd $ sortBy (compare`on`fst) $ set <$> vergeLists
      where
        set :: [(Int, Int, Double)] -> (Int, IntMap Double)
        set ls = (a, IM.fromList $ (\(_, a, d) -> (a, d)) <$> ls)
          where
            {-#INLINE a#-}
            a :: Int
            a = (\(a, _, _) -> a) $ L.head ls

    vergeLists :: [[(Int, Int, Double)]]
    vergeLists = groupEq (\(a, _, _) -> a) $ transformVerge <$> ls

    {-#INLINE indexOfApexe#-}
    indexOfApexe :: a -> Int
    indexOfApexe x = case x`M.lookup`apexes of
        Just i -> i

    {-#INLINE transformVerge#-}
    transformVerge :: (a, a, Double) -> (Int, Int, Double)
    transformVerge (a, b, d) = (indexOfApexe a, indexOfApexe b, d)

-- TODO
-- Переработать уменьшив число трансформаций между форматами!!!


-- см. стр. 111.
formN :: GraphA a -> VergeA
formN = IM.fromList.iToList.formN'
  where
    formN' :: GraphA a -> Vector (Int, Double)
    formN' (GraphA v _) = L.minimumBy (compare`on`snd) <$>
        IM.toList <$> v


    iToList :: Vector a -> [(Int, a)]
    iToList x = V.toList $ V.imap (\i a -> (i, a)) x


-- Для всех вершин, чей индекс входящих не равен нулю.
formS :: GraphA a -> IntMap Int
formS gr = IM.fromList $ zip (snd . L.head <$> s) (L.length <$> s)
  where
    s :: [[(Int, Int)]]
    s = groupEq snd $ (\(a, (b, _)) -> (a, b)) <$>
        (IM.toList $ formN gr)


formS0 :: Ord a => GraphA a -> [Int]
formS0 gr = IS.toList $ IS.difference
    (IS.fromList $ elems gr)
    (IS.fromList $ fst <$> (IM.toList $ formS gr))


type ApexesI = IntMap Int
type Verge  = (Int, Int)


-- Так как для каждой вершины существует только одна исходящая
-- дуга, то такое предатавление Ns допустимо.
type VergeA = IntMap (Int, Double)
type State = (
    [Int],      -- список вершин, чья степень равна нулю.
    ApexesI,   -- список вершин с степенянми не равными нулю.
    VergeA      -- множество ребер.
  )


bazeContour :: Ord a => GraphA a -> [Verge]
bazeContour gr = toVerges $ (L.minimumBy cntCmp $
    tr $ until p f gr')
  where
    -- сравниваем два контура.
    cntCmp :: VergeA -> VergeA -> Ordering
    cntCmp = compare`on`zOfConter

    toVerges :: VergeA -> [Verge]
    toVerges x = (\(a,(b, c)) -> (a, b)) <$> IM.toList x

    p :: State -> Bool
    p (a, _, _) = null a

    f :: F State
    f (a, s, n) = (a', s', n')
      where
        a' :: [Int]
        a' = if
            | solv      -> j:L.tail a
            | otherwise ->   L.tail a

        s' :: ApexesI
        s' = if
            | solv      -> IM.delete             i s
            | otherwise -> IM.adjust (\x -> x-1) i s

        n' :: VergeA
        n' = IM.delete i n

        j :: Int
        j = case i`IM.lookup`n of Just (j, _) -> j

        solv :: Bool
        solv = case j`IM.lookup`s of Just i -> i - 1 == 0

        i :: Int
        i = L.head a

    gr' :: State
    gr' = (formS0 gr, formS gr, formN gr)

    -- разделение на контуры.
    tr :: State -> [VergeA]
    tr (_, _, v) = separation v
      where
        separation :: VergeA -> [VergeA]
        separation v = if
            | not.null $ v  -> ext:separation (IM.difference v ext)
            | otherwise     -> empty
          where
            ext :: VergeA
            ext = extract v

            extract :: VergeA -> VergeA
            extract v = IM.fromList $ st : go v (fst.snd $ st)
              where
                st :: (Int, (Int, Double))
                st = IM.findMin v

                go :: VergeA -> Int -> [(Int, (Int, Double))]
                go v k = case k`IM.lookup`v of
                    Just (i, d) -> (k, (i, d)) : go v i
                    Nothing     -> empty


groupEq :: Ord b => (a -> b) -> [a] -> [[a]]
groupEq f = groupBy ((==)`on`f) . sortBy (compare`on`f)


instance Ord a => Container (GraphA a) where
    type Elem (GraphA a) = Int

    empty = GraphA empty empty
    elems (GraphA _ apexes)= elems apexes
    null (GraphA _ apexes) = null apexes

useFst f (a1 ,_ , _) (a2, _, _) = f a1 a2
useFst2 f (a1, _) (a2, _) = f a1 a2

-----------------------------------------------------------------

toListOfV :: VergeA -> [Int]
toListOfV v = fst <$> elems v


zOfConter :: VergeA -> Double
zOfConter v = L.sum elemsOfV / toEnum (L.length elemsOfV)
  where
    elemsOfV :: [Double]
    elemsOfV = snd <$> elems v


-- (1) стр. 110.
pots :: VergeA -> [(Int, Double)]
pots v = go start 0 v
  where
    start :: Int
    start = L.head $ IM.keys v

    go :: Int ->  Double -> VergeA -> [(Int, Double)]
    go i d v = if
        | Just (int, dou) <- i' -> let
                d' = dou + d - z
            in (i, d) : go int d' v'
        | otherwise      -> []
      where
        v' :: VergeA
        v' = IM.delete i v

        i' :: Maybe (Int, Double)
        i' = i `IM.lookup` v

    z :: Double
    z = zOfConter v

-- (2) стр. 110
startM1 :: VergeA -> [Int]
startM1 v = fst <$> pots v
