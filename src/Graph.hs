{-# Language
    TupleSections,
    RankNTypes,
    MultiWayIf,
    BangPatterns,
    FlexibleContexts,
    LambdaCase #-}
module Graph where

import Data.Array.ST
import Data.Array       as A
import Data.IntMap.Strict as IM
import Data.Map.Strict    as M
import Control.Monad.Fix
import Data.List        as L
import Data.Maybe       as MB
import Control.Monad
import Data.Function
import Data.Monoid
import Data.STRef
import Data.Set   as S
import Data
import Control.Monad.ST.Strict

-----------------------------------------------------------------
-- Структуры
-- 1. Дерево
    -- array (1, n) Int Double [(Int, Double)]
    -- root Int
-- set

-- дерево
data Tree = Tree {
    tree :: Graph2,         -- прямой граф связей.
    root :: Int,            -- корень.
    rev  :: Array Int Int   -- обратный граф связей.
  }

type Graph2 = Array Int (IntMap Double) -- граф
type Circuit = [(Int,Int, Double)]              -- контур


minBazeCircuit, maxBazeCircuit :: Graph2 -> Circuit
minBazeCircuit = bazeCircuit minCircuit (compare`on`snd)
maxBazeCircuit = bazeCircuit maxCircuit (\a b -> compare (snd b) (snd a))

-- выбор базового контура.
bazeCircuit :: ([Circuit] -> Circuit)-> OrdF (Int, Double) -> Graph2 -> Circuit
bazeCircuit elect cmp gr = elect $ separation ribs
  where
    -- Минимальное из исходящих рёбер.
    min :: IntMap Double -> (Int, Double)
    !min = minimumBy cmp . IM.toList

    -- число вершин
    n :: Int
    !n = snd . bounds $ gr

    -- рёбра с наименьшей степенью.
    -- номер вершины начала, совпадает с индексом в списке.
    -- индексы в списке отсчитываются начиная от 1.
    minRibs :: [(Int, Double)]
    !minRibs = min <$!> A.elems gr

    ends :: Array Int Int
    !ends = listArray (1, n) $! fst <$> minRibs

    -- подсчитываем степень вершин.
    pots :: [(Int, Int)]
    !pots = (\a -> (fst $! head a, length a)) <$!>
        groupEq fst minRibs

    -- список вершин с нулевой степенью.
    listS0 :: [Int]
    !listS0 = S.toList $ S.difference
        (S.fromList [1..n]) (S.fromList $! fst <$!> pots)

    -- вершины траекторий, среди которых есть базовый контур.
    stDo :: [Int]
    stDo = IM.keys $
        -- вершины со степенью ноль.
        let go !s0 !sp = if L.null s0 then sp
                else
                let (hl:s0') = s0
                    !e    = ends A.! hl
                    !sp'  = IM.delete hl sp
                    !sp'' = IM.update upd e sp'
                    !s0'' = if Nothing == e`IM.lookup`sp''
                            then e:s0' else s0'
                in go s0'' sp'

            upd :: Int -> Maybe Int
            upd !x = if x > 0 then Just $! x - 1 else Nothing
        in go (listS0) (IM.fromList pots)

    ribs :: IntMap (Int, Double)
    !ribs = IM.intersection
        (IM.fromList $! zip [1..] minRibs)
        (IM.fromList $! (,"") <$!> stDo)


-- выбор контура с минимальной характеристикой
minCircuit, maxCircuit :: [Circuit] -> Circuit
minCircuit = electCircuit (compare`on`snd)
maxCircuit = electCircuit (\a b -> compare (snd b) (snd a))


electCircuit :: OrdF (Circuit, Double) -> [Circuit] -> Circuit
electCircuit cmp !c = fst $! minimumBy cmp c'
  where
    -- модифицируем список, чтобы сократить повторные расчёты.
    c' :: [(Circuit, Double)]
    !c' = (\x -> (x, zOfCircuit x)) <$!> c


-- разделение на множество контуров.
separation :: IntMap (Int, Double) -> [Circuit]
separation !x = if
    | Prelude.null x -> []
    | otherwise -> circuit : separation (IM.difference x cir)
  where
    cir :: IntMap (Int, Double)
    !cir = IM.fromList $! (\(a,b,c) -> (a, (b, c))) <$!> circuit

    -- контур
    circuit :: Circuit
    !circuit = stape st

    -- точка отсёта траекторий
    st :: Int
    !st = fst.head $! IM.toList x

    -- перебираем по шагам
    stape :: Int -> Circuit
    stape !a = case a`IM.lookup`x of
        Just (!i, !d) | i /= st -> (a, i, d):stape i
        _                       -> []

-- размыкание контура путём удаления одного ребра.
unlock :: F Circuit
unlock = tail


-- строим граф.
formGraph :: [(Ceil3, [(Ceil3, Double)])] -> Graph2
formGraph gr = array (1, n) ribs
  where
    -- число вершин.
    n :: Int
    n = length $ setOfC

    -- множество вершин.
    setOfC :: Set Ceil3
    setOfC = S.fromList $! fst <$!> gr

    -- список определяющий соответсвия.
    listOfC :: Map Ceil3 Int
    listOfC = M.fromList $! zip (S.toList setOfC) [1..]

    -- Меняем Ceil -> Int
    intForm :: [(Int, [(Int, Double)])]
    intForm = (\(a, b) -> (toInt a, MB.mapMaybe toInt' b)) <$!> gr
      where
        -- первичные ключи существуют все.
        toInt :: Ceil3 -> Int
        toInt  x = case x`M.lookup`listOfC of
            Just i  -> i

        -- вторичные ключи могут вести за пределы графа.
        toInt' :: (Ceil3, Double) -> Maybe (Int, Double)
        toInt' (x, d) = case x`M.lookup`listOfC of
            Just i  -> Just (i, d)
            Nothing -> Nothing

    -- список рёбер
    ribs :: [(Int, IntMap Double)]
    ribs = (\(x, y) -> (x, IM.fromList y)) <$!> intForm


-- сгруппировать эквивалентные по признаку.
groupEq :: Ord b => (a -> b) -> [a] -> [[a]]
groupEq f = groupBy ((==)`on`f) . sortBy (compare`on`f)
-----------------------------------------------------------------
--                 Построение оптимального контура             --
-----------------------------------------------------------------
-- потенциалы
-- v e(j) − v b(j) = c j − z
-- v e = c j - z - v b
pots :: Circuit -> [(Int, Double)]
pots c = pots'
  where
    -- считаем потенциалы вершин на контуре.
    pots' :: [(Int, Double)]
    pots' = (headOfCircuit c, 0) : do
        ((_,e,d), (_, b)) <- zip (init c) pots'
        return (e, d - zOfCircuit c - b)

formTree :: Graph2 -> Circuit -> ST s0
    (STArray s0 Int (Set Int), -- tree
     STArray s0 Int Int,             -- reverse tree
     Int)                            -- root of tree
formTree gr cir = do
    tree <- newArray (bounds gr) S.empty ::
        ST s (STArray s Int (Set Int))
    rev  <- newArray (bounds gr) 0 ::
        ST s (STArray s Int Int)
    forM_ (init cir) $ \(a, b, d) -> do
        writeArray tree a (S.singleton b)
        writeArray rev b a
    return (tree, rev, headOfCircuit cir)


headOfCircuit :: Circuit -> Int
headOfCircuit c = fst3 $ head c


-- характеристика контура.
zOfCircuit :: Circuit -> Double
{-#INLINE zOfCircuit#-}
zOfCircuit v = sum elemsOfV / toEnum (length v)
  where
    elemsOfV :: [Double]
    elemsOfV = (\(_,_,a) -> a) <$!> v


-- модификация элемента.
modifyArray :: (MArray array elem m, Ix index) =>
    array index elem -> index -> (elem -> elem) -> m ()
{-#INLINE modifyArray#-}
modifyArray a i f = do
    e <- readArray a i
    writeArray a i (f e)

minOptZ, maxOptZ :: Graph2 -> Double
minOptZ gr = optZ' (<=) gr (minBazeCircuit gr)
maxOptZ gr = optZ' (>=) gr (maxBazeCircuit gr)


--
--
--

optZ cmp gr cir = mfix (\fm cir -> do
    -- п.2.
    let (_, n) = bounds gr
        setM1  = S.fromList $ fst3 <$> cir
    m2  <- newSTRef $! S.difference (S.fromList [1..n]) setM1
    m1  <- newSTRef $! fromCir cir
    m0  <- newSTRef $! S.empty
    (tree, rev, root) <- formTree gr cir
    -- храним множество потенциалов.
    potSet <- newSTRef $! IM.fromList $! pots cir
    let z = zOfCircuit cir
        cycleA m0 m1 m2 tree rev root = do
            m1' <- readSTRef m1
            -- если список не пуст, делать.
            if isEmpty m1' then do
               -- (a)
                let (x, xs) = headTail m1'
                writeSTRef  m1 xs
                modifySTRef m0 (S.insert x)

                -- (b)
                potSet' <- readSTRef potSet
                let vi1 = v x potSet'
                    cycleB m0 m1 m2 tree rev root (y:ys) = do
                        let w = vi1 + cOf gr x y - z
                        m2'  <- readSTRef m2
                        isPr <- prev rev y x
                        if  | y`S.member`m2' -> do
                                modifySTRef m1 (snoc y)
                                modifySTRef m2 (S.delete y)
                                modifySTRef potSet (IM.insert y w)
                                modifyArray tree x (S.insert y)
                                writeArray rev y x
                                cycleB m0 m1 m2 tree rev root ys
                            | cmp w (v y potSet') -> cycleA m0 m1 m2 tree rev root
                            | isPr || preceded y cir -> do
                                cir' <- newCircuit gr rev cir x y []
                                return fm cir'
                            | otherwise -> do
                                modifySTRef potSet (IM.insert y w)
                                x' <- readArray rev y
                                modifyArray tree x' (S.delete y)
                                writeArray rev y x
                                m0' <- readSTRef m0
                                when (y`S.member`m0') $ do
                                    modifySTRef m0 (S.delete y)
                                    modifySTRef m1 (cons y)
                                cycleA m0 m1 m2 tree rev root
                    cycleB m0 m1 m2 tree rev root [] = cycleA m0 m1 m2 tree rev root
                cycleB m0 m1 m2 tree rev root (fst <$!> (IM.toList $! gr A.! x))
            else return z
    cycleA m0 m1 m2 tree rev root
  ) cir



-- находим предшествует ли элемент другому в дереве.
prev :: MArray a Int m => a Int Int -> Int -> Int -> m Bool
prev rev y x = do
    x' <- readArray rev x
    if  | x' == 0   -> return False
        | x' == y   -> return True
        | otherwise -> prev rev y x'


-- достраиваем от конца контура, до нужного элемента
-- XXX CirF принимает контур в обратном порядке, и возвращает
--     кусок контура тоже в обратном порядке.
cirF :: Int -> F Circuit
cirF y = \case
    (a,b,d):cir | b == y    -> [(a,b,d)]
                | otherwise -> (a,b,d):cirF y cir
    _                       -> []


v :: Int -> IntMap Double -> Double
v x p = fromJust $ x`IM.lookup`p

cOf :: Graph2 -> Int -> Int -> Double
{-#INLINE cOf#-}
cOf gr a b = fromJust $ b`IM.lookup`(gr A.! a)


preceded :: Int -> Circuit -> Bool
{-#INLINE preceded#-}
preceded a cir = elem a $ fst3 <$> cir


-- на основе контура строим двунаправленный список вершин.
fromCir :: Circuit -> DList Int
fromCir cir = toDList $ fst3 <$> cir


-- выделяем новый контур из графа
newCircuit :: MArray a Int m =>
    Graph2 -> a Int Int -> Circuit -> Int -> Int -> Circuit ->
    m Circuit
newCircuit gr rev cir y x l = do
    x' <- readArray rev x
    if  | x' == 0 -> return (reverse $ cirF y (reverse cir) ++ l)
        | x' == y -> return (reverse $ (x, x', cOf gr x x'):l)
        | otherwise -> newCircuit
            gr rev cir y x' ((x, x', cOf gr x x'):l)


fst3 (a, _, _) = a

-----------------------------------------------------------------
--                  Двунаправленный список                     --
-----------------------------------------------------------------
-- список с возможностью добавлять в начало и конец.
data DList a = DList [a] [a]


toDList :: [a] -> DList a
toDList a = DList a []


-- Добавляем элемент в начало списка.
cons :: a -> DList a -> DList a
cons a (DList x y) = DList (a:x) y


-- Добавляем элемент в конец списка.
snoc :: a -> DList a -> DList a
snoc a (DList x y) = DList x (a:y)


-- Приводим список к нормальной форме.
toNorm :: F (DList a)
toNorm (DList x y) = DList (x <> reverse y) []


-- находим голову и хвост списка.
headTail :: DList a -> (a, DList a)
headTail (DList x y) = if
    | s:sl <- x -> (s, DList sl y)
    | otherwise -> (head y', DList (tail y') [])
  where y' = reverse y


-- находим последний элемент и всё без него.
lastInit :: DList a -> (a, DList a)
lastInit (DList x y) = if
    | s:sl <- y -> (s, DList x sl)
    | otherwise -> (head x', DList [] (tail x'))
  where x' = reverse x


-- переворачиваем список.
reverseDList :: F (DList a)
reverseDList (DList x y) = DList y x


-- проверка на пустоту
isEmpty :: Query (DList a)
isEmpty (DList x y) = L.null x && L.null y


emptyList :: DList a
emptyList = DList [] []

instance Functor DList where
    fmap f (DList x y) = DList (fmap f x) (fmap f y)

-----------------------------------------------------------------
-----------------------------------------------------------------
optZ' :: (Double -> Double -> Bool) -> Graph2 -> Circuit -> Double
optZ' !cmp !gr !cir =
    -- п.2.
    let (_, n) = bounds gr
        setM1  = S.fromList $ fst3 <$> cir
        m2 = S.difference (S.fromList [1..n]) setM1
        m1 = fromCir cir
        m0 = S.empty
        (tree, rev, root) = formTree' cir
        -- храним множество потенциалов.
        potSet = IM.fromList $! pots cir
        z = zOfCircuit cir

    in cycleA z cmp gr m0 m1 m2 tree rev root potSet cir

cycleA :: Double -> (Double -> Double -> Bool) -> Graph2 -> Set Int ->
    DList Int -> Set Int -> IntMap (IntMap Double) -> IntMap Int -> Int ->
    IntMap Double -> Circuit -> Double
cycleA !z !cmp !gr !m0 !m1 !m2 !tree !rev !root !potSet !cir
    | isEmpty m1 = cycleB z vi1 x cmp gr m0' m1' m2 tree rev root potSet cir (fst <$!> (IM.toList $! gr A.! x))
    | otherwise  = z
  where
    (!x, !m1') = headTail m1
    !m0' = S.insert x m0
    !vi1 = v x potSet

cycleB :: Double -> Double -> Int -> (Double -> Double -> Bool) ->
    Graph2 -> Set Int -> DList Int -> Set Int -> IntMap (IntMap Double) ->
    IntMap Int -> Int -> IntMap Double -> Circuit -> [Int] -> Double
cycleB !z !vi1 !x !cmp !gr !m0 !m1 !m2 !tree !rev !root !potSet !cir !(y:ys)
    | y`S.member`m2 =
        let !m1' = snoc y m1
            !m2' = S.delete y m2
            !tree' = IM.adjust (IM.insert y (cOf gr x y)) x tree
            !rev'  = IM.insert y x rev
            !potSet' = IM.insert y w potSet
        in cycleB z vi1 x cmp gr m0 m1' m2' tree' rev' root potSet' cir ys
    | cmp w (v y potSet) = cycleA z cmp gr m0 m1 m2 tree rev root potSet cir
    | prev' rev y x || preceded y cir = optZ' cmp gr  (newCircuit' gr rev cir x y [])
    | otherwise =
        let !potSet' = IM.insert y w potSet
            !x' = fromJust $ y`IM.lookup`rev
            !tree' = IM.adjust (IM.delete y) x' tree
            !rev'  = IM.insert y x rev
            (!m0', !m1') = if y`S.member`m0
                then (S.delete y m0, cons y m1)
                else (m0, m1)
        in cycleA z cmp gr m0' m1' m2 tree' rev' root potSet' cir
  where
    w = vi1 + cOf gr x y - z
cycleB !z !vi1 !x !cmp !gr !m0 !m1 !m2 !tree !rev !root !potSet !cir ![] =
    cycleA z cmp gr m0 m1 m2 tree rev root potSet cir


formTree' :: Circuit -> (IntMap (IntMap Double), IntMap Int, Int)
formTree' cir = (
    IM.fromList $ (\(a, b, d) -> (a, IM.singleton b d)) <$> icir,
    IM.fromList $ (\(a, b, _) -> (b, a)) <$> icir,
    headOfCircuit cir)
  where icir = init cir


prev' :: IntMap Int -> Int -> Int -> Bool
prev' rev y x = case x`IM.lookup`rev of
    Just x' | x' == y   -> True
            | otherwise -> prev' rev y x'
    _                   -> False


newCircuit' :: Graph2 -> IntMap Int -> Circuit -> Int -> Int -> Circuit -> Circuit
newCircuit' gr rev cir y x l = case x`IM.lookup`rev of
    Just x' | x' == y   -> reverse $ (x, x', cOf gr x x'):l
            | otherwise -> newCircuit'
                gr rev cir y x' ((x, x', cOf gr x x'):l)
    _ -> reverse $ cirF y (reverse cir) ++ l
