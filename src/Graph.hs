{-# Language
    TupleSections,
    RankNTypes,
    MultiWayIf,
    BangPatterns,
    FlexibleContexts,
    ScopedTypeVariables,
    LambdaCase #-}
module Graph where

import Data.Array.ST
import Data.Array       as A
import Data.IntMap.Strict as IM
import Data.Map.Strict    as M
import Control.Monad.Fix
import Data.List        as L
import Data.Maybe       as MB
import Data.Array.MArray
import Control.Monad
import Data.Function
import Data.Monoid
import Data.STRef
import System.IO.Unsafe
import Data.Set   as S
import Data
import Control.Monad.ST.Strict

eps = 1e-6

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
minBazeCircuit = bazeCircuit (compare`on`snd)
maxBazeCircuit = bazeCircuit (\a b -> compare (snd b) (snd a))


-- упаковываем список из одинаковых эллементов.
-- forall i, i : x_i == x_j
packEqList :: Eq a => [a] -> (a, Int)
packEqList (x:xs) = (x, length xs + 1)
packEqList []     = error "packEqList []"


-- FIXME head separation => elect separation
--          когда починю функцию separation
-- выбор базового контура.
bazeCircuit :: OrdF (Int, Double) -> Graph2 -> Circuit
bazeCircuit cmp gr = head $ separation $ ribs stDo minRibs
  where
    -- число вершин
    n :: Int
    b@(_, !n) = bounds $ gr

    -- рёбра с наименьшей степенью.
    -- номер вершины начала, совпадает с индексом в списке.
    -- индексы в списке отсчитываются начиная от 1.
    minRibs :: [(Int, Double)]
    !minRibs = (minimumBy cmp . IM.toList) <$!> A.elems gr

    -- подсчитываем степень вершин.
    pots :: [(Int, Int)]
    !pots = packEqList <$> (groupEq id $ fst <$> minRibs)

    stDo :: [Int]
    stDo = IM.keys $ runST $ do
        -- (1) задаём множество рёбер.
        ns <- newSTRef $ IM.fromList $ zip [1..] (fst <$> minRibs)
        -- (2) список вершин с потенциалами (p /= 0)
        s1 <- newSTRef $ IM.fromList pots
        let loop (i1:xs) = do
                -- (b)
                ns' <- readSTRef ns
                let i2 = i1 `jLookup` ns'                           -- XXX
                modifySTRef ns (IM.delete i1)
                -- (c)
                s1' <- readSTRef s1
                let p = i2 `jLookup` s1'                            -- XXX
                when (p > 1) $ do
                    writeSTRef s1 (IM.insert i2 (p - 1) s1')
                when (p == 1) $ do
                    writeSTRef s1 (IM.delete i2 s1')
                let l = if p > 1 then [] else [i2]
                --return $! unsafePerformIO $! print i1
                loop (l ++ xs)
            -- (5) null s0 => return ns
            loop [] = readSTRef ns
        -- (3) список вершин с потенциалами (p == 0)
        loop $ S.toList $ S.difference
            -- s0 = A \ s1
            (S.fromList [1..n]) (S.fromList $! fst <$!> pots)

jLookup i n = fromJust $ i `IM.lookup` n

-- выбор контура с минимальной характеристикой
minCircuit, maxCircuit :: Elect Circuit
minCircuit = electCircuit (compare`on`snd)
maxCircuit = electCircuit (\a b -> compare (snd b) (snd a))


electCircuit :: OrdF (Circuit, Double) -> Elect Circuit
electCircuit cmp !c = fst $! minimumBy cmp c'
  where
    -- модифицируем список, чтобы сократить повторные расчёты.
    c' :: [(Circuit, Double)]
    !c' = (\x -> (x, zOfCircuit x)) <$!> c


ribs :: [Int] -> [(Int, Double)] -> IntMap (Int, Double)
ribs !stDo !minRibs = IM.fromList $ f st r
  where
    st = sort stDo
    r  = zip [1..] minRibs

    f (s:sx) ((i,(r, d)):rx)
        | s == i    = (i, (r, d)) : f    sx  rx
        | otherwise =               f (s:sx) rx
    f _ _           = []


-- FIXME порождает бесконечный список, а должн конечный. Однако
--       взятие головы списка норм.
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
        Just (!i, !d) | i /= st   -> (a, i, d):stape i
                      | otherwise -> [(a, i, d)]
        Nothing                   -> error "Контур не замкнут!!!"





-- строим граф.
formGraph ::forall a . Ord a => [(a, [(a, Double)])] -> Graph2
formGraph gr = array (1, n) ribs
  where
    -- число вершин.
    n :: Int
    n = length $ setOfC

    -- множество вершин.
    setOfC :: Ord a => Set a
    setOfC = S.fromList $! fst <$!> gr

    -- список определяющий соответсвия.
    listOfC :: Ord a => Map a Int
    listOfC = M.fromList $! zip (S.toList setOfC) [1..]

    -- Меняем Ceil -> Int
    intForm :: [(Int, [(Int, Double)])]
    intForm = (\(a, b) -> (toInt a, MB.mapMaybe toInt' b)) <$!> gr
      where
        -- первичные ключи существуют все.
        toInt :: Ord a => a -> Int
        toInt  x = case x`M.lookup`listOfC of
            Just i  -> i

        -- вторичные ключи могут вести за пределы графа.
        toInt' :: Ord a => (a, Double) -> Maybe (Int, Double)
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
-- модификация элемента.
modifyArray :: (MArray array elem m, Ix index) =>
    array index elem -> index -> (elem -> elem) -> m ()
{-#INLINE modifyArray#-}
modifyArray a i f = do
    e <- readArray a i
    writeArray a i (f e)


-----------------------------------------------------------------
--                                                             --
-----------------------------------------------------------------
-- потенциалы
-- v e(j) − v b(j) = c j − z
-- v e = c j - z - v b
-- NOTE length cir > 1 otherwise error
pots :: Circuit -> Double -> [(Int, Double)]
pots []  _ = error "pots: empty cir."
pots cir z = (headOfCircuit cir, 0) : pots' 0 (init cir)
  where
    pots' :: Double -> Circuit -> [(Int, Double)]
    pots' b ((_, i2, c):xs) = (i2, w) : pots' w xs
      where w = b + c - z + eps
    pots' _  _              = []


headOfCircuit :: Circuit -> Int
headOfCircuit = fst3 . head

-- на основе контура строим двунаправленный список вершин.
fromCir :: Circuit -> DList Int
fromCir cir = toDList $ fst3 <$> cir

-- характеристика контура.
zOfCircuit :: Circuit -> Double
{-#INLINE zOfCircuit#-}
zOfCircuit v = sum (tr3 <$> v) / toEnum (length v)

cOf :: Graph2 -> Int -> Int -> Double
{-#INLINE cOf#-}
cOf gr a b = case b`IM.lookup`(gr A.! a) of
    Just a -> a
    Nothing -> error $ "cOf " ++ show a ++ " " ++ show b

-- разбираем кортеж на запчасти.
fst3 (a, _, _) = a ; snd3 (_, a, _) = a ;tr3  (_, _, a) = a


minOptZ gr c = runST $ optZ' gr c

optZ' gr cir = let
  loop1 cir = do
    let -- входной контур.
        -- <Mc, Nc>
        mc  = fst3 <$> cir
        nc  = cir
        -- NOTE для ускорения проверок
        --      o(log(n)) < o(n)
        smc = S.fromList mc

        -- (1)
        z   = zOfCircuit nc
        -- (2,3,4)
        p   = pots cir z
        n   = snd . bounds $ gr

    -- (4.2) создаём дерево по контуру.
    -- NOTE  у Петренок в алгоритме нет этого пункта (sic!)
    tree <- newArray (bounds gr) 0 :: ST s (STArray s Int Int)
    let -- наличие ребра i1i2 <=> наличию e i2 предка i1
        addToTree (i1, i2) = writeArray tree i2 i1

    -- добавляем в дерево рёбра из nc.
    forM_ (init nc) $ \(i1, i2, _) -> addToTree (i1, i2)

    -- (5)
    nc <- newSTRef $ init nc

    -- (6)  Задаём множества M0, M1 и M2
    --      Задаём хранилища для потенциалов.

    m0 <- newSTRef S.empty
    m1 <- newSTRef $ toDList mc

    -- NOTE все вершины из mc уже в m1 => их нужно вычесть из m2.
    m2 <- newSTRef $ S.difference (S.fromList [1..n]) smc
    p  <- newSTRef $ IM.fromList p
    st <- newSTRef S.empty
    -- (7)
    let loop2 = do
            m1' <- readSTRef m1
            -- Если not null m1 =>
            if (not.isEmpty $ m1') then do
                -- (a)
                let (i1, m1'') = headTail m1'
                writeSTRef m1 m1''
                -- (b)
                modifySTRef m0 (S.insert i1)
                -- (c)

                let loop3 (i2:i2s) = do
                        -- (*)
                        -- p множество потенциалов.
                        p' <- readSTRef p
                        let -- потенциалы вершин
                            v1 = i1`jLookup`p'
                            v2 = i2`jLookup`p'
                            -- характеристика ребра
                            c  = cOf gr i1 i2
                            -- пересчитанный потенциал
                            w  = v1 + c - z + eps
                            j  = (i1, i2)
                        -- (**)
                        m2'      <- readSTRef m2
                        m0'      <- readSTRef m0
                        prec     <- preceded i2 i1 tree smc
                        st' <- readSTRef st
                        if  | i2 `S.member`m2' -> do
                                -- добавить посчитанный потенциал.
                                modifySTRef p (IM.insert i2 w)
                                -- добавляем ветвь в дерево
                                addToTree j
                                -- убираем вершину из незадействованных
                                modifySTRef m2 (S.delete i2)
                                -- вносим в очередь на обработку
                                modifySTRef m1 (snoc i2)
                                -- возврат.
                                loop3 i2s
                            -- (**.2) отброс дуги.
                            -- NOTE   эта возможность не учтена у
                            --        Петренко (sic!)
                            | w >= v2 -> loop3 i2s
                            -- (***)
                            | prec  -> do
                                -- дописываем замыкающее контур ребро
                                addToTree j
                                cir' <- сircuit i2 tree gr cir
                                --if counter < 100
                                --then
                                loop1 cir'
                                --else return z
                            -- (***.2)
                            | j`S.member`st' -> loop3 i2s
                            | otherwise -> do
                                -- ведём учёт вершин, по которым
                                -- вели пересчёт.
                                modifySTRef st (S.insert j)
                                -- вставляем пересчтанный потенциал.
                                -- NOTE этот пункт пропущен у
                                --      Петренко (sic!).
                                modifySTRef p  (IM.insert i2 w)
                                -- NOTE дерево у нас перевёрнуто,
                                --      поэтому заменив кому-то
                                --      предка мы одновременно
                                --      и убираем какую-то другую
                                --      дугу
                                addToTree j
                                -- если вершина уже отброшена =>
                                -- вернуть в очередь на обработку.
                                when (i2 `S.member` m0') $ do
                                    modifySTRef m0 (S.delete i2)

                                -- переносим i2 в начло m1
                                modifySTRef m1 (lDelete i2)
                                modifySTRef m1 (cons i2)
                                loop3 i2s
                    loop3 _  = do
                        loop2
                loop3 (fst <$> (IM.toList $ gr A.! i1))
            -- (8)
            else return z
    loop2
  in loop1 cir

printF a = return $! unsafePerformIO $! print a
-- предшествует ли вершина i1 вершине i2?
-- NOTE Если существует контур, то i2 должна лежать на нём.
--      иначе forever loop
preceded :: forall m arr. MArray arr Int m =>
    Int -> Int -> arr Int Int -> Set Int -> m Bool
preceded i2 i1 tree smc = do
    b1 <- return   $ i2 `S.member` smc
    b2 <- preceded' i1 0
    return $ b1 || b2
  where
    -- i под i2
    preceded' ::  Int -> Int -> m Bool
    preceded' i count = do
        -- посмотреть, кто прямо над нами.
        i' <- readArray tree i
        --printF i'
            -- если никого, то i2 не может быть над нами.
        if  | count > 10^7 -> error "preceded: В дереве обнаружен цикл."
            | i' == 0      -> return False
            -- если i2, то мы получили результат.
            | i' == i2     -> return True
            -- иначе посмотртреть, кто над следующим.
            | otherwise    -> preceded' i' (count + 1)

-- явлется ли вершина i1 родителем для i2.
isPrecedFor :: MArray arr Int m => Int -> Int ->arr Int Int -> m Bool
isPrecedFor i1 i2 tree = do
    pr <- readArray tree i2
    return $ i2 == pr


-- выделяем контур.
-- NOTE (1!) Контур должен быть.
--      (2!) i2 должна лежать на котуре.
-- XXX Если вершина находится на изначальном контуре, то получаем ошибку.
сircuit st tree gr cir = сircuitf st []
  where
    сircuitf i2 l = do
        i1 <- readArray tree i2
        if  -- если уперлись в корень, то нужно просмотреть в контуре.
            | i1 == 0   -> return (rev takeC cir ++ l)
            -- если дошли до старта => замкнули контур
            | i1 == st  -> return ((i1, i2, cOf gr i1 i2):l)
            -- иначе => поднимаемся выше.
            | otherwise -> сircuitf  i1 ((i1, i2, cOf gr i1 i2):l)

    takeC ((a, b, c):xs)
        | a == st   =  [(a, b, c)]
        | otherwise = (a, b, c):takeC xs
    takeC _ = error "takeC !!!"

    rev f a = reverse . f . reverse $ a


-----------------------------------------------------------------
--                  Двунаправленный список                     --
-----------------------------------------------------------------
-- список с возможностью добавлять в начало и конец.
data DList a = DList [a] [a]

lDelete :: Eq a => a -> F (DList a)
lDelete a (DList x y) = DList (L.delete a $ x <> reverse y) []

rDelete :: Eq a => a -> F (DList a)
rDelete a (DList x y) = DList [] (L.delete a $ y <> reverse x)

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

