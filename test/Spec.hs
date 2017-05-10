{-#Language ScopedTypeVariables, BangPatterns, MultiWayIf#-}
module Main where

import Graph
import Point
import Shift
import Control.Monad
import Control.Monad.Extra
import SymbolicImage
import Stratification
import Control.Monad.ST as ST
import Data.IntMap      as IM
import Data.Array.ST    as SA
import Data.Array       as A
import Data.Set         as S
import Test.QuickCheck

main = do
    putStrLn ""
    unless test1 $ print "Err 1"
    when   test2 $ print "Err 2"
    when   test3 $ print "Err 3"
    unless test4 $ print "Err 4"
    unless test5 $ print "Err 5"
    unless test6 $ print "Err 6"
    unless test7 $ print "Err 7"
    unless test8 $ print "Err 8"
    unless (rad 100 100 == pi/2) $ print "Err 8"
    unless test9 $ print "Err 9"
    unless test10 $ print "Err 10"
    unless test11 $ print "Err 11"
    unless test12 $ print "Err 12"
    unless test13 $ print "Err 13"
    quickCheck
        (\x y -> abs (shift fx1 (Point x y) - shift fx2 (Point x y)) < 1e-10)
    return $! testCir1
    return $! testCir2
    return $! testCir3
    return $! testCir4
    return $! testCir5
    print "Ok!"

fx1 = "1 + 0.3*y - 1.4 * pot(x, 2)"
fx2 = "1 - 1.4 * pot(x, 2) + 0.3*y"

test1 = testPreced S.empty arr1 3 4 -- True
test2 = testPreced S.empty arr2 2 3 -- False
test3 = testPreced s1      arr2 2 3 -- False

-- контур замыкается по дереву.
test4 = cir11 == testCircuit 1 arr1 gr1 cir11 -- True

-- контур замыкается по старому контуру.
test5 = cir12 == testCircuit 1 arr2 gr1 cir12 -- True

-- проверяем то, что потенциалы извлекаются верно.
test6 = 16 == cOf gr2 3 4
test7 = 9  == cOf gr2 2 3

test8 = (zOfCircuit $ (\a -> (1,1,1)) <$> [1..9]) == 1

test9 = (1 == minOptZ gr (minBazeCircuit gr))
    where gr = formGr 1 gr1P

test10 = (-1 == minOptZ gr (minBazeCircuit gr))
    where gr = formGr (-1) gr1P

test11 = 4 == (length $ pure Point <*> [1, 2] <*> [1, 2])

test12 = 10 == int 40 (rad 40 10)

test13 = reverse [-0.25, 0..1.25] == mySequence 1 1

testPreced set arr i1 i2 = runST $ do
    tree <- makeTree arr
    preceded i1 i2 tree set

-- 1. Контур замкнулся в дереве.                    [+]
    -- arr1
-- 2. Новый контур замкнулся по старому.
        -- в дереве нет контуров.
-- 3. Ошибка?
testCircuit st tr gr cir = runST $ do
    tree <- makeTree tr
    сircuit st tree gr cir

testCir1 = runST $ do
    tree <- makeTree arr4 :: ST s (STArray s Int Int)
    -- один предшествует пяти.
    t <- preceded 1 5 tree S.empty
    unless t $ error "testCir err1!!!"
    writeArray tree 1 5
    cir <- сircuit 1 tree gr1 []
    unless (cir3 == cir) $ error "testCir err2!!!"

testCir2 = runST $ do
    tree <- makeTree (rv <$> arrEr) :: ST s (STArray s Int Int)
    -- один предшествует пяти.
    t <- preceded 120 8 tree S.empty
    if  | not t     -> return ()
        | otherwise -> printF "testCir2 Err!!!"

testCir3 = runST $ do
    tree <- makeTree (rv <$> arrEr2) :: ST s (STArray s Int Int)
    -- один предшествует пяти.
    t <- preceded 120 8 tree S.empty
    if  | not t     -> return ()
        | otherwise -> printF "testCir3 Err!!!"

testCir4 = runST $ do
    tree <- makeTree (rv <$> [(2,1)]) :: ST s (STArray s Int Int)
    t <- preceded 1 2 tree S.empty
    if  | t     -> return ()
        | otherwise -> printF "testCir4 Err!!!"

testCir5 = runST $ do
    tree <- makeTree (rv <$> arrEr3) :: ST s (STArray s Int Int)
    t <- preceded 120 8 tree smc3
    if  | not t -> return ()
        | otherwise -> printF "testCir5 Err!!!"


-----------------------------------------------------------------
--                          DATA MAKERS                        --
-----------------------------------------------------------------

makeTree arr = do
    let n = maximum $ plat arr
    tree <- newArray (1, n) 0 :: ST s (STArray s Int Int)
    forM_ arr $ \(a, b) -> writeArray tree b a
    return tree

formSimpleNode i (a, b) = (a, IM.fromList $ addSnd i <$> b)

formSimpleNode2 :: (Int, [Int]) -> (Int, IntMap Double)
formSimpleNode2 (a, b) = (a, IM.fromList $ addSnd2 <$> b)

addSnd i a = (a, i)
addSnd2 a = (a, toEnum a ^ 2)


addTr  (a, b) = (a, b, 0)
addTr2 (a, b) = (a, b, toEnum $ a+b)

--
makeSimpleCir, makeSimpleCir2  :: [Int] -> Circuit
makeSimpleCir  = makeSimpleCirT addTr
makeSimpleCir2 = makeSimpleCirT addTr2

makeSimpleCirT f x = makeSimpleCir' (x ++ [head x])
  where
    makeSimpleCir' (x:y:xs) = f (x, y):makeSimpleCir' (y:xs)
    makeSimpleCir' _        = []

plat :: [(a, a)] -> [a]
plat list = concat $ do
    (a, b) <- list
    return [a, b]

-- строим граф с потенциалами рёбер i по схеме gr
formGr :: Double -> [(Int, [Int])] -> Array Int (IntMap Double)
formGr i gr = array (1 , maximum gr') $ (formSimpleNode i <$> gr)
  where gr' = concat $ (\(a, b) -> a:b) <$> gr


-----------------------------------------------------------------
--                          DATA                               --
-----------------------------------------------------------------
s1 = S.singleton 3

arr1 :: [(Int, Int)]
arr1 = [
    (1, 2), (2, 3), (3, 4), (4, 5),
    (6, 7), (7, 8), (5, 6), (8, 1)]

arr2 :: [(Int, Int)]
arr2 = [
    (1, 2), (1, 3), (1, 4), (4, 5),
    (6, 7), (7, 8), (5, 6)]

arr3 :: [(Int, Int)]
arr3 = [(1, 2), (2, 3), (3, 4)]

-- Контуры
cir11 = makeSimpleCir [1,2,3,4,5,6,7,8]

-- для деревьев.
cir12 = makeSimpleCir [1,4,5,6,7,8]
cir21 = makeSimpleCir [1,4]

cir3 = makeSimpleCir [1,2,3,4,5]

gr1 :: Array Int (IntMap Double)
gr1 = array (1, 8) $ formSimpleNode 0 <$> gr1P

gr1P = [
    (1, [2, 3, 4]),
    (2, [3]),
    (3, [4]),
    (4, [1, 5]),
    (5, [6, 1]),
    (6, [7]),
    (7, [8]),
    (8, [1])]

gr2 :: Array Int (IntMap Double)
gr2 = array (1, 8) $ formSimpleNode2 <$> [
    (1, [2, 3, 4]),
    (2, [3]),
    (3, [4]),
    (4, [1, 5]),
    (5, [6, 1]),
    (6, [7]),
    (7, [8]),
    (8, [1])]

-----------------------------------------------------------------
arr4 :: [(Int, Int)]
arr4  = [(1,2), (2,3), (3, 4), (4, 5)]

arr5 = [(1,2)]

rv (a, b) = (b, a)

arrEr = [(8,187),(10,187),(12,187),(15,187),(18,190),(21,191),(24,191),(26,191),(78,128),(79,128),(80,128),(82,127),(94,118),(95,115),(96,115),(98,115),(100,115),(115,7),(117,7),(118,8),(119,7),(120,8),(121,7),(127,18),(128,21),(129,21),(131,21),(187,79),(190,94),(191,96),(195,79),(196,82),(198,96),(199,96)]

arrEr2 = [(8,187),(10,187),(12,187),(15,187),(18,190),(21,191),(24,191),(26,191),(78,128),(79,128),(80,128),(82,127),(94,118),(95,115),(96,115),(98,115),(100,115),(115,7),(117,7),(118,8),(119,7),(120,7),(121,7),(127,18),(128,21),(129,21),(131,21),(187,79),(190,94),(191,96),(195,79),(196,82),(198,96),(199,96)]

arrEr3 = [(8,187),(10,187),(12,187),(15,187),(18,190),(21,191),(24,191),(26,191),(78,128),(79,128),(80,128),(82,127),(94,118),(95,115),(96,115),(98,115),(100,115),(115,7),(117,7),(118,8),(119,7),(120,7),(121,7),(127,18),(128,21),(129,21),(131,21),(187,79),(190,94),(191,96),(195,79),(196,82),(198,96),(199,96)]

smc3 :: Set Int
smc3 = S.fromList [7,8,18,21,79,82,94,96,115,118,127,128,187,190,191,196]
