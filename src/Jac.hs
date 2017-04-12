{-# LANGUAGE
    MultiWayIf,
    FlexibleInstances,
    NumDecimals #-}

module Jac (
    jac,
    e,
    ) where

import Data.Matrix (Matrix(..), fromLists, fromList, toList)

import Point
import Data (F)

type FPD = Point -> Double


-----------------------------------------------------------------
-- производная.
diff :: (Floating a, Eq a) => F a -> a -> a
diff f x = (f (x - d/2) - f (x + d/2))/d
  where
    d = if
        | x == 0    ->   1e-9
        | otherwise -> x*1e-5


-- якобиан.
jac :: F Point -> Point -> Matrix Double
jac f p@(Point x' y') = fromLists [
    [diff (\a -> f1 p{x = a}) x', diff (\a -> f1 p{y = a}) y'],
    [diff (\a -> f2 p{x = a}) x', diff (\a -> f2 p{y = a}) y']]
  where
    f1 = x.f ; f2 = y.f

-- единичный вектор.
e :: Double -> Matrix Double
e f = fromList 2 1 [cos f, sin f]


-- см. стр. 102. в книге 1.
formA :: F Point -> Point -> Double -> Double
formA f p k = log.abs.toList $ (jac f p) * e k
    where abs [x, y] = sqrt $ x^2 + y^2

-----------------------------------------------------------------
-- преобразование вектора в точку.
instance Points (Matrix Double) where
    toPoint = toPoint.toList
    fromPoint = fromList 2 1.fromPoint

-----------------------------------------------------------------
--                   Справочная литература                     --
-----------------------------------------------------------------
-- 1. Г.С. Осипенко, Н.Б.Ампилова:
--      ВВЕДЕНИЕ В СИМВОЛИЧЕСКИЙ АНАЛИЗ ДИНАМИЧЕСКИХ СИСТЕМ
