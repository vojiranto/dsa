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
jac f (Point x y) = fromLists [
    [diff (\x -> f1 $ Point x y) x, diff (\y -> f1 $ Point x y) y],
    [diff (\x -> f2 $ Point x y) x, diff (\y -> f2 $ Point x y) y]]
  where
    f1 = (\(Point x _) -> x).f
    f2 = (\(Point _ y) -> y).f

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
