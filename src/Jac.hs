{-# LANGUAGE
    MultiWayIf,
    FlexibleInstances #-}

module Jac where

import Data
import Data.Matrix


type FPD = Point -> Double


-----------------------------------------------------------------
-- производная.
diff :: (Floating a, Eq a) => F a -> a -> a
diff f x = (f (x - d/2) - f (x + d/2))/d
  where
    d = if
        | x == 0    -> 1/10^9
        | otherwise -> x/10^5


-- якобиан.
jac :: FPD -> FPD -> Point -> Matrix Double
jac f1 f2 (Point x y) = fromLists [
    [diff (\x -> f1 $ Point x y) x, diff (\y -> f1 $ Point x y) y],
    [diff (\x -> f2 $ Point x y) x, diff (\y -> f2 $ Point x y) y]]


-- единичный вектор.
e :: Double -> Matrix Double
e f = fromList 2 1 [cos f, sin f]


-- см. стр. 102. в книге 1.
formA :: FPD -> FPD -> Point -> Double -> Double
formA f1 f2 p f = log.abs.toList $ (jac f1 f2 p) * e f
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
