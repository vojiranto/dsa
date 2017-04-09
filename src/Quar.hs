{-#Language MultiWayIf, LambdaCase, FlexibleInstances#-}
module Quar ( patch
            , localizate
            , patchStep
            , toChain
            , toPatch
            , distance
            , zonePoint) where

import Data
--import Shift


type Link = (Point, Point, Point, Point)
-------------------------------------------------------
--                    Функции                        --
-------------------------------------------------------
--  Построение линии неустойчивости
patch :: F Point -> [Point] -> IterateNumber -> [Point]
patch f p n = iterate
    (patchStep f) (localizate f 2 p) !! n


-- Локализация отрезка в области
localizate :: F Point -> Int -> F [Point]
localizate f i p = iterate
    (filter zonePoint.patchStep f) p !! i


--                                                   --
--           Преобразование пути в путь              --
--                                                   --
patchStep :: F Point -> [Point] -> [Point]
patchStep f (x:xs) = f x : concatMap
    (toPatch f) (toChain f (f x) (x:xs))


--  Преобразование списка точек в список звеньев
toChain :: F Point -> Point -> [Point] -> [Link]
toChain f x' = \case
    --  завершение обработки
    [x, y]   -> [link x y]
    --  шаг итерации.
    x:y:tail ->  link x y : toChain f (f y) (y:tail)
  where
    {-#INLINE link#-}
    link :: Point -> Point -> Link
    link x y = (x', f y, x, y)

--                                                   --
--          Преобразование звена в ломанную          --
--                                                   --
toPatch :: F Point -> Link -> [Point]
toPatch f p@(x', y', x, y) = f . point <$> [1..k]
  where
    --  Число отрезков, на которое подразбиваем звено
    k :: Int
    k = ceiling (distance x' y'/ 0.001) - 1

    --  Точка O на отрезке AB
    point :: Int -> Point
    point i = Point (f x1 x2) (f y1 y2)
      where
        {-#INLINE f#-}
        f :: Double -> Double -> Double
        f a b = alfa * a + (1 - alfa) * b

        --  Расстояние в процентах от точки A до
        --  точки O
        alfa :: Double
        alfa = toEnum i/toEnum k

        --  Раскладываем точки на координаты.
        Point x1 y1 = x; Point x2 y2 = y

-------------------------------------------------------
--            Вспомогательные функции                --
-------------------------------------------------------
--  Расстояние между точками
distance :: Point -> Point -> Double
distance (Point x1 y1) (Point x2 y2) = sqrt $
    (x1 - x2)^2 + (y1 - y2)^2


--  Точка в окрестностях нуля.
zonePoint :: Point -> Bool
zonePoint x = distance (Point 0 0) x < 1

instance Size [Point] where
    size = length

instance Size Patch where
    size (Patch a) = size a
