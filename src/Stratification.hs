{-#Language TupleSections, MultiWayIf#-}
module Stratification where

-- Внешние импорты
import Data.Matrix

import Data
import Graph
import Jac

toStratification :: Imagination -> Stratification
toStratification (Imagination d c) = Stratification
    d 2 (concatMap (\c -> [Ceil3 c 1, Ceil3 c 2]) c)

-- подразбиение на более мелкие уровни
celling :: Ceil3 -> [Ceil3]
celling (Ceil3 c i) = [Ceil3 c (i*2), Ceil3 c (i*2-1)]


shiftCeil :: Diameter -> Diameter -> F Point -> Ceil3 -> [Ceil3]
shiftCeil d1 d2 fp (Ceil3 c l) = Ceil3 p' . (\i ->
    fromRad d2 $ arc $ signum $ toPoint $
    jacobian * (e $ toRad d2 i)) <$> ls
  where
    p' = toCeil d1 $ fp ceilCenter

    -- номера фрагментов окружности.
    ls = [l*2, l*2-1]

    -- минус d/2 из-за особенностей отображения точек в номера
    -- ячеек.
    -- (fromCeil 1 $ Ceil 1 1)    == Point 1.0 1.0
    -- (toCeil 1 $ Point 0.4 0.4) == Ceil 1 1
    ceilCenter = Point (x-d1/2) (y-d1/2)

    Point x y = fromCeil d1 c

    jacobian = jac fp ceilCenter

    arc (Point x y) = if
        | y > 0     -> acos x
        | otherwise -> acos x + pi
