{-#Language TupleSections, MultiWayIf#-}
module Stratification where

-- Внешние импорты
import Data.Matrix

import Data
import Graph
import Jac

toStratification :: Imagination -> Stratification
toStratification (Imagination d c) = Stratification
    d 2 ((,[1,2]) <$> c)


-- подразбиение на более мелкие уровни
celling :: [Int] -> [Int]
celling = concatMap $! \c -> [c*2, c*2-1]


shiftCeil :: Diameter -> Diameter -> F Point -> F (Ceil,[Int])
shiftCeil d1 d2 fp (c, l) = (toCeil d1 $ fp ceilCenter,
    map (\i ->
        fromRad d2 $ arc $ signum $ toPoint $
            jacobian * (e $ toRad d2 i)) l)
  where
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
