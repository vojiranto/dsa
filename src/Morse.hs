{-#Language TypeFamilies, BangPatterns, MultiWayIf#-}
module Morse where

import Stratification as S hiding (shiftCeil)
import Data.Matrix (Matrix(..))
import SymbolicImage (
    fromRad, toRad, fromCeil, toCeil, myNub)
import Jac (jac, e)
import Graph
import Data.Map as M
import Data
import Point
import Data.Graph

morse :: F Point -> Imagination -> Int -> [(Double, Double)]
morse fp im i = mrsElem <$> cyc
  where
    -- строим раслоение.
    cyc :: [[(Ceil3, [(Ceil3, Double)])]]
    cyc = cyclics g

    mrsElem :: [(Ceil3, [(Ceil3, Double)])] -> (Double, Double)
    mrsElem a = (zOfCircuit $ minBazeCircuit gr, zOfCircuit $ maxBazeCircuit gr)
        where gr = formGraph a

    (Stratification d1 d2 ls) = str fp im i

    (#=) !f !c = (c, c, f c)

    g = (shiftCeil d1 d2 fp #=) <$> ls


-- выделяем области связности
cyclics g = M.toList <$> do
    CyclicSCC a <- stronglyConnComp $
        (\(a, b, c) -> (a, b, fst <$> c)) <$> g
    let g' = M.fromList $! (\(a, _, b) -> (a, b)) <$> g
        a' = M.fromList $! (\a -> (a, undefined)) <$> a
    return $ M.intersection g' a'


shiftCeil :: Diameter -> Diameter -> F Point -> Ceil3 -> [(Ceil3, Double)]
shiftCeil d1 d2 fp (Ceil3 c l) = myNub $!
    (\i -> (Ceil3 p' (tr $! i), log $ frP.abs $ trp i)) <$> list
  where
    p' :: Ceil
    p' = toCeil d1 $ fp ceilCenter

    frP (Point x y) = x

    tr i  = fromRad d2 $! arc $! signum $! trp i
    trp i = toPoint $! jacobian * e i

    list :: [Double]
    list = do
        let t = toRad d2 l
        x <- [t-d2, t-d2+d2/16..t]
        return x
    -- минус d/2 из-за особенностей отображения точек в номера
    -- ячеек.
    -- (fromCeil 1 $ Ceil 1 1)    == Point 1.0 1.0
    -- (toCeil 1 $ Point 0.4 0.4) == Ceil 1 1
    ceilCenter :: Point
    ceilCenter = Point (x-d1/2) (y-d1/2)

    Point x y = fromCeil d1 c

    jacobian :: Matrix Double
    jacobian = jac fp ceilCenter

    arc :: Point -> Double
    arc (Point x y) = if
        | y > 0     -> acos x
        | otherwise -> acos x + pi

