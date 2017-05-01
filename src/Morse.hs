{-#Language TypeFamilies, BangPatterns, MultiWayIf#-}
module Morse where

import Stratification as S hiding (shiftCeil)
import Data.Matrix (Matrix(..))
import Control.Monad
import SymbolicImage (
    fromRad, toRad, fromCeil, toCeil, myNub)
import Jac (jac, e)
import Data.Array as A
import Graph
import Data.Map as M
import Data
import Point
import Data.Graph

morse :: F Point -> Imagination -> Int -> [Int]
morse fp im i = mrsElem <$> cyc
  where
    -- строим раслоение.
    cyc :: [[(Ceil3, [(Ceil3, Double)])]]
    cyc = cyclics g

    mrsElem :: [(Ceil3, [(Ceil3, Double)])] -> Int
    mrsElem a = (length $ minBazeCircuit gr)
        where gr = formGraph a

    (Stratification d1 d2 ls) = str fp im i

    (#=) !f !c = (c, c, f c)

    g = (shiftCeil d1 d2 fp #=) <$> ls


-- выделяем области связности
cyclics g = M.toList <$> do
    CyclicSCC a <- stronglyConnComp $
        (\(a, b, c) -> (a, b, fst <$> c)) <$> g
    -- чистим изолированные точки.
    guard $ length a > 1
    let g' = M.fromList $! (\(a, _, b) -> (a, b)) <$> g
        a' = M.fromList $! (\a -> (a, undefined)) <$> a
    return $ M.intersection g' a'


shiftCeil :: Diameter -> Diameter -> F Point -> Ceil3 -> [(Ceil3, Double)]
shiftCeil d1 d2 fp (Ceil3 c l) = myNub $ do
    let t = toRad d2 l
        Point x1 y1 = fromCeil d1 c
        x' = x1 - d1
        y' = y1 - d1
        d1' = d1/4
    t' <- [t-d2, t-d2+d2/16..t]
    x' <- [x', x'+d1'..x1]
    y' <- [y', y'+d1'..y1]
    let p = Point x' y'
        pc = toPoint $ jacobian p * e t'
        cs = log $ x $ abs pc
    return $ (Ceil3 (toCeil d1 $ fp p)
        (fromRad d2 $ arc $ signum pc), cs)
  where
    jacobian :: Point -> Matrix Double
    jacobian p = jac fp p

    arc :: Point -> Double
    arc (Point x y) = if
        | y > 0     -> acos x
        | otherwise -> acos x + pi

