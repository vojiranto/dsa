{-#Language TypeFamilies#-}
module Morse where

import Stratification as S hiding (shiftCeil)
import Data.Matrix (Matrix(..))
import Control.Monad
import Control.Arrow
import SymbolicImage (
    fromRad, toRad, fromCeil, toCeil, myNub, mySequence, (#=))
import Jac (jac, e)
import Data.Array as A
import Graph
import Data.Function
import Data.List as L
import Data.Map as M
import Data.IntMap as IM
import Data.Complex
import Data
import Point
import Data.Graph


morse :: F Point -> Imagination -> Int -> [(Double, Double, Int)]
morse fp im i = mrsElem <$> cyclics
    ((shiftCeil d1 d2 fp #=) <$> ls)
  where
    mrsElem :: [(Ceil3, [(Ceil3, Double)])] -> (Double, Double, Int)
    mrsElem a = (
                         minOptZ gr  (minBazeCircuit gr),
                negate $ minOptZ gr' (maxBazeCircuit gr'),
                length a
--                       maximum $ plat a,
--                       minimum $ plat a
            )
        where
            gr  = formGraph                    a
            gr' = formGraph $ modifyIns negate a

    (Stratification d1 d2 ls) = str fp im i

plat :: [(Ceil3, [(Ceil3, Double)])] -> [Double]
plat = concatMap (\x -> snd <$> snd x)

-- XXX Дзен
modifyIns :: (a -> b) -> [(c, [(d, a)])] -> [(c, [(d, b)])]
modifyIns f l = second (second f <$>) <$> l



-- выделяем области связности
--cyclics :: (Ord t0, Ord k0) => [(k0, t0, [(t0, b0)])] -> [[(k0, [(t0, b0)])]]
cyclics g = M.toList <$> do
    let g' = (\(a, b, l) -> (a, b, L.filter (\x -> fst x /= a) l)) <$> g
    CyclicSCC a <- stronglyConnComp
        [(a, b, fst <$> c) | (a, b, c) <- g']
    return $ elect a $ M.fromList [(a, b) | (a, _, b) <- g']



shiftCeil :: Diameter -> Diameter -> F Point -> Ceil3 -> [(Ceil3, Double)]
shiftCeil d1 d2 fp (Ceil3 c l) = myNub $ do
    let Point x y = fromCeil d1 c
    ret <$> mySequence x d1
        <*> mySequence y d1
        <*> mySequence (rad d2 l) (pi/2/d2)
  where
    ret x' y' t' = (
        func d1 d2 fp x' y' t',
        log $ x $ abs $ toPoint $ jacobian fp x' y' * e t')


elect :: Ord a => [a] -> F (Map a b)
elect a m = M.intersection m $ M.fromList $ (\a -> (a,a)) <$> a
