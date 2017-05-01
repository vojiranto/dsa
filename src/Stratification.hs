{-#Language TupleSections, MultiWayIf, BangPatterns#-}
module Stratification where

-- Внешние импорты
import Data.Matrix (Matrix(..))

import Data
import Point
import SymbolicImage (fromRad, toRad, fromCeil, toCeil, cyclics, myNub)
import Jac (jac, e)

str :: F Point -> Imagination -> Int -> Stratification
str fp im i = iterate
    (stepStratification fp)
    (toStratification im) !! i

-- Один шаг с подразбиением.
stepStratification :: F Point-> F Stratification
stepStratification fp (Stratification d1 d2 ls) = Stratification
    d1 (d2*2) $ concatMap celling $ cyclics $
    (shiftCeil d1 d2 fp #=) <$> ls
  where
    (#=) !f !c = (c, c, f c)


toStratification :: Imagination -> Stratification
toStratification (Imagination d c) = Stratification d pi
    (flip Ceil3 1 <$> c)


-- подразбиение на более мелкие уровни
celling :: Ceil3 -> [Ceil3]
celling (Ceil3 c i) = [Ceil3 c (i*2), Ceil3 c (i*2-1)]


shiftCeil :: Diameter -> Diameter -> F Point -> Ceil3 -> [Ceil3]
shiftCeil d1 d2 fp (Ceil3 c l) = myNub $ do
    let t = toRad d2 l
        Point x y = fromCeil d1 c
        x' = x - d1
        y' = y - d1
        d1' = d1/4
    t' <- [t-d2, t-d2+d2/16..t]
    x' <- [x', x'+d1'..x]
    y' <- [y', y'+d1'..y]
    let p = Point x' y'
    return $ Ceil3 (toCeil d1 $ fp p)
        (fromRad d2 $ arc $ signum $ toPoint $ jacobian p * e t')
  where
    jacobian :: Point -> Matrix Double
    jacobian p = jac fp p

    arc :: Point -> Double
    arc (Point x y) = if
        | y > 0     -> acos x
        | otherwise -> acos x + pi
