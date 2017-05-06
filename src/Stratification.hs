{-#Language TupleSections, MultiWayIf, BangPatterns#-}
module Stratification where

-- Внешние импорты
import Data.Matrix (Matrix(..))

import Data
import Point
import SymbolicImage
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


toStratification :: Imagination -> Stratification
toStratification (Imagination d c) = Stratification d 1
    $ concatMap (\c -> [Ceil3 c 1, Ceil3 c 0]) c



-- подразбиение на более мелкие уровни
celling :: Ceil3 -> [Ceil3]
celling (Ceil3 c i) = [Ceil3 c (i*2), Ceil3 c (i*2-1)]

-- Тут что-то не то.               FIXME
shiftCeil :: Diameter -> Diameter -> F Point -> Ceil3 -> [Ceil3]
shiftCeil d1 d2 fp (Ceil3 c l) = myNub $ do
    let Point x y = fromCeil d1 c
    pure (func d1 d2 fp)
        <*> (mySequence x d1)
        <*> (mySequence y d1)
        <*> (mySequence (rad d2 l) (pi/2/d2))


func d1 d2 fp x y t = Ceil3 (ceil d1 fp x y) (int d2 (arcOf fp x y t))

arcOf :: F Point -> Double -> Double -> Double -> Double
arcOf fp x y t = arc $ signum $ toPoint $ jacobian fp x y * e t


ceil :: Double -> F Point -> Double -> Double -> Ceil
ceil d fp x y = toCeil d $ fp (Point x y)


jacobian :: F Point -> Double -> Double -> Matrix Double
jacobian fp x y = jac fp $ Point x y


arc :: Point -> Double
arc (Point x y) = if
    | y > 0, x > 0 -> acos x
    | x > 0        -> negate $ acos x
    | y > 0        -> acos $ negate x
    | otherwise    -> negate $ acos $ negate x

int :: Double -> Double -> Int
int d r = ceiling $ r/pi*2*d


rad :: Double -> Int ->Double
rad d r = r'*pi/2/d
  where r' = toEnum r

