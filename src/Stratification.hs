{-#Language TupleSections, MultiWayIf#-}
module Stratification where

-- Внешние импорты
import Data.Matrix (Matrix(..))

import Data
import Point
import Data.Array as A
import Data.Map as M
import Data.Complex
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
  where

{-
  where
    -- считаем граф
    !gr     = (shiftCeil d1 d2 fp ##=) <$> ls
    -- индех вершин
    !index  = formArray gr
    -- обратный индех
    !indexR = formMap index

    unpack !gr = (index A.!) <$> gr
    pack !gr = do
        (a, b) <- gr
        let a' = fromJust $ a`M.lookup`indexR
        return (a', a', MB.mapMaybe (`M.lookup`indexR) b)

(##=) !f !c = (c, f c)

-}

-- вспомогательные структуры данных
-- массив с ячейками п индексам.
formArray :: [(Ceil3, [Ceil3])] -> Array Int Ceil3
formArray gr = array (1, length a) (zip [1..] a)
  where a = plat gr

-- находим индекс по ячейке.
formMap :: Array Int Ceil3 -> Map Ceil3 Int
formMap  arr = M.fromList $ (\(a,b) -> (b,a)) <$> A.assocs arr


plat :: Ord c => [(c, [c])] -> [c]
plat a = myNub.concat $ (\(a, b) -> a:b) <$> a


toStratification :: Imagination -> Stratification
toStratification (Imagination d c) = Stratification d 1
    $ concatMap (\c -> [Ceil3 c 1, Ceil3 c 0]) c


-- подразбиение на более мелкие уровни
celling :: Ceil3 -> [Ceil3]
celling (Ceil3 c i) = [Ceil3 c (i*2), Ceil3 c (i*2-1)]

-- Тут что-то не то.
shiftCeil :: Diameter -> Diameter -> F Point -> Ceil3 -> [Ceil3]
shiftCeil d1 d2 fp (Ceil3 c l) = myNub $ do
    let Point x y = fromCeil d1 c
    func d1 d2 fp
        <$> mySequence x d1
        <*> mySequence y d1
        <*> mySequence (rad d2 l) (pi/2/d2)


func :: Double -> Double -> F Point -> Double -> Double -> Double -> Ceil3
func d1 d2 fp x y t = Ceil3 (ceil d1 fp x y) (int d2 (arcOf fp x y t))


arcOf :: F Point -> Double -> Double -> Double -> Double
arcOf fp x y t = arc $ toPoint $ jacobian fp x y * e t


ceil :: Double -> F Point -> Double -> Double -> Ceil
ceil d fp x y = toCeil d $ fp (Point x y)


jacobian :: F Point -> Double -> Double -> Matrix Double
jacobian fp x y = jac fp $ Point x y


arc :: Point -> Double
arc (Point x y) = if
    | x >  0    ->       phase $          x :+ y
    | x == 0    -> abs $ phase $          x :+ y
    | otherwise ->       phase $ negate $ x :+ y


int :: Double -> Double -> Int
int d r = ceiling $ r/pi*2*d


rad :: Double -> Int ->Double
rad d r = toEnum r * pi/2/d
