{-#Language LambdaCase, BangPatterns#-}
module Graph where

import Data.Graph
import Control.Parallel.Strategies
import Shift
import Data
import qualified Data.Set as Set

-- Строим бесконечную последовательность приблежений.
formImagination :: F Point -> Space -> [(Imagination, Int)]
formImagination f s = iterate
    (stepImagination f)
    (bazeImagination s, 0)


-- Нахождение всех цеклических вершин графа.
cyclics :: Ord key => Graf node key -> [node]
cyclics g = concat [a | CyclicSCC a <- stronglyConnComp g]

-- Построение образа поданных ячеек.
stepImagination :: F Point -> F (Imagination, Int)
stepImagination fp (im, i) =
    (stepImagination' fp im, 4 * size im + i)

{-# INLINE stepImagination' #-}
stepImagination' :: F Point -> F Imagination
stepImagination' fp (Imagination d c) = Imagination (d/2) $!
    cyclics $! parMap rseq
        (shiftCeil 4 (d/2) fp #=)
        (celling d c)
  where
    (#=) !f !c = (c, c, f c)

-- Получение по точке номера ячейки в которой она
-- находится.
{-# INLINE toCeil #-}
toCeil :: Diameter -> Point -> Ceil
toCeil d (Point x y) = formCeil x y d


-- Построение ячейки
{-# INLINE formCeil #-}
formCeil :: Double -> Double -> Diameter -> Ceil
formCeil x y d = Ceil (f x) (f y)
  where f x = ceiling $! x/d

-- Получение по номеру ячейки её левого верхнего угла.
{-# INLINE fromCeil #-}
fromCeil :: Diameter -> Ceil -> Point
fromCeil d (Ceil x y) = Point (f x) (f y)
  where
    {-# INLINE f #-}
    f :: Int -> Double
    f x = d * fromIntegral x

-- По ячейке и уровнениям формируем её образ.
shiftCeil :: Double -> Diameter -> F Point ->
    Ceil -> [Ceil]
shiftCeil k d fp p = myNub $ parMap rseq
    (\p -> toCeil d $ fp p) $
  do
    let Point x y = fromCeil d p
        x' = x-d
        y' = y-d
        d' = d/k
    x' <- [x', x'+d'..x]
    y' <- [y', y'+d'..y]
    return $ Point x' y'



bazeImagination :: Space -> Imagination
bazeImagination s = Imagination 1 $! formBaze s 1

-- Построение базового разбиения.
formBaze :: Space -> Diameter -> [Ceil]
formBaze (Space (Point x1 y1) (Point x2 y2)) d = do
    x <- [x1, x1+d..x2]
    y <- [y1, y1+d..y2]
    return $! formCeil x y d

-- Произведение разбиение сетки ячеек на более мелкую.
celling :: Diameter -> F [Ceil]
celling !d = concatMap $! \c -> do
    let Point x y = fromCeil d c
        x' = x - d
        y' = y - d
        d' = d/2
    x' <- [x', x'+d'..x]
    y' <- [y', y'+d'..y]
    return $! formCeil x' y' d'

-- Накладываю более строгие требования на входные
-- данные. Решаю не за О(n^2), а за O(log n).
{-# INLINE myNub #-}
myNub :: Ord a => F [a]
myNub x = Set.toList . Set.fromList $! x


-- определяем кол-во ячеек.
instance Size Imagination where
    size (Imagination _ x) = length x
