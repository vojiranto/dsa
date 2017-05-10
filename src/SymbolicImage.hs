{-#Language LambdaCase, BangPatterns, TypeFamilies#-}
module SymbolicImage (
    formImagination,
    stepImagination,
    stepImagination',
    cyclics,
    fromRad,
    toRad,
    myNub,
    fromCeil,
    toCeil,
    mySequence,
    (#=)
    ) where

import Data.Graph
import Control.Parallel.Strategies
import qualified Data.Set as Set

import Shift
import Data
import Point


-- Строим бесконечную последовательность приблежений.
formImagination :: F Point -> Space -> [(Imagination, Int)]
formImagination f s = iterate
    (stepImagination f)
    (bazeImagination s, 0)


-- Нахождение всех цеклических вершин графа.
cyclics :: (Ord key, node ~ key) => Graf node key -> [node]
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


(#=) :: (a -> b) -> a -> (a, a, b)
(#=) !f !c = (c, c, f c)


-- Получение по точке номера ячейки в которой она
-- находится.
{-# INLINE toCeil #-}
toCeil :: Diameter -> Point -> Ceil
toCeil d (Point x y) = formCeil x y d


-- Построение ячейки
{-# INLINE formCeil #-}
formCeil :: Double -> Double -> Diameter -> Ceil
formCeil x y d = Ceil (fromRad d x) (fromRad d y)


-- Получение по номеру ячейки её левого верхнего угла.
{-# INLINE fromCeil #-}
fromCeil :: Diameter -> Ceil -> Point
fromCeil d (Ceil x y) = Point (toRad d x) (toRad d y)


{-# INLINE fromRad #-}
fromRad :: Diameter -> Double -> Int
fromRad d x = ceiling $! x/ d


{-# INLINE toRad #-}
toRad :: Diameter -> Int -> Double
toRad d x = d * fromIntegral x


-- По ячейке и уровнениям формируем её образ.
shiftCeil :: Double -> Diameter -> F Point -> Ceil -> [Ceil]
shiftCeil k d fp p = myNub $ parMap rseq (toCeil d.fp) $ do
    let Point x y = fromCeil d p
    Point <$> mySequence x d <*> mySequence y d


bazeImagination :: Space -> Imagination
bazeImagination s = Imagination 1 $! formBaze s


-- Построение базового разбиения.
formBaze :: Space -> [Ceil]
formBaze (Space (Point x1 y1) (Point x2 y2)) =
    formCeil <$> [x1..x2] <*> [y1..y2] <*> [1]

-- Произведение разбиение сетки ячеек на более мелкую.
celling :: Diameter -> F [Ceil]
celling !d = concatMap $! \c -> do
    let Point x y = fromCeil d c
    formCeil <$> [x-d, x-d/2, x] <*> [y-d, y-d/2, y] <*> [d/2]


-- Накладываю более строгие требования на входные
-- данные. Решаю не за О(n^2), а за O(log n).
{-# INLINE myNub #-}
myNub :: Ord a => F [a]
myNub x = Set.toList . Set.fromList $! x


mySequence :: Double -> Double -> [Double]
mySequence t d = [t - d*x | x <- [-0.25, 0..1.25]]


-- определяем кол-во ячеек.
instance Size Imagination where
    size (Imagination _ x) = length x
