{-#Language TupleSections#-}
module Stratification where

-- Внешние импорты
import Data.Matrix

import Data
import Graph


toStratification :: Imagination -> Stratification
toStratification (Imagination d c) = Stratification
    d 2 ((,[1,2]) <$> c)


-- подразбиение на более мелкие уровни
celling :: [Int] -> [Int]
celling = concatMap $! \c -> [c*2, c*2-1]


shiftCeil :: Diameter -> Diameter -> F Point -> (Ceil,[Int])
    -> [(Ceil, [Int])]
shiftCeil d1 d2 fp  = undefined

