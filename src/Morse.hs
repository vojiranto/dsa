module Morse where

import Data.List (maximumBy)
import Data.Function (on)

-- находим самое длинное слово.
myMax :: [String] -> String
myMax = maximumBy (compare`on`length)

-- считываем, обрабатываем и печатаем результат.
test = do
    s <- readLn
    putStrLn . myMax . words $ s
