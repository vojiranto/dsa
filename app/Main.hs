module Main where

import Lib
import Control.Monad.Fix

main :: IO ()
main = someFunc

{- mfix example code
main = mfix (\f a -> do
    print "hello world"
    if True then return ()
    else return f a
  ) undefined
-}
