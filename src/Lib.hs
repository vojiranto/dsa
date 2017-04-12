{-#Language MultiWayIf, LambdaCase, TupleSections#-}
module Lib (
    Imagination(..),
    Point(..),
    someFunc,
    formLexTree
  ) where

import System.Clock
import Control.Monad ( unless, forM, void, forM_)
import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import Graphics.UI.GLUT hiding ( initialize, shift)
import Graphics.Rendering.OpenGL.GL.Tensor
import Data.IORef
import Data.Char
import Data.List

import Stratification
import SymbolicImage
import GrBuild
import Point
import Morse
import Shift
import Data
import Quar
import Jac
import Gr

someFunc :: IO ()
someFunc = grInit $ \win -> do
    -- глобальная переменная для хранения состояния программы.
    st  <- newIORef $ createSt

    displayCallback $= Gr.display st
    reshapeCallback $= Just reshape
    keyboardMouseCallback $= Just (keyboardMouseHandler st)

    forM_ [(0, ok1k), (40, ok2k)] $ \(i, call) ->
        createButton win (Position 0 i) (Size 100 30) (call st)


createButton win position size callback = do
    ok <- createSubWindow win position size
    displayCallback       $= showGround white
    keyboardMouseCallback $= Just callback
    return ok


showGround color = do
    clearColor $= color
    clear [ColorBuffer]
    swapBuffers

instance Num a => White (Color4 a) where
    white = Color4 1 1 1 1

-----------------------------------------------------------------
-- [1] Расслоение.
-- [2] Поиск min и max.

