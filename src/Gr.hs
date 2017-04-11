module Gr where

import Control.Monad ( unless, forM, void, join )
import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import Graphics.UI.GLUT hiding ( initialize )
import Graphics.Rendering.OpenGL.GL.Tensor
import Data.IORef
import System.Clock
import Graph as G
import Shift
import Quar
import qualified Data as D

grInit func = do
    _ <- getArgsAndInitialize
    initialDisplayMode $= [DoubleBuffered, RGBMode, WithDepthBuffer]
    initialWindowSize     $= Size 1024 768
    initialWindowPosition $= Position 100 150
    win <- createWindow "DSA - Dinamic Sistem Analiser"
    func win
    mainLoop

ok1k :: IORef D.St -> KeyboardMouseCallback
ok1k st (MouseButton LeftButton) Down _ _ = do
    fx:fy:x1:y1:x2:y2:_ <- form
    let f p        = D.Point (Gr.shift fx p) (Gr.shift fy p)
        sp         = D.Space (Gr.point x1 y1) (Gr.point x2 y2)
        (image, i) = formImagination f sp !! 0
    print i
    st $=! D.St {
        D.render  = renderIm image,
        D.block   = False,
        D.workLab = D.Lab1 image,
        D.f       = f
      }
ok1k _ _ _ _ _ = return ()

ok2k :: IORef D.St -> KeyboardMouseCallback
ok2k st (MouseButton LeftButton) Down _ _ = do
    fx:fy:x1:y1:x2:y2:_ <- form
    let f p        = D.Point (Gr.shift fx p) (Gr.shift fy p)
        ptch :: [D.Point]
        ptch       = patch f [Gr.point x1 y1, Gr.point x2 y2] 1
        d          = 0.001
    pt <- tempOf . newIORef $ fromCeil d <$>
         myNub (toCeil d <$> ptch)
    st $=! D.St {
        D.render  = do
            ptch <- get pt
            renderPt ptch,
        D.block   = False,
        D.workLab = D.Lab2 ptch,
        D.f       = f
      }
    print $ D.size ptch
ok2k _ _ _ _ _ = return ()

tempOf_ = void.tempOf

tempOf x = do
    t1 <- getTime Realtime
    d <- x
    t2 <- getTime Realtime
    print $ diffTimeSpec t1 t2
    return d

shift :: String -> D.Point -> Double
shift xs p = formEl (formLexTree xs) p

point :: String -> String -> D.Point
point x y = D.Point (read x) (read y)

sp :: D.Space
sp = D.Space (D.Point (-2) (-2)) (D.Point 2 2)

keyboardMouseHandler :: IORef D.St -> KeyboardMouseCallback
keyboardMouseHandler _  (Char 'q') _ _ _ = exitWith ExitSuccess
keyboardMouseHandler st (Char ' ') _ _ _ = do
    st' <- get st
    case D.workLab st' of
        D.Lab1 l -> do
            let fIm = stepImagination' (D.f st') l
            print $ D.size fIm
            st $=! st' {
                D.render = renderIm fIm,
                D.workLab = D.Lab1 fIm
            }
        D.Lab2 pt -> do
            let d    = 0.001
                ptch = patchStep (D.f st') pt
            print $ D.size ptch
            pt <- tempOf . newIORef $ fromCeil d <$>
                myNub (toCeil d <$> ptch)
            st $=! st' {
                 D.render = do
                    ptch <- get pt
                    renderPt ptch,
                 D.workLab = D.Lab2 ptch
                }
        _ -> return ()
keyboardMouseHandler _ _          _ _ _ = postRedisplay Nothing

--------------------------------------------------------------------------------
-- Reset the viewport for window changes.
--------------------------------------------------------------------------------
reshape :: ReshapeCallback
reshape size@(Size width height) =
    unless (height == 0) $ do
        viewport   $= (Position 0 0, size)
        matrixMode $= Projection
        loadIdentity
        perspective 90 (fromIntegral width / fromIntegral height) 1 100
        matrixMode $= Modelview 0

--------------------------------------------------------------------------------
-- Clear and redraw the scene.
--------------------------------------------------------------------------------
display :: IORef D.St -> DisplayCallback
display var = do
    loadIdentity
    lookAt (Vertex3 0 0 1) (Vertex3 0 0 0) (Vector3 0 1 0)
    clear [ColorBuffer, DepthBuffer]
    color3f white >> axis 0 >> axis 90
    tempOf $ join $ D.render <$> get var
    swapBuffers

axis a = rotateObj a (Vector3 0 0 (1.0 :: GLfloat)) $ do
    renderPrimitive Lines $ do
        -- Ось
        vertex3f (Vertex3 (-5) 0 0)
        vertex3f (Vertex3 5 0 0)

        -- стрелка
        vertex3f (Vertex3 4.9 0.05 0)
        vertex3f (Vertex3 5 0 0)
        vertex3f (Vertex3 4.9 (-0.05) 0)
        vertex3f (Vertex3 5 0 0)

    -- Единичные отрезки по оси OX
    -- насечки
    let notches l a = renderPrimitive Lines $ forM l $ \i -> do
            vertex3f (Vertex3 i a        0)
            vertex3f (Vertex3 i (-1 * a) 0)
    notches [-5,-4..5]   0.1
    notches [-5,-4.9..5] 0.02

rotateObj a b x = rotate a b >> x >> rotate (-1*a) b

renderIm :: D.Imagination -> IO [()]
renderIm (D.Imagination d im) = do
    renderPrimitive Points $ forM im $ \(D.Ceil x y) -> do
        color3f (Color3 1 1 0)
        vertex3f (Vertex3
            (realToFrac x * realToFrac d)
            (realToFrac y * realToFrac d)
            0)

renderPt :: [D.Point] -> IO [()]
renderPt p = do
    renderPrimitive Points $ forM p $ \(D.Point x y) -> do
        color3f (Color3 1 1 0)
        vertex3f (Vertex3 (realToFrac x) (realToFrac y) 0)

instance Num a => White (Color3 a) where
    white = Color3 1 1 1

color3f = color :: Color3 GLfloat -> IO ()
vertex3f = vertex :: Vertex3 GLfloat -> IO ()

form :: IO [String]
form = do
    txt <- readFile "form.txt"
    return $ lines txt

class White a where
  white :: a

-----------------------------------------------------------------
--                          Текстуры                           --
-----------------------------------------------------------------

