{-#Language ScopedTypeVariables, BangPatterns, MultiWayIf#-}
import Graphics.UI.Gtk hiding (Point)
import Graphics.UI.Gtk.Gdk.GC
import Graphics.UI.Gtk hiding (Color, Point, Object)
import Shift
import Control.Monad
import Data.Maybe
import Control.Exception
import Data.IORef as R
import SymbolicImage
import Data
import Point

defaultFgColor :: Color
defaultFgColor = Color 65535 65535 65535

defaultBgColor :: Color
defaultBgColor = Color 0 0 0

-- Необходимо хранить.
    -- Края: xMin, xMax, yMin, yMax -- Ok.
    -- Образ

renderScene d lims p ev = do
    dw           <- widgetGetDrawWindow d
    (xMax, yMax) <- readIORef lims
    Imagination d' p <- readIORef p

    (w, h) <- widgetGetSize d
    gc     <- gcNew dw
    let fg = color 0 0 0
    gcSetValues gc $ newGCValues { foreground = fg }
    let wM = round (xMax * 2)
        hM = round (yMax * 2)
        tr (x, y) = (
            round (x *        toEnum w) `div` wM + w`div`2,
            round (y * (-1) * toEnum h) `div` hM + h`div`2)
        drawLine' x y = drawLine dw gc (tr x) (tr y)

    -- Оси OX и OY
    drawLine' (0, -yMax) (0, yMax)
    drawLine' (-xMax, 0) (xMax, 0)

    -- Насечки (мелкие)
    forM_ [-xMax, -xMax+0.1..xMax] $ \i -> do
        drawLine' (i, 0.01) (i, -0.01)
    forM_ [-yMax, -yMax+0.1..yMax] $ \i -> do
        drawLine' (0.01, i) (-0.01, i)

    -- насечки крупные
    forM_ [-xMax..xMax] $ \i -> do
        drawLine' (i, 0.1) (i, -0.1)

    forM_ [-yMax..yMax] $ \i -> do
        drawLine' (0.1, i) (-0.1, i)
    let pTr (Point x y) = tr (x, y)
    drawPoints dw gc $ pTr.fromCeil d' <$> p
    return True

main :: IO ()
main = do
    initGUI
    window  <- windowNew
    hBox    <- hBoxNew False 4
    vBox    <- vBoxNew False 4
    hl@[h1, h2, h3,hx,hy, h4, h5,her] <- forM [1..8] $
        const (hBoxNew False 4)
    [   ar1, ar2, ar3,
        arX, arY] <- forM [1..5] (const entryNew)
    drawing <- drawingAreaNew

    set window [
        windowDefaultWidth := 900,
        windowDefaultHeight := 600,
        containerChild := hBox,
        windowTitle := "DSA – Dinamic Sistem Analiser"]

    let setСontainers a b = do
            set a $ (containerChild :=) <$> b


    -- TODO проблема несовпадения типов vBox и drawing
    setСontainers hBox [vBox]
    setСontainers hBox [drawing]

    l0 <- labelNewWithMnemonic "Введите формулы"
    setСontainers vBox [l0]
    boxSetChildPacking vBox l0 PackNatural 4 PackStart

    setСontainers vBox hl

    let extr hBox ar txt1 txt2 = do
            lExt <- labelNewWithMnemonic txt1
            setСontainers hBox [lExt]
            setСontainers hBox [ar]
            entrySetText  ar txt2

    extr h1 ar1 "x' = " "1 + y - 1.4 * pot(x, 2)"
    extr h2 ar2 "y' = " "0.3*x"


    extr hx arX "x max = "  "1.5"
    extr hy arY "y max = "  "1"
    extr h3 ar3 "Число итераций = " "5"

    let buttons h txt = forM txt $ \txt -> do
             b <- buttonNewWithLabel txt
             setСontainers h [b]
             return b

    [b1, b2] <- buttons h4 ["Образ", "Дополнительная итерация"]
    [b3] <- buttons h5 ["Спектр морса"]

    -- Настраиваем упаковку аплетов
    boxSetChildPacking hBox vBox PackNatural 4 PackStart
    boxSetChildPacking hBox drawing PackGrow 4 PackStart

    forM_ hl $ \a ->
        boxSetChildPacking vBox a PackNatural 4 PackStart

    let bg = color 1 1 1
    widgetModifyBg drawing StateNormal bg

    r <- newIORef (1.5, 1)
    i <- newIORef $ Imagination 1 []
    onExpose drawing (renderScene drawing r i)

    -- Нажимаем на кнопку "Образ"
    onClicked b1 $ do
        [x',y',xMax, yMax, nInt] <- mapM entryGetText
            [ar1, ar2, arX, arY, ar3]

        -- TODO Нужна обработка ошибок чтения.
        xMax <- catch
            (pure $! Just $! (read xMax :: Double))
            (\(_ :: SomeException) ->
                pure Nothing :: IO (Maybe Double))
        yMax <- catch
            (pure $! Just $! (read yMax :: Double))
            (\(_ :: SomeException) ->
                pure Nothing :: IO (Maybe Double))
        if isJust xMax && isJust yMax then do
            -- разворачиваем
            Just xMax <- pure xMax
            Just yMax <- pure yMax

            writeIORef r (xMax, yMax)
            widgetDestroy drawing

            let f p        = Point (shift x' p) (shift y' p)
                sp         = Space
                    (Point (-1 * xMax) (-1 * yMax))
                    (Point       xMax        yMax )
                (image, i') = formImagination f sp !! (read nInt)

            writeIORef i image

            setСontainers hBox [drawing]
            void $ onExpose drawing (renderScene drawing r i)
        else if
            | not.isJust $ xMax -> pure ()
            | not.isJust $ yMax -> pure ()
        widgetShowAll window

    onClicked b2 $ do
        [x',y',xMax, yMax, nInt] <- mapM entryGetText
            [ar1, ar2, arX, arY, ar3]
        im <- readIORef i
        let (im', i') = stepImagination f (im, 0)
            f p       = Point (shift x' p) (shift y' p)
        writeIORef i im'
        widgetDestroy drawing
        setСontainers hBox [drawing]
        void $ onExpose drawing (renderScene drawing r i)
        widgetShowAll window

    onDestroy window mainQuit
    windowSetPosition window WinPosCenter
    widgetShowAll window
    mainGUI

color r g b = Color
    (round $ 65535 * r)
    (round $ 65535 * g)
    (round $ 65535 * b)

point :: String -> String -> Point
point x y = Point (read x) (read y)

point' :: String -> String -> Point
point' x y = Point (-1*(read x)) (-1*(read y))


