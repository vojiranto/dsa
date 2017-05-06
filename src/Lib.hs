{-# Language
    GADTs,
    ScopedTypeVariables,
    BangPatterns,
    MultiWayIf#-}
module Lib (someFunc) where

import Graphics.UI.Gtk hiding (Point)
import Graphics.UI.Gtk.Gdk.GC
import Graphics.UI.Gtk hiding (Color, Point, Object)
import Shift
import Control.Monad
import Data.Maybe
import Control.Exception
import Data.IORef as R
import SymbolicImage
import Morse
import Data
import Point

defaultFgColor :: Color
defaultFgColor = Color 65535 65535 65535

defaultBgColor :: Color
defaultBgColor = Color 0 0 0

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

someFunc :: IO ()
someFunc = do
    initGUI
    window  <- windowNew
    hBox    <- hBoxNew False 4
    vBox    <- vBoxNew False 4
    hl@[h0, h1, h2, h3,hx,hy, h4, h5, hs1, hs2,her] <- forM [1..11] $
        const (hBoxNew False 4)
    [   ar1, ar2, ar3,
        arX, arY] <- forM [1..5] (const entryNew)
    drawing <- drawingAreaNew

    set window [
        windowDefaultWidth  := 900,
        windowDefaultHeight := 600,
        containerChild      := hBox,
        windowTitle         := "DSA – Dinamic Sistem Analiser"]

    -- TODO проблема несовпадения типов vBox и drawing
    setСontainers hBox [vBox]
    setСontainers hBox [drawing]

    setСontainers vBox hl
    setLabelNew h0 "Введите формулы"
    extr h1 ar1 "x' = " "1 + 0.3*y - 1.4 * pot(x, 2)"
    extr h2 ar2 "y' = " "x"


    extr hx arX "x max = "          "2"
    extr hy arY "y max = "          "2"
    extr h3 ar3 "Число итераций = " "7"

    [b1, b2] <- buttons h4 ["Образ", "Дополнительная итерация"]
    [b3]     <- buttons h5 ["Спектр Морса"]

    -- поля для вывода крайних значний спектра Морса.
    let stList = ["min: -- ", "max: -- "]
    [sp1Min, sp1Max] <- forM stList (setLabelNew hs1)
    [sp2Min, sp2Max] <- forM stList (setLabelNew hs2)

    -- Настраиваем упаковку аплетов
    boxSetChildPacking hBox vBox PackNatural 4 PackStart
    boxSetChildPacking hBox drawing PackGrow 4 PackStart

    forM_ hl $ \a ->
        boxSetChildPacking vBox a PackNatural 4 PackStart

    let bg = color 1 1 1
    widgetModifyBg drawing StateNormal bg

    -- Переиенные.
    r <- newIORef (1.5, 1)
    i <- newIORef $ Imagination 1 []

    onExpose drawing (renderScene drawing r i)

    -- Нажимаем на кнопку "Образ"
    onClicked b1 $ do
        [x',y',xMax, yMax, nInt] <- mapM entryGetText
            [ar1, ar2, arX, arY, ar3]

        -- TODO Переработать обнаружение этой ошибки.
        !t <- cathF (\p -> Point (shift x' p) (shift y' p)) $ Point 0 0
        -- XXX Если использовать не список, и выставить аргументы
        --     в ином порядке, то перестанет ловить ошибки.
        l@[nInt, xMax, yMax] <- mapM catchRead [nInt, xMax, yMax]
        if isJust t && all isJust l then do
            -- разворачиваем
            [Just nInt, Just xMax, Just yMax] <- pure l

            writeIORef r (xMax, yMax)
            widgetDestroy drawing

            let f p        = Point (shift x' p) (shift y' p)
                sp         = Space
                    (Point (-1 * xMax) (-1 * yMax))
                    (Point       xMax        yMax )
                (image, i') = formImagination f sp !! (fromEnum nInt)

            writeIORef i image

            setСontainers hBox [drawing]
            void $ onExpose drawing (renderScene drawing r i)
        else if
            -- TODO настроить выдачу сообщений о ошибках
            | not.isJust $ xMax -> pure ()
            | not.isJust $ yMax -> pure ()
            | not.isJust $ nInt -> pure ()
            | not.isJust $ t    -> pure ()
        widgetShowAll window

    onClicked b2 $ do
        [x',y',xMax, yMax, nInt] <- mapM entryGetText
            [ar1, ar2, arX, arY, ar3]
        im <- readIORef i

        -- TODO Переработать обнаружение этой ошибки.
        !t <- cathF (\p -> Point (shift x' p) (shift y' p)) $ Point 0 0
        when (isJust t) $ do
            let (im', i') = stepImagination f (im, 0)
                f p       = Point (shift x' p) (shift y' p)
            writeIORef i im'
            widgetDestroy drawing
            setСontainers hBox [drawing]
            void $ onExpose drawing (renderScene drawing r i)
        widgetShowAll window

    onClicked b3 $ do
        [x',y',xMax, yMax, nInt] <- mapM entryGetText
            [ar1, ar2, arX, arY, ar3]
        im <- readIORef i

        -- TODO Переработать обнаружение этой ошибки.
        !t <- cathF (\p -> Point (shift x' p) (shift y' p)) $ Point 0 0
        when (isJust t) $ do
            let f p       = Point (shift x' p) (shift y' p)
            -- вычисление спектра морса
            print $ morse f im 4
            return ()
        widgetShowAll window

    onDestroy window mainQuit
    windowSetPosition window WinPosCenter
    widgetShowAll window
    mainGUI


color r g b = Color
    (round $ 65535 * r)
    (round $ 65535 * g)
    (round $ 65535 * b)


-- Элементы  интерфейса.
setСontainers a b = do
    set a $ (containerChild :=) <$> b


extr hBox ar txt1 txt2 = do
    setLabelNew hBox txt1
    setСontainers hBox [ar]
    entrySetText  ar txt2


setLabelNew bx lb = do
    l <- labelNewWithMnemonic lb
    setСontainers bx [l]
    return l


buttons h txt = forM txt $ \txt -> do
    b <- buttonNewWithLabel txt
    setСontainers h [b]
    return b


-- Обработчики ошибок
catchRead :: Read a => String -> IO (Maybe a)
catchRead = cathF read


cathF :: (a -> b) -> a -> IO (Maybe b)
cathF f x = catch
    (pure $! Just $! f $! x)
    (\(_ :: SomeException) -> pure Nothing)

