import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.GC
import Graphics.UI.Gtk hiding (Color, Point, Object)
import Control.Monad
import Data.IORef
import Point

defaultFgColor :: Color
defaultFgColor = Color 65535 65535 65535

defaultBgColor :: Color
defaultBgColor = Color 0 0 0

renderScene d p ev = do
    dw     <- widgetGetDrawWindow d
    (w, h) <- widgetGetSize d
    gc     <- gcNew dw
    let fg = color 0 0 0
    gcSetValues gc $ newGCValues { foreground = fg }
    let tr (x, y) = (
            round (x *        toEnum w) `div` 6 + w`div`2,
            round (y * (-1) * toEnum h) `div` 6 + h`div`2)
        drawLine' x y = drawLine dw gc (tr x) (tr y)

    -- Оси OX и OY
    drawLine' (0, -3) (0, 3)
    drawLine' (-3, 0) (3, 0)

    -- Насечки (мелкие)
    forM_ [-2.9, -2.8..3] $ \i -> do
        drawLine' (i, 0.01) (i, -0.01)
        drawLine' (0.01, i) (-0.01, i)

    -- насечки крупные
    forM_ [-3..3] $ \i -> do
        drawLine' (i, 0.1) (i, -0.1)
        drawLine' (0.1, i) (-0.1, i)

    drawPoints dw gc $ (\(Point x y) -> tr (x, y)) <$> p
    return True

main :: IO ()
main = do
    initGUI
    window  <- windowNew
    hBox    <- hBoxNew False 4
    vBox    <- vBoxNew False 4
    hl@[h1, h2, h3,hx1,hx2,hy1, hy2, h4, h5] <- forM [1..9] $
        const (hBoxNew False 4)
    [   ar1, ar2, ar3,
        arXmin, arXmax, arYmin, arYmax] <- forM [1..7] (const entryNew)
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

    extr hx1 arXmin "x min = " "-1.5"
    extr hx2 arXmax "x max = "  "1.5"
    extr hy1 arYmin "y min = " "-1"
    extr hy2 arYmax "y max = "  "1"
    extr h3 ar3 "Число итераций = " "5"

    [b1, b2] <- forM ["Образ", "Дополнительная итерация"]
        buttonNewWithLabel

    setСontainers h4 [b1, b2]

    b3 <- buttonNewWithLabel "Спектр морса"
    setСontainers h5 [b3]

    -- Настраиваем упаковку аплетов
    boxSetChildPacking hBox vBox PackNatural 4 PackStart
    boxSetChildPacking hBox drawing PackGrow 4 PackStart

    forM_ hl $ \a ->
        boxSetChildPacking vBox a PackNatural 4 PackStart

    let bg = color 1 1 1
    widgetModifyBg drawing StateNormal bg
    onExpose drawing (renderScene drawing [])

    onDestroy window mainQuit
    windowSetPosition window WinPosCenter
    widgetShowAll window
    mainGUI

color r g b = Color
    (round $ 65535 * r)
    (round $ 65535 * g)
    (round $ 65535 * b)

{-
import Graphics.UI.Gtk
--import System.Glib.UTFString

main :: IO ()
main = do
  initGUI
  window <- windowNew
  hb     <- hBoxNew False 4
  entry  <- entryNew
  set entry [entryText := "entry"]
  drawingArea <- drawingAreaNew
  button <- buttonNewWithLabel "Reset"
  set hb [
    containerChild := button,
    containerChild := entry,
    containerChild := drawingArea]
  set window [windowTitle := "Hello",
              windowDefaultWidth := 800,
              windowDefaultHeight := 800,
              containerChild := hb]
  set button [containerBorderWidth := 10]
  onClicked button $ do
    txt <- entryGetText entry
    buttonSetLabel button (txt :: String)
  onDestroy window mainQuit
  widgetShowAll window
  mainGUI

-}
