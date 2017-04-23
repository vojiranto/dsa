import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.GC
import Graphics.UI.Gtk hiding (Color, Point, Object)

defaultFgColor :: Color
defaultFgColor = Color 65535 65535 65535

defaultBgColor :: Color
defaultBgColor = Color 0 0 0

renderScene d ev = do
    dw     <- widgetGetDrawWindow d
    (w, h) <- widgetGetSize d
    gc     <- gcNew dw
    let fg = color 0 0 0
    gcSetValues gc $ newGCValues { foreground = fg }
    drawPoint dw gc (220, 220)
    drawPoint dw gc (22, 22)
    drawRectangle dw gc True 0 0 10 10
    return True

main :: IO ()
main = do
    initGUI
    window  <- windowNew
    drawing <- drawingAreaNew
    windowSetTitle window "Cells"
    containerAdd window drawing
    let bg = color 1 1 1
    widgetModifyBg drawing StateNormal bg
    onExpose drawing (renderScene drawing)

    onDestroy window mainQuit
    windowSetDefaultSize window 800 600
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
