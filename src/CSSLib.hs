{-#Language OverloadedStrings#-}
module CSSLib where

import Data.Text.Lazy
import Control.Monad
import Graphics.UI.Threepenny
import Clay hiding ((#), (#+))

addCSS :: Window -> Css -> UI ()
addCSS w css = void $ getHead w #+
    [mkElement "style"
        # set text (unpack.render $ css)]
