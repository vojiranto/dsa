{-#Language OverloadedStrings, ScopedTypeVariables#-}
module CSS where

import Clay
import Data.Monoid
import Data.Text


css :: Css
css = do
    body ? do
        background backGroundСolor
        borderLine
        Key browsers <> "hyphens" -: "auto"
        height $ vh 97 
        width $ vw 97
    p ? do
        textIndent $ indent $ px 20
        marginTop <> marginBottom $ em 0.0

    -- Тип окраски первой буквы.
    let letter = do
            color           red
            fontSize      $ pct 200
            fontFamily  [] [serif]
            position        relative
            top           $ px 5
    ".letter" ? letter
    "#menu"   ? do
        position $ fixed
        right $ vw 1
        borderLine 
        background $ backGroundСolor -. 20
        width $ vw 17
        height $ vh 97
        firstLetter & letter
    "#mapImage" ? do
        borderLine
        background white
        width $ vw 82
        height $ vh 97

borderLine = border dashed (px 1) borderСolor
backGroundСolor, borderСolor :: Color
backGroundСolor = parse "#FCFCEF"
borderСolor     = parse "#D8D8C4"

