module GrBuild where

import Data
import Point
import SymbolicImage

form = 150

--                                                   --
--          Функции вывода данных в файл             --
--                                                   --
-- Формирование ломаной
formPathFile :: Space -> [Point] ->  IO ()
formPathFile s x = do
    let svg :: String
        svg = scale s ++ mkPath (transf s <$> x)
    writeFile "grSvg.svg" $ svgFile svg s

-- формирование образа
formGrFile :: Space -> Imagination ->  IO ()
formGrFile s i = writeFile "grSvg.svg" $
    svgFile (scale s ++ mkImag s i) s
--                                                   --
--             Шаблонная часть SVG-файла             --
--                                                   --
svgFile :: String -> Space -> String
svgFile x s =
    xml ++
    "\t<svg" ++
    t ++ viewBox s   ++
    t ++ version 1.1 ++ ">" ++ x ++
    "\t</svg>"


-- Формируем поле показывающее область просмотра.
viewBox :: Space -> String
viewBox s@(Space p1 p2) = concat [
    "viewBox=\"",     show x1, ' ':show y1,
                  ' ':show x2, ' ':show y2,
    "\""]
  where
    Point x1 y1 = transf s p1
    Point x2 y2 = transf s p2

xml = "<?xml " ++ version 1.0 ++ "?>\n"
version  x = "version=\"" ++ show x ++ "\""
t = "\n\t\t"


-- Меняем систему координат в которой существует точка
transf :: Space -> F Point
transf (Space (Point x1 y1) _) (Point x y) =
    Point (form*(x-x1)) (form*(y-y1))
--                                                   --
--                Построение ломаной                 --
--                                                   --
-- стандартный стиь оформления для ломаной.
pathStyle =
    "style=\"fill:none;stroke:#000000;stroke-width:1px\""


-- Формируем ломаную
mkPath :: [Point] -> String
mkPath x = "<path \n" ++
    pathStyle ++ " d=\"M " ++ path x ++ "\"" ++ " />"
  where
    path :: [Point] -> String
    path = concat.ins " ".map
        (\(Point x y) -> show x ++ ',':show y)

    ins :: x -> F [x]
    ins p (x:y:xs) = x:p:ins p (y:xs)
    ins p (x:xs)   = x:p:xs
--                                                   --
--                Построение образа                  --
--                                                   --
mkImag :: Space -> Imagination -> String
mkImag s (Imagination d x) = concatMap
    (\p -> rect d $ transf s $ fromCeil d p) x


-- Построение квадрата
rect :: Double -> Point -> String
rect d (Point x y) = concat ["<rect",
    t,"width=\"", show (form*d), "\"",
    t,"height=\"", show (form*d), "\"",
    t,"x=\"", show x, "\"",
    t,"y=\"", show y, "\"/>\n"]
--                                                   --
--                 Фон изображения                   --
--                                                   --
-- Строим координатную сетку, чтобы показать разбиение
-- области на участки.
scale :: Space -> String
scale s@(Space (Point x1 y1) (Point x2 y2)) =
    t ++ mkPath (f [Point x1 0, Point x2 0]) ++
    t ++ mkPath (f [Point 0 y1, Point 0 y2])
  where
    f = map (transf s)
