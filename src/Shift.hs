{-#Language MultiWayIf, LambdaCase, BangPatterns#-}
module Shift where

import Data.Char
import Data.List
import Data.Foldable

import Data
import Point
import Quar

-- функция для получения одной из координат
shift :: String -> Point -> Double
shift xs p = formEl (formLexTree xs) p

-- Строим лексическое дерево.
formLexTree :: String -> Lex
formLexTree = restruct.toTree.cleaning.bracket

formEl :: Lex -> Point -> Double
formEl = \case
    Signum a      -> (\p -> -1 * formEl a p)
    Sig Plus a b  -> (\p -> formEl a p + formEl b p)
    Sig Minus a b -> (\p -> formEl a p - formEl b p)
    Sig Mull a b  -> (\p -> formEl a p * formEl b p)
    Sig Div a b   -> (\p -> formEl a p / formEl b p)
    Var X         -> (\(Point x _) -> x)
    Var Y         -> (\(Point _ y) -> y)
    Int a         -> (\_ -> a)
    Bracket    [a]-> formEl a
    Fun Sin  a  -> sin.formEl a
    Fun Ln   a  -> log.formEl a
    Fun Cos  a  -> cos.formEl a
    Fun Tan  a              -> tan.formEl a
    Fun Pot (Bracket [a,b]) -> (\p -> formEl a p ** formEl b p)
    Fun Log (Bracket [a,b]) -> (\p -> logBase (formEl a p) (formEl b p))
    Fun Max (Bracket a)     -> (\p -> maximum $ map (\a -> formEl a p) a)
    Fun Min (Bracket a)     -> (\p -> minimum $ map (\a -> formEl a p) a)
    Fun Sqrt  a             -> sqrt.formEl a
    Fun Exp   a             -> exp.formEl a
    Fun Asin  a             -> asin.formEl a
    Fun Acos  a             -> acos.formEl a
    Fun Atan  a             -> atan.formEl a
    Fun Sinh  a             -> sinh.formEl a
    Fun Cosh  a             -> cosh.formEl a
    Fun Tanh  a             -> tanh.formEl a
    Fun Asinh a             -> asinh.formEl a
    Fun Acosh a             -> acosh.formEl a
    Fun Atanh a             -> atanh.formEl a

-- Корректируем синтаксическое дерево.
restruct :: Lex -> Lex
restruct x = case x of
    Signum    (Sig a   b c) -> restruct $ Sig a (Signum b) c
    Signum  a               -> Signum $ restruct a
    Sig Div a (Sig Div b c) -> restruct $ Sig Div (Sig Div a b) c
    Sig Minus a (Sig Minus b c) -> restruct $ Sig Minus (Sig Minus a b) c
    Fun Pot (Bracket [Var x, Int 2]) -> Sig Mull (Var x) (Var x)
    Fun a b                 -> Fun a $ restruct b
    Bracket a               -> Bracket $ map restruct a
    Sig a b c               -> Sig a (restruct b) (restruct c)
    a                       -> a


-- Преобразуем список лексем в синтаксическое дерево.
toTree :: [Lex] -> Lex
toTree s = case span (/= Lex "+") s of
    ([], _:x) -> toTree x
    (y, _:x)  -> Sig Plus (toTree y) (toTree x)
    (y, _)    -> case span (/= Lex "-") y of
        ([], Lex "-":x)-> Signum $ toTree x
        (y, _:x)       -> Sig Minus (toTree y) (toTree x)
        (y, _)         -> case span (/= Lex "*") y of
            (y, _:x)   -> Sig Mull (toTree y) (toTree x)
            (y, _)     -> case span (/= Lex "/") y of
                (y, _:x) -> Sig Div (toTree y) (toTree x)
                (y, _)   -> case y of
                    [Lex x] | x == "x" -> Var X
                            | x == "y" -> Var Y
                            | x == "e" -> Int $! exp 1
                            | x == "pi" -> Int pi
                    Lex x : xs   -> Fun (fromStr x)   (toTree xs)
                    [Bracket xs] -> Bracket $ spaner (/= Lex ",") toTree xs
                    [Int i]      -> Int i
                    x            -> error $ show x

-- TODO Переписать через Map.
fromStr = \case
    "sin"   -> Sin;  "ln"    -> Ln;    "cos"   -> Cos
    "tan"   -> Tan;  "pot"   -> Pot;   "log"   -> Log
    "max"   -> Max;  "min"   -> Min;   "sqrt"  -> Sqrt
    "exp"   -> Exp;  "asin"  -> Asin;  "acos"  -> Acos
    "atan"  -> Atan; "sinh"  -> Sinh;  "cosh"  -> Cosh
    "tanh"  -> Tanh; "asinh" -> Asinh; "acosh" -> Acosh
    "atanh" -> Atanh

-- Делим список на подсписки.
spaner :: Query a -> Fold a b -> [a] -> [b]
spaner query fold list = case span query list of
    (y, x:xs) ->  fold y:spaner query fold xs
    (y, _)    -> [fold y]

-- Удаление пробельных лексем.
cleaning :: [Lex] -> [Lex]
cleaning = foldr' (\a b -> case a of
    Lex ""    -> b
    Bracket x -> Bracket (cleaning x):b
    x         -> x:b) []

-- Расстановка скобок.
bracket :: String -> [Lex]
bracket str = fst (bracket' [] str) ++ end
  where
    br = snd $ bracket' [] str
    end = if
        | br == ""  -> []
        | otherwise -> bracket br

bracket' :: [Lex] -> String -> ([Lex], String)
bracket' l (s:sx) = if
    -- Нужна внешняя функции которая рзберет sx.
    | s == ')' -> (reverse l, sx)
    | s == '(' -> bracket' (Bracket (fst brackets) : l) (snd brackets)
    | otherwise -> bracket' (fst lex : l) (snd lex)
      where
        brackets :: ([Lex], String)
        brackets = bracket' [] sx

        lex :: (Lex, String)
        lex = lexer (Lex "") (s:sx)
bracket' l "" = (reverse l, "")


-- Выделение отдельных лексем.
lexer :: Lex -> String -> (Lex, String)
-- Обрабатываем первый символ лексемы.
lexer (Lex "") (s:sx) = if
    | isNumber s     -> lexer (Int $ read [s]) sx
    | s == ' '       -> (Lex "", sx)
    | s == ')'       -> (Lex "", ')':sx)
    -- Если это был знак операции.
    | s`elem`"+-*/," -> (Lex [s], sx)
    | otherwise      -> lexer (Lex [s]) sx
-- Обрабатываем названия функций.
lexer (Lex xs) (s:sx) = if
    | isNumber s     -> (Err "Число не может быть частью названия функции!", sx)
    | s == ' '       -> (Lex $ reverse xs, sx)
    | s`elem`"()+-*/,"-> (Lex $ reverse xs, s:sx)
    | otherwise      -> lexer (Lex $ s:xs) sx
-- обрабатываем числа.
lexer (Int i) (s:sx) = if
    | isNumber s     -> lexer (Int $ 10*i + read [s]) sx
    | s == ' '       -> (Int i, sx)
    | s`elem`"()+-*/," -> (Int i, s:sx)
    | s == '.'       -> lexer (Double i 0.1) sx
    | otherwise      -> (Err "Буква не может быть частью числа", sx)
lexer (Double i d) (s:sx) = if
    | isNumber s     -> lexer (Double (i + d * (read [s])) (d/10)) sx
    | s == ' '       -> (Int i, sx)
    | s`elem`"()+-*/,"-> (Int i, s:sx)
    | otherwise      -> (Err "Буква не может быть частью числа", sx)
lexer s _ = case s of
    Double i _ -> (Int i, "")
    _          -> (s, "")
