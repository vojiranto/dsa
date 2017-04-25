{-#Language MultiWayIf, LambdaCase, BangPatterns#-}
module Shift where

import Data.Char
import Data.Foldable

import Data
import Point

-- функция для получения одной из координат
shift :: String -> Point -> Double
shift xs p = formEl (formLexTree xs) p

-- Строим лексическое дерево.
formLexTree :: String -> Lex
formLexTree = restruct.toTree.cleaning.bracket

formEl :: Lex -> Point -> Double
formEl = \case
    Signum a      -> (\p -> -formEl a p)
    Sig s a b     -> (\p -> sig s (formEl a p) (formEl b p))
    Var a         -> var a
    Int a         -> const a
    Bracket [a]   -> formEl a
    Fun Pot (Bracket [a,b]) -> (\p -> formEl a p ** formEl b p)
    Fun Log (Bracket [a,b]) -> (\p -> logBase (formEl a p) (formEl b p))
    Fun Max (Bracket a)     -> (\p -> maximum $ (flip formEl p) <$> a)
    Fun Min (Bracket a)     -> (\p -> minimum $ (flip formEl p) <$> a)
    Fun fu  a               -> (\p -> fun fu $ formEl a p)
    Err a                   -> error a
    _                       -> error
        "Undefined error in function formEl in module Shift."

  where
    fun :: Func -> F Double
    fun = \case
        Sin   -> sin;  Ln   -> log; Cos     -> cos; Tan -> tan;
        Sqrt  -> sqrt; Exp  -> exp; Asin    -> asin;Acos-> acos;
        Atan  -> atan; Sinh -> sinh;Cosh    -> cosh;Tanh-> tanh;
        Asinh -> asinh;Acosh-> acosh;Atanh  -> atanh

    sig :: Sigs -> (Double -> Double -> Double)
    sig = \case
        Plus   -> (+)
        Minus  -> (-)
        Mull   -> (*)
        Div    -> (/)

    var :: Vars -> (Point -> Double)
    var = \case
        X   -> x
        Y   -> y

-- Корректируем синтаксическое дерево.
restruct :: F Lex
restruct = \case
    Signum    (Sig a   b c) -> restruct $ Sig a (Signum b) c
    Signum  a               -> Signum $ restruct a
    Sig Div a (Sig Div b c) -> restruct $ Sig Div (Sig Div a b) c
    Sig Minus a (Sig Minus b c) -> restruct $ Sig Minus (Sig Minus a b) c
    Fun Pot (Bracket [Var a, Int 2]) -> Sig Mull (Var a) (Var a)
    Fun a b                 -> Fun a $ restruct b
    Bracket a               -> Bracket $ map restruct a
    Sig a b c               -> Sig a (restruct b) (restruct c)
    a                       -> a


-- Преобразуем список лексем в синтаксическое дерево.
toTree :: [Lex] -> Lex
toTree s = case span (/= Lex "+") s of
    ([], _:ls) -> toTree ls
    (l,  _:ls) -> Sig Plus (toTree l) (toTree ls)
    (l,  _)    -> fromMin l
  where
    fromMin :: [Lex] -> Lex
    fromMin a = case span (/= Lex "-") a of
        ([], Lex "-":ls)-> Signum $ toTree ls
        (l, _:ls)       -> Sig Minus (toTree l) (toTree ls)
        (l, _)          -> fromSig "*" Mull
                            (fromSig "/" Div fromV) l

    fromSig :: String -> Sigs -> ([Lex] -> Lex) -> [Lex] -> Lex
    fromSig c sig fun a = case span (/= Lex c) a of
        (l, _:ls) -> Sig sig (toTree l) (toTree ls)
        (l, _)    -> fun l

    fromV :: [Lex] -> Lex
    fromV = \case
        [Lex l] | l == "x"  -> Var X
                | l == "y"  -> Var Y
                | l == "e"  -> Int $! exp 1
                | l == "pi" -> Int pi
        Lex l : ls   -> Fun (fromStr l)   (toTree ls)
        [Bracket ls] -> Bracket $ spaner (/= Lex ",") toTree ls
        [Int i]      -> Int i
        l            -> error $ show l

-- TODO Переписать через Map.
fromStr :: String -> Func
fromStr = \case
    "sin"   -> Sin;  "ln"    -> Ln;    "cos"   -> Cos
    "tan"   -> Tan;  "pot"   -> Pot;   "log"   -> Log
    "max"   -> Max;  "min"   -> Min;   "sqrt"  -> Sqrt
    "exp"   -> Exp;  "asin"  -> Asin;  "acos"  -> Acos
    "atan"  -> Atan; "sinh"  -> Sinh;  "cosh"  -> Cosh
    "tanh"  -> Tanh; "asinh" -> Asinh; "acosh" -> Acosh
    "atanh" -> Atanh;_       -> error "Undefined function"

-- Делим список на подсписки.
spaner :: Query a -> Fold a b -> [a] -> [b]
spaner !query !foldf !list = case span query list of
    (a, _:ls) ->  foldf a:spaner query foldf ls
    (a, _)    -> [foldf a]

-- Удаление пробельных лексем.
cleaning :: F [Lex]
cleaning = foldr' (\a b -> case a of
    Lex ""    -> b
    Bracket c -> Bracket (cleaning c):b
    c         -> c:b) []

-- Расстановка скобок.
bracket :: String -> [Lex]
bracket !str = fst (bracket' [] str) ++ end
  where
    br = snd $! bracket' [] str
    end = if
        | br == ""  -> []
        | otherwise -> bracket br

bracket' :: [Lex] -> String -> ([Lex], String)
bracket' !l !(s:sx) = if
    -- Нужна внешняя функции которая рзберет sx.
    | s == ')' -> (reverse l, sx)
    | s == '(' -> bracket' (Bracket (fst brackets) : l) (snd brackets)
    | otherwise -> bracket' (fst lexem : l) (snd lexem)
      where
        brackets :: ([Lex], String)
        brackets = bracket' [] sx

        lexem :: (Lex, String)
        lexem = lexer (Lex "") (s:sx)
bracket' l "" = (reverse l, "")


-- Выделение отдельных лексем.
lexer :: Lex -> String -> (Lex, String)
-- Обрабатываем первый символ лексемы.
lexer !(Lex "") !(s:sx) = if
    | isNumber s     -> lexer (Int $ read [s]) sx
    | s == ' '       -> (Lex "", sx)
    | s == ')'       -> (Lex "", ')':sx)
    -- Если это был знак операции.
    | s`elem`"+-*/," -> (Lex [s], sx)
    | otherwise      -> lexer (Lex [s]) sx
-- Обрабатываем названия функций.
lexer !(Lex xs) !(s:sx) = if
    | isNumber s     -> error "Число не может быть частью названия функции!"

    | s == ' '       -> (Lex $! reverse xs, sx)
    | s`elem`"()+-*/,"-> (Lex $! reverse xs, s:sx)
    | otherwise      -> lexer (Lex $! s:xs) sx
-- обрабатываем числа.
lexer !(Int i) !(s:sx) = if
    | isNumber s     -> lexer (Int $! 10*i + read [s]) sx
    | s == ' '       -> (Int i, sx)
    | s`elem`"()+-*/," -> (Int i, s:sx)
    | s == '.'       -> lexer (Double i 0.1) sx
    | otherwise      -> error "Буква не может быть частью числа"
lexer !(Double i d) !(s:sx) = if
    | isNumber s     -> lexer (Double (i + d * (read [s])) (d/10)) sx
    | s == ' '       -> (Int i, sx)
    | s`elem`"()+-*/,"-> (Int i, s:sx)
    | otherwise      -> error "Буква не может быть частью числа"
lexer !s _ = case s of
    Double i _ -> (Int i, "")
    _          -> (s, "")
