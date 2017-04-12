{-#LAnguage BangPatterns, DeriveAnyClass, FlexibleInstances#-}
module Data where

import Point

data Lex
    = Double !Double !Double
    | Int !Double
    | Lex !String
    | Bracket [Lex]
    | Err !String
    | Sig !Sigs !Lex !Lex
    | Signum !Lex
    | Fun !Func !Lex
    | Var !Vars
  deriving (Eq, Show)

data Func =
    Sin   | Ln    | Cos   | Tan  | Pot  | Log  | Max  |
    Min   | Sqrt  | Exp   | Asin | Acos | Atan | Sinh |
    Cosh  | Tanh  | Asinh | Acosh | Atanh
  deriving (Eq, Show)

data Space = Space Point Point

data LabNumb =
     Lab1 {
    imag :: Imagination
  }| Lab2 {
    ptch :: [Point]
  }| Und
  deriving Eq

data St = St {
    workLab :: LabNumb, -- запоминаем, где находимся.
    render  :: IO [()], -- храним функцию отрисовки.
    block   :: Bool,    -- блокировка итератора.
    f       :: F Point
  }

createSt :: St
createSt = St {
    workLab = Und,
    render  = return [],
    block   = False,
    f       = id
  }

data Ceil = Ceil !Int !Int
  deriving (Eq, Ord, Show)

data Ceil3 = Ceil3 !Ceil !Int
  deriving (Eq, Ord, Show)

data Imagination = Imagination !Diameter ![Ceil] deriving Eq

-- Расслоение
data Stratification = Stratification {
    d1 :: !Diameter,
    d2 :: !Diameter,
    imagination :: ![Ceil3]
  } deriving Eq

type Graf node key  = [(node, key, [key])]
type Query a        = a -> Bool
type Diameter       = Double
type Fold a b       = [a] -> b
type F a            = a -> a
type IterateNumber  = Int

data Line           = Line Point Point
  deriving (Eq, Show)

data Patch          = Patch [Point]
  deriving Eq

data Sigs        = Plus | Minus | Mull | Div
  deriving (Eq, Show)

data Vars        = X | Y
  deriving (Eq, Show)


-- Класс определяющий размер.
class Size a where
    size :: a -> Int

