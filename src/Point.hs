{-# Language
    BangPatterns,
    DeriveAnyClass,
    FlexibleInstances#-}

module Point where

data Point = Point {
    x, y :: !Double
  } deriving (Eq, Show)

class Points a where
    toPoint :: a -> Point
    fromPoint :: Point -> a


instance Points Point where
    toPoint = id
    fromPoint = id


instance Points [Double] where
    toPoint [x, y] = Point x y
    fromPoint (Point x y) = [x, y]


-- Реализуем мат действия над точками, как над комплексными
-- числами.
instance Num Point where
    (+) (Point x1 y1) (Point x2 y2) = Point (x1 + x2) (y1 + y2)

    (-) (Point x1 y1) (Point x2 y2) = Point (x1 - x2) (y1 - y2)

    negate (Point x y) = Point (-x) (-y)

    (*) (Point x1 y1) (Point x2 y2) = Point
        (x1*x2 - y1*y2) (x1*y2 + x2*y1)

    abs (Point x y) = Point (sqrt $ x^2 + y^2) 0

    signum x = div x $ abs x
      where
        -- деление.
        div :: Point -> Point -> Point
        div (Point x1 y1) (Point x2 y2) = Point
            ((x1*x2 + y1*y2)/d) ((x2*y1 - x1*y2)/d)
          where
            -- делитель.
            d :: Double
            d = x2^2 + y2^2

    fromInteger x = Point (fromInteger x) 0
