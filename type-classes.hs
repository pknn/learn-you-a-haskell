module Shapes
  ( Point,
    Shape,
    area,
    nudge,
    baseCircle,
    baseRectangle,
  )
where

data Point = Point Float Float deriving (Show)

data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

area :: Shape -> Float
area (Circle _ r) = pi * r ^ 2
area (Rectangle (Point x1 y1) (Point x2 y2)) = abs (x2 - x1) * abs (y2 - y1)

nudge :: Shape -> Point -> Shape
nudge (Circle (Point x y) r) (Point dx dy) = Circle p' r
  where
    x' = x + dx
    y' = y + dy
    p' = Point x' y'
nudge (Rectangle (Point x1 y1) (Point x2 y2)) (Point dx dy) = Rectangle p1 p2
  where
    x1' = x1 + dx
    y1' = y1 + dy
    x2' = x2 + dx
    y2' = y2 + dy
    p1 = Point x1' y1'
    p2 = Point x2' y2'

baseCircle :: Float -> Shape
baseCircle = Circle (Point 0 0)

baseRectangle :: Float -> Float -> Shape
baseRectangle width height = Rectangle (Point 0 0) (Point width height)

-- records
data Person = Person
  { firstName :: String,
    lastName :: String,
    age :: Int,
    height :: Float,
    phoneNumber :: String,
    flavor :: String
  }
  deriving (Show, Eq)

data Car = Car
  { company :: String,
    model :: String,
    year :: Int
  }
  deriving (Show)

-- data declaration with type parameter

data Vector a = Vector a a a deriving (Show)

vplus :: (Num a) => Vector a -> Vector a -> Vector a
(Vector i j k) `vplus` (Vector l m n) = Vector (i + l) (j + m) (k + n)

dotProduct :: (Num a) => Vector a -> Vector a -> a
(Vector i j k) `dotProduct` (Vector l m n) = i * l + j * m + k * n

vmult :: (Num a) => Vector a -> Vector a -> Vector a
(Vector i j k) `vmult` (Vector l m n) = Vector (i * l) (j * m) (k * n)
