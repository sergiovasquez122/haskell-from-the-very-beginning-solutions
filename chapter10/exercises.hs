-- 1. Design a new type Rect for representing rectangles. Treat squares as a special case
data Rect a = Square a | Rectangle a a deriving Show

calculateArea :: (Num a) => Rect a -> a

calculateArea (Square a) = a * a
calculateArea (Rectangle l w) = l * w

rotateRectangle :: (Num a, Ord a) => Rect a -> Rect a

rotateRectangle (Square a) = (Square a)
rotateRectangle (Rectangle l w) = (Rectangle (max l w) w)
