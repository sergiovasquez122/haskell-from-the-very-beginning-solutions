-- 1. Design a new type Rect for representing rectangles. Treat squares as a special case
data Rect a = Square a | Rect a a deriving Show
