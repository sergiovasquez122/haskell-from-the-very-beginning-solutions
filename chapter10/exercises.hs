-- 1. Design a new type Rect for representing rectangles. Treat squares as a special case
data Rect a = Square a | Rectangle a a deriving Show
-- 2. Now write a function of type Num a => Rect a -> a to calculate the area of given rect
calculateArea :: (Num a) => Rect a -> a

calculateArea (Square a) = a * a
calculateArea (Rectangle l w) = l * w
-- 3. Write a function which rotates a Rect such that it is at least as tall as it is wide
rotateRectangle :: (Num a, Ord a) => Rect a -> Rect a

rotateRectangle (Square a) = (Square a)
rotateRectangle (Rectangle l w) = (Rectangle (max l w) w)
-- 4. Use this function to write one which given a [Rect], return another such list which hs the smallest total width and whose members are sorted narrowes first
-- 5. Write version of the seqTake, seqDrop and seqMap for the Sequence type

-- 6. Extend the Expr type and the evaluate function to allow raising a number to a power.
data Expr a = Num a
	    | Add (Expr a) (Expr a)
	    | Subtract (Expr a) (Expr a)   
            | Multiply (Expr a) (Expr a)
	    | Divide (Expr a) (Expr a) 
	    | Raise (Expr a) (Expr a) deriving Show

evaluate :: Integral a => Expr a -> a

evaluate (Num x) = x
evaluate (Add e e') = evaluate e + evaluate e'
evaluate (Subtract e e') = evaluate e - evaluate e'
evaluate (Multiply e e') = evaluate e * evaluate e'
evaluate (Divide e e') = evaluate e `div` evaluate e'
evaluate (Raise e e') = evaluate e ^ evaluate e'
