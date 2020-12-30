data Colour = Red | Green | Blue | Yellow deriving Show

data Sequence a = Nil | Cons a (Sequence a) deriving Show

length' :: Num b => [a] -> b
append :: [a] -> [a] -> [a]

length' [] = 0
length' (_:xs) = 1 + length' xs

append [] ys = ys
append (x:xs) ys = x : append xs ys

seqLength :: Num b => Sequence a -> b

seqLength Nil = 0
seqLength (Cons x xs) = 1 + seqLength xs

seqAppend :: Sequence a -> Sequence a -> Sequence a

seqAppend Nil ys = ys
seqAppend (Cons x xs) ys = Cons x (seqAppend xs ys)

data Expr a = Num a
	    | Add (Expr a) (Expr a)
	    | Subtract (Expr a) (Expr a)   
            | Multiply (Expr a) (Expr a)
	    | Divide (Expr a) (Expr a) deriving Show

evaluate :: Integral a => Expr a -> a

evaluate (Num x) = x
evaluate (Add e e') = evaluate e + evaluate e'
evaluate (Subtract e e') = evaluate e - evaluate e'
evaluate (Multiply e e') = evaluate e * evaluate e'
evaluate (Divide e e') = evaluate e `div` evaluate e'
