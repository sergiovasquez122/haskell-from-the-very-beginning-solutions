apply :: (Num c, Eq c) => (b -> b) -> c -> b -> b

apply f 0 a = a
apply f 1 a = f a
apply f n a = apply f (n - 1) (f a)
