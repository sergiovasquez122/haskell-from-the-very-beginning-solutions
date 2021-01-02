makeVector :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
vectorLength :: Floating a => (a, a) -> a
offsetPoint :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
scaleToLength :: (Eq a, Floating a) => a -> (a, a) -> (a, a)
gcd' :: Integral a => a -> a -> a

gcd' a b = if b == 0 then a
		     else gcd' b (a `rem` b)
makeVector (x0, y0) (x1, y1) = (x1 - x0, y1 - y0)
vectorLength (x, y) = sqrt(x * x + y * y)
offsetPoint (x, y) (px, py) = (px + x, py + y)
scaleToLength l (a, b) = 
	if current_length == 0 then (a, b) else (a * factor, b * factor) 
		where 
			current_length = vectorLength (a, b)
			factor = l / current_length
-- 1. Write a function to find the point midway between two given points in two dimensions
midPoint :: Floating a => (a, a) -> (a, a) -> (a, a)
midPoint (x0, y0) (x1, y1) = (x, y)
                             where x = ((x0 + x1) / 2)
				   y = ((y0 + y1) / 2)

-- 2. Give a function roundNum which rounds  positive real number to the nearest whole number, returning it as another real number. You may use the built-in ceiling function, which is the opposite of the floor function
roundNum :: (Ord a, RealFrac a, Integral b) => a -> b
roundNum x = let 
             ceil_x = ceiling x
             floor_x = floor x
             in 
              if (fromIntegral ceil_x - x) <= (x - fromIntegral floor_x) 
		 then ceil_x
                 else floor_x

-- 3. With a function to separate a real number into its whole and fractional parts. Return them as a tuple
separate :: (RealFrac a, Integral b) => a -> (b, a) 
separate x = if x < 0 then 
		      let (a, b) = separate (-x)
			in (-a, b)
                      else 
	     let
             whole_part = floor x
             real_part = x - fromIntegral whole_part
             in
		(whole_part, real_part)
-- 4. Write a function star of type which, given a number zero and one, draws an asterisk to indicate the position. An argument of zero will result in an asterisk in column one, and an argument of one an asterisk in column fifty. 
replicate' :: (Num a, Eq a) => a -> Char -> String
replicate' 0 _ = []
replicate' n x = x : replicate' (n - 1) x

makeLine :: (Num a, Eq a) => a -> String
makeLine x = replicate' x ' ' ++ ['*', '\n']

star :: (RealFrac a) => a -> String

star x = let 
	 value = roundNum (x * 50)
	 in 
		if value == 50
     		then makeLine 49
		else makeLine value

plot :: (Ord a, Num a, RealFrac b) => (a -> b) -> a -> a -> a -> String
plot f a b dy = 
	if a > b then
	  []
        else 
          star (f a) ++ plot f (a + dy) b dy
-- 6. Add the bool and char Types to our type class diagram 
-- bool and char can be enumerated and are comparable
-- they cannot provide any operations that would fit them into other categories

