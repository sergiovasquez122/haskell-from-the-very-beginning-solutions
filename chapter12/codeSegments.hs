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

midPoint :: Floating a => (a, a) -> (a, a) -> (a, a)

midPoint (x0, y0) (x1, y1) = (x, y)
                             where x = ((x0 + x1) / 2)
				   y = ((y0 + y1) / 2)

roundNum :: (Ord a, RealFrac a, Integral b) => a -> b

roundNum x = let 
             ceil_x = ceiling x
             floor_x = floor x
             in 
              if (fromIntegral ceil_x - x) <= (x - fromIntegral floor_x) 
		 then ceil_x
                 else floor_x
