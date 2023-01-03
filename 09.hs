{-
	we have two formulas: a^2 + b^2 = c^2 and a + b + c = k
	a + b = k - c
	a^2 + 2*a*b + b^2 = k^2 - 2*k*c + c^2
	a^2 + b^2 = k^2 + c^2 - 2*k*c - 2*a*b
	c^2 = k^2 + c^2 - 2*k*c - 2*a*b
	k^2 - 2*k*c - 2*a*b = 0
	2*k*c = k^2 - 2*a*b
	c = (k^2 - 2*a*b)/(2*k)
-}
k :: Integer
k = 1000
getPairs :: Integer -> [(Integer, Integer)]
getPairs x = [(a,b) | a <- [1..x], b <- [1..x], a^2 + b^2 == (div (k^2 - 2*a*b) (2*k))^2]
getProduct :: (Integer, Integer) -> Integer
getProduct (a,b) = a*b*((floor.sqrt.fromIntegral) (a^2 + b^2))

main = do
    print (getProduct (head(take 1 (getPairs (div k 2)))))