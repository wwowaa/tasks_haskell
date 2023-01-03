import Data.List

--generating pairs of multipliers of number, it's worth consdering that in pair only one multiplier can be more than square root of number, so we count y to sqrt of current number
divisors :: Integer -> [Integer]
divisors x = sort(nub(concat ([[y, div x y] | y <- [1..(floor.sqrt.fromIntegral) x], mod x y == 0])))
isPrime :: Integer -> Bool
isPrime x = x == 2 || (divisors x == [1,x])
findLargestPrimeFactorWithDivs :: [Integer] -> Integer
findLargestPrimeFactorWithDivs xs = if null xs then 1 else (if isPrime (last(xs)) then last(xs) else findLargestPrimeFactorWithDivs (init(xs)))
findLargestPrimeFactor :: Integer -> Integer
findLargestPrimeFactor x = findLargestPrimeFactorWithDivs (divisors x)
main :: IO()
main = do
    print (findLargestPrimeFactor 600851475143)