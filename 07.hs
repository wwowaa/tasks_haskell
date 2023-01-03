import Data.List

divisors :: Integer -> [Integer]
divisors x = sort(nub(concat ([[y, div x y] | y <- [1..(floor.sqrt.fromIntegral) x], mod x y == 0])))
isPrime :: Integer -> Bool
isPrime x = x == 2 || (divisors x == [1,x])
getPrimes :: [Integer]
getPrimes = [x | x <- [2,3..], isPrime x == True]
main :: IO()
main = do
    print (last (take 10001 getPrimes))