--works too long :(
import Data.List

k :: Integer
k = 2000000
divisors :: Integer -> [Integer]
divisors x = sort(nub(concat ([[y, div x y] | y <- [1..(floor.sqrt.fromIntegral) x], mod x y == 0])))
isPrime :: Integer -> Bool
isPrime x = divisors x == [1,x]
getPrimes :: [Integer]
getPrimes = 2:[x | x <- [3,5..k], isPrime x == True]
getSumPrimes :: [Integer] -> Integer
getSumPrimes xs = if null xs then 0 else head(xs) + getSumPrimes (tail(xs))

main :: IO()
main = do
    print (getSumPrimes (getPrimes))