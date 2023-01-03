import Data.List

divisorsThreeDigits :: Integer -> [(Integer, Integer)]
divisorsThreeDigits x = filter (\a -> fst(a) >= 100 && fst(a) <= 999 && snd(a) >= 100 && snd(a) <= 999)  ([(y, div x y) | y <- [1..(floor.sqrt.fromIntegral) x], mod x y == 0])
isPalindrome :: Integer -> Bool
isPalindrome x = x == (read(reverse(show(x)))::Integer)
checkAllThreeDigits :: Integer -> Integer
checkAllThreeDigits x = if x < 10000 then 10201 else (if isPalindrome x && length(divisorsThreeDigits x) > 0 then x else checkAllThreeDigits (x - 1))

main :: IO()
main = do
    print (checkAllThreeDigits 999999)