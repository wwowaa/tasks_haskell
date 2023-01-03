fibonacci :: Int -> Int
fibonacci n = if n == 1 || n == 2 then n else fibonacci (n - 1) + fibonacci (n - 2)
fibonacciEvenNums :: [Int]
fibonacciEvenNums = [fibonacci x | x <- [1, 2..], mod (fibonacci x) (2) == 0]
getSumOfEvenFib :: Int -> Int
getSumOfEvenFib x = if fibonacci x >= 4000000 then 0 else (if mod (fibonacci x) (2) == 0 then fibonacci x + getSumOfEvenFib (x+1) else getSumOfEvenFib (x+1))
main :: IO()
main = do 
    print (getSumOfEvenFib 1)
