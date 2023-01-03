sum35Multiplies :: Int -> Int
sum35Multiplies i = if i == 1000 then 0 else (if mod i 3 == 0 || mod i 5 == 0 then i + sum35Multiplies (i+1) else sum35Multiplies (i+1))
main :: IO()
main = do 
    print (sum35Multiplies 1)