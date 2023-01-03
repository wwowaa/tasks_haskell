--lcm(a,b) = a*b/gcd(a,b)
myGCD :: Integer -> Integer -> Integer
myGCD 0 b = b
myGCD a 0 = a
myGCD a b = if b > a then myGCD (a) (mod b a) else myGCD (b) (mod a b)
myLCM :: Integer -> Integer -> Integer
myLCM a b = div (a*b) (myGCD a b)
lcmOf20 :: Integer -> Integer
lcmOf20 x = if x <= 1 then 1 else myLCM (x) (lcmOf20 (x-1))

main :: IO()
main = do
    print (lcmOf20 20)