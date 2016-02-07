


euler1 :: Int
euler1 = sum ( filter (\xs -> mod xs 3 == 0 || mod xs 5 == 0 ) [1..999])


fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

euler2 :: Int
euler2 = sum(filter(\xs -> even xs) (takeWhile (<4000000) (map fib [1..])))


primes = 2 : filter (null . tail . primeFactors) [3,5..]


primeFactors n = factor n primes
    where
        factor n (p:ps)
            | p*p > n = [n]
            | n `mod` p == 0 = p : factor (n `div` p) (p:ps)
            | otherwise = factor n ps


euler3 = last (primeFactors 600851475143)



euler4 = maximum [x | y <- [100..999], z<-[y..999], let x=y*z, let s=show x, s==reverse s]

euler6 = (sum ([1..100]))^2 - sum (map (^2) [1..100])

euler7 



