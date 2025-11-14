divisors :: Integer -> [Integer]
divisors n = [d | d <- [1..n], n `mod` d == 0]


isPrime :: Integer-> Bool
isPrime n = divisors n == [1,n]



