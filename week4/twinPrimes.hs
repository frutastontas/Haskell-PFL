primes :: [Integer]
primes = sieve [2..]
sieve :: [Integer] -> [Integer]
sieve (p:xs) = p : sieve [x | x<-xs, x`mod`p/=0]


twinPrimes :: [(Integer,Integer)]
twinPrimes = [(p1,p2) | (p1,p2) <- zip primes (tail primes) , p2-p1 == 2 ]
