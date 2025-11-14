divisors :: Integer -> [Integer]
divisors n = filter (\d -> n `mod` d == 0 ) [1..n]

isPrimeFast :: Integer-> Bool
isPrimeFast n = all (\x -> (x > floor (sqrt (fromIntegral n))) || (x < 2)) (divisors n)
