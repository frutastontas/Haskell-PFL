leastDiv :: Integer-> Integer
leastDiv n = loop 2
    where 
        loop i
            | (n `mod` i == 0) = i
            | i*i > n  = n
            | otherwise = loop (i+1)

isPrimeFast :: Integer-> Bool
isPrimeFast n = (n > 1) && (leastDiv n == n)