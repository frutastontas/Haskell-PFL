binom :: Integer-> Integer-> Integer
binom n k = product [1..n] `div `(product [1..k] * product [1..n-k])



pascal :: Integer-> [[Integer]]
pascal n = [[binom i j | j <- [0..i]] | i <- [0..n]]
