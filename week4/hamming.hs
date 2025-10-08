
aux :: Int -> [Int]
aux n = [2^i*3^j*5^k | i<-[0..n], j<-[0..n], k<-[0..n], i+j+k == n]

hamming :: [Int]
hamming =  concat[aux x | x<-[0..]]


merge :: [Integer]-> [Integer]-> [Integer]
merge (x:xs) (y:ys) | x < y = x:merge xs (y:ys)
                    | x == y = x :merge xs ys
                    | otherwise = y: merge (x:xs) ys

hammingalt :: [Integer]
hammingalt = 1: merge (merge (map (2*) hammingalt) (map (3*) hammingalt) ) (map (5*) hammingalt)