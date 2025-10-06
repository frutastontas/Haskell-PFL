
toBits :: Int-> [Int]
toBits n = loop [] n
        where 
            loop xs x | x > 0 = loop ((x`mod`2) : xs) (x`div`2)
                      | otherwise = xs
