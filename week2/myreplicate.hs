myreplicate :: Int -> a -> [a]
myreplicate n element = [x | i <- [1..n], x <- [element]]