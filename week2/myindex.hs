myindex :: [a] -> Int -> a
myindex xs n = head [x | (x,i) <- zip xs [0..] , i == n]