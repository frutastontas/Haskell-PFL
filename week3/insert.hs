insert :: Ord a => a-> [a]-> [a]
insert x [] = [x]
insert x (y:ys)
        | x <= y = x : y : ys -- first x then y and the tail
        | otherwise = y : insert x ys


isort :: Ord a => [a]-> [a]
isort [] = []
isort (x:xs) = insert x (isort xs)