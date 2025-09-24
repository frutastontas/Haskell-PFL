max3, min3 :: Ord a => a-> a-> a->a

max3 x y z = if x>=y then (if x >= z then x else z) else (if y>=z then y else z)

min3 x y z = min x (min y z)