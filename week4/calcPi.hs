calcPi1 :: Int-> Double

calcPi1 itr = sum (take itr (zipWith (/) (cycle [4, -4]) [1,3..]))

calcPi2 :: Int-> Double

calcPi2 itr = 3 + sum (take itr (zipWith (\a b -> a/(b*(b+1)*(b+2))) (cycle [4, -4]) [2,4..]))


