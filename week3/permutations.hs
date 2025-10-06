intercalate :: a -> [a] -> [[a]]
intercalate n [] = [[n]]
intercalate n (x:xs) =
  [n : x : xs] ++ map (x :) (intercalate n xs)



