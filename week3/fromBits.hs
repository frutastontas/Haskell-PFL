fromBits :: [Int] -> Int
fromBits xs = loop xs 0
  where
    loop [] acc     = acc
    loop (x:xs) acc = loop xs (acc * 2 + x) -- shift left (multiply by 2) then add the new bit~


fromBitsalt :: [Int] -> Int
fromBitsalt xs = foldl (\acc b -> acc*2 +b) 0 xs