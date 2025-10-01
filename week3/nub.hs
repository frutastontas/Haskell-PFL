removeAll :: Eq a => a -> [a] -> [a]
removeAll _ [] = []
removeAll a (x:xs) = if (a == x) then removeAll a xs -- skip dups 
                    else [x] ++ removeAll a xs -- add x to the solution



nub :: Eq a => [a]-> [a]
nub [] = []
nub (x:xs) = [x] ++ nub (removeAll x xs)