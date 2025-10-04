myappend :: [a]-> [a]-> [a]
myappend xs ys = foldr (:) ys xs

myconcat :: [[a]]-> [a]
myconcat xss = foldr (++) [] xss



-- ???????????????????????????
myreverse :: [a] -> [a]
myreverse xs = foldr (\x acc -> acc ++ [x]) [] xs

myreversealt :: [a] -> [a]
myreversealt xs = foldl (\acc x -> x : acc) [] xs

myfilter :: (a -> Bool) -> [a] -> [a]
myfilter f xs = foldr (\x acc -> if f x then x : acc else acc) [] xs

mymap :: (a -> b) -> [a] -> [b]
mymap f xs = foldr (\x acc -> f x : acc) [] xs