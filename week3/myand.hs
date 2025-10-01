myand :: [Bool]-> Bool
myand [] = True
myand (x:xs) = x && myand (xs)

myconcat :: [[a]]-> [a]
myconcat [] = []
myconcat (xs:xss) = xs ++ myconcat xss

myreplicate :: Int-> a-> [a]
myreplicate 0 a = []
myreplicate x a = [a] ++ myreplicate (x-1) a

myindex :: [a]-> Int-> a 
myindex [x] 0 = x
myindex (x:xs) n = myindex xs (n-1)

myelem :: Eq a => a-> [a]-> Bool
myelem a [] = False
myelem a (x:xs) = (a==x) || myelem a xs