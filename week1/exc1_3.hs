second :: [a] -> a
second xs = xs !! 1 -- starts indexing from 0

lastalt :: [a] -> a
lastalt xs = head (reverse xs)
-- can also index it xs !! (lenght xs -1)

initalt :: [a] -> [a]
initalt xs = reverse (tail (reverse xs))

-- will give middle element
middle :: [a] -> a
middle xs = xs !! xa
                where xa = length xs `div` 2

-- we have to specify that the type of a wil be checked for equality
checkPalindrome :: Eq a => [a] -> Bool
checkPalindrome xs = xs == reverse xs


