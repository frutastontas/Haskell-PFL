myconcat :: [[a]] -> [a]
myconcat xss = [x | xs <- xss, x <- xs]