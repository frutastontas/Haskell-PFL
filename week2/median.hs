median :: Ord a => a -> a -> a -> a
median a b c
  | (b <= a && a <= c) || (c <= a && a <= b) = a
  | (a <= b && b <= c) || (c <= b && b <= a) = b
  | otherwise = c

medianAlt :: (Num a, Ord a) => a -> a -> a -> a
medianAlt a b c = total - min c (min a b) - max c (max a b)
            where total = a +b +c