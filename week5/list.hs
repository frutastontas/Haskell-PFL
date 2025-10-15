data List a = Empty | Cons a (List a)
                deriving (Eq, Show)

toList :: [a]-> List a
toList [] = Empty
toList (x:xs) = Cons x (toList xs)

fromList :: List a-> [a]
fromList Empty = []
fromList (Cons x xs)  = x : fromList(xs)
