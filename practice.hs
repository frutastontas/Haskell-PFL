

-- This file is only oriented to solving exercises

data AVLTree a = Empty
 | Node (AVLTree a) a (AVLTree a)
 deriving (Show,Eq)

height :: (Num b, Ord b) => AVLTree a-> b
height Empty = 0
height (Node t1 _ t2) = 1 + max (height t1) (height t2)


isBalanced :: AVLTree a -> Bool
isBalanced Empty = True
isBalanced (Node Empty _ right) = height right < 2
isBalanced (Node left _ Empty) = height left < 2
isBalanced (Node left key right) = (abs(height left - height right) < 2) && isBalanced left && isBalanced right


