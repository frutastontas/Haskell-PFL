import Set

size :: Set a-> Int
size Empty = 0
size (Node _ s_left s_right) = 1 + size (s_left) size (s_right)

height :: Set a-> Int
height Empty = 0
height (Node _ s_left s_right) = 1 + max(height s_left) (height s_right) 