
lefthalf :: [a]->[a]
lefthalf xs = take a xs
                where a = length xs `div` 2

righthalf :: [a]->[a]
righthalf xs = drop a xs
                where a = length xs `div` 2