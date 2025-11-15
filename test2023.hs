
-- This file is only oriented to solving TP exercises


type Species = (String, Int)
type Zoo = [Species]

isEndangered :: Species -> Bool
isEndangered (name,value) | value > 100 = False
                          | otherwise = True

updateSpecies :: Species -> Int -> Species
updateSpecies (name,value) newvalue = (name, value + newvalue)

filterSpecies :: Zoo -> (Species -> Bool) -> Zoo
filterSpecies [] _ = []
filterSpecies (x:xs) predicate | predicate x = x : filterSpecies xs predicate
                               | otherwise = filterSpecies xs predicate


countAnimals :: Zoo -> Int
countAnimals zoo = foldr (\spec acc -> snd spec + acc) 0 zoo

substring :: (Integral a) => String -> a -> a -> String
substring name ini fin = [a | (a,index) <- zip name [0..], index >= ini && index <= fin]

hasSubstr :: String -> String -> Bool
hasSubstr s1 s2 = loop s1 s2 0 (length s2 -1)
    where 
        loop s1 s2 ini fin | fin > (length s1 -1) = False   -- cannot index further so no substring
                           | subS1 == s2 = True
                           | otherwise = loop s1 s2 (ini+1) (fin +1) -- if not then try to find a substring in the next positions
                            where subS1 = substring s1 ini fin

sortSpeciesWithSubstr :: Zoo -> String -> (Zoo, Zoo)
sortSpeciesWithSubstr [] _ = ([],[])
sortSpeciesWithSubstr zoo str = (filter (\(name,value)-> hasSubstr name str) zoo, filter (\(name,value)-> not (hasSubstr name str)) zoo)



rabbits :: (Integral a) => [a]
rabbits = 2 : 3 : [y1+y2 | (y1,y2) <- zip rabbits (tail rabbits)]

rabbitYears :: (Integral a) => a -> Int
rabbitYears n = loop rabbits n 0
    where
        loop (r:rab) number cur_year | r >= number = cur_year
                                     | otherwise = loop rab number (cur_year+1)


data Dendrogram = Leaf String | Node Dendrogram Int Dendrogram

leftMost :: Dendrogram -> Int
leftMost (Leaf _) = 0
leftMost (Node left dist right) = dist + leftMost left

rightMost :: Dendrogram -> Int
rightMost (Leaf _) = 0
rightMost (Node left dist right) = dist + rightMost right

dendroWidth :: Dendrogram -> Int
dendroWidth (Leaf _) = 0
dendroWidth deandro = leftMost deandro + rightMost deandro

myDendro :: Dendrogram
myDendro = Node (Node (Leaf "dog") 3 (Leaf "cat")) 5 (Leaf "octopus")

leftSearch :: Dendrogram -> Int -> Int -> [String]
leftSearch (Leaf name) cur_dist thresh_hold = if (cur_dist <= thresh_hold) then [name] else []
leftSearch (Node left dist right) cur_dist thold = (leftSearch left (cur_dist + dist) thold) ++ (leftSearch right (cur_dist - dist) thold)

rightSearch :: Dendrogram -> Int -> Int -> [String]
rightSearch (Leaf name) cur_dist thresh_hold = if (cur_dist <= thresh_hold) then [name] else []
rightSearch (Node left dist right) cur_dist thold = (rightSearch left (cur_dist - dist) thold) ++ (rightSearch right (cur_dist + dist) thold)

dendroInBounds :: Dendrogram -> Int -> [String]
dendroInBounds (Node left dist right) thold = (leftSearch left dist thold) ++ (rightSearch right dist thold)