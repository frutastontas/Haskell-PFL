import Data.Char
import Data.IntMap (update)

type Letras = (Char,Char)          -- um bloco de letras
type Digitos = (Int,Int)           -- um bloco de algarismos
type Matricula = (Letras, Digitos, Letras)  -- uma matrícula

validaLetras :: Letras -> Bool
validaLetras (c1,c2) = (c1 <= 'Z') && (c1 >= 'A') && (c2 <= 'Z') && (c2 >= 'A')

validaDigitos :: Digitos -> Bool
validaDigitos (x,y) = (x <= 9) && (x >= 0) && (y <= 9) && (y >= 0)

valida :: Matricula -> Bool
valida ( l1, digi, l2) = validaLetras(l1) && validaDigitos(digi) && validaLetras(l2)



incLetras :: Letras -> (Letras, Bool)
incLetras (l1,l2)
    | l2 < 'Z' = ((l1,chr(ord l2+1)), False)
    | l1 < 'Z' = ((chr(ord l1 +1),'A'), False)
    | otherwise = (('A','A'),True)


incDigitos :: Digitos -> (Digitos, Bool)
incDigitos (d1,d2) 
    | d2 < 9 = ((d1,d2+1),False)
    | d1 < 9 = ((d1+1,0),False)
    | otherwise = ((0,0),True)


incrMatricula :: Matricula -> Matricula
incrMatricula (ll, dd, rl)
    | carry_1 == False = (ll,dd,rl_new)
    | carry_2 ==False  = (ll, dd_new, rl_new)
    | otherwise = (ll_new, dd_new, rl_new)
    where 
        (rl_new, carry_1) = incLetras rl
        (dd_new, carry_2) = incDigitos dd
        (ll_new, carry_3) = incLetras ll




data Arv a = Vazia | No a (Arv a) (Arv a)
    deriving (Eq)

listarDecr :: Arv a -> [a]
listarDecr Vazia = []
listarDecr (No node left right) = listarDecr right ++ [node] ++ listarDecr left


maisDir :: Arv a -> a
maisDir (No y _ Vazia) = y
maisDir (No _ _ right) = maisDir right

remover :: Ord a => a -> Arv a -> Arv a
remover _ Vazia = Vazia
remover x (No y left right)
 | x > y = No y left (remover x right)
 | x<y = No y (remover x left) right
 | left == Vazia = right
 | right == Vazia = left
 | otherwise = No l (remover l left) right
 where l = maisDir left




palavras :: String -> [String]
palavras [] = []
palavras str =  takeWhile (/=' ') str' : palavras (dropWhile (/=' ') str')
                where str' = dropWhile (==' ') str

algarismos :: Int -> [Int]
algarismos 0 = []
algarismos x = (x `mod` 10) : algarismos (x `div` 10)




maxIndex :: Ord a => [a] -> (a, Int)
maxIndex (x:xs) = loop xs (x,0) 1
    where
        loop [] res _ = res
        loop (y:ys) (max_x,idx) cur_idx
            | y >= max_x = loop ys (y,cur_idx) (cur_idx+1)
            | otherwise = loop ys (max_x,idx) (cur_idx+1)

updateVotos :: [Int] -> Int -> Int -> [Int]
updateVotos [] _ _ = []
updateVotos (x:xs) cur_idx idx | cur_idx == idx = (x+1) : xs
                               | otherwise = x : updateVotos xs (cur_idx+1) idx

hondtAux :: Int -> [Int] -> [Int]-> [Int]
hondtAux 0 xs _ = xs
hondtAux n xs votos = let (val,idx) = maxIndex quocientes in
                        hondtAux (n-1) (updateVotos xs 0 idx) votos
                    where quocientes = zipWith (\v x -> v `div` (1+x) ) votos xs


hondt :: Int -> [Int] -> [Int]
hondt n votos = hondtAux n (replicate (length votos) 0) votos



fromBits :: [Int] -> Int
fromBits list = loop (reverse list) 0 1
    where 
        loop [] res _ = res
        loop (x:xs) res mul = loop xs (res + mul*x) (mul*2)


scalarProduct :: Num a => [a]-> [a]-> a
scalarProduct xs ys = sum [x*y| (x,y)<-zip xs ys ]

infL :: Integral a => [a]
infL = 1 : [2*prev + i -1 | (i,prev) <- zip [2..] infL]

differentFromNext :: String -> String
differentFromNext s = [x | (x,y) <- zip s (tail s), x /= y]

dropN :: [a] -> Int -> [a]
dropN xs n = [val | (i,val)<- zip [1..] xs, i/=n]

repeatNTimes :: Int -> String -> String
repeatNTimes n s = [i | i <- s, j <- [1..n] ]

deleteOne :: Eq a => a -> [a] -> [a]
deleteOne _ [] = []
deleteOne v (x:xs) |  v == x = xs
                   | otherwise = x : deleteOne v xs

mycycle :: [a] -> [a]
mycycle [] = []
mycycle (x:xs) = x : mycycle (xs ++ [x])


myNub :: Eq a => [a]-> [a]
myNub list = loop list []
    where 
        loop [] acc = reverse acc
        loop (x:xs) acc | elem x acc = loop xs acc
                        | otherwise = loop xs (x:acc)



paragraphs :: String -> [String]
paragraphs str = loop str [] [] 0 
    where
        loop [] [] cur_s _ = [cur_s] 
        loop [] res [] _ = reverse res
        loop [] res cur_s _ = reverse (reverse cur_s : res)
        loop (s:sr) res cur_s flag 
            | s == '\n' = if flag == 1 then loop sr (reverse cur_s : res) [] 0 else loop sr res cur_s 1
            | otherwise = if flag == 1 then loop sr res (s:'\n':cur_s) 0 else loop sr res (s:cur_s) 0




myInits :: String -> [String]
myInits [] = [[]]
myInits (x:xs) = [] : map (x:) (myInits xs)

myTranspose :: Eq a => [[a]] -> [[a]]
myTranspose [] = []
myTranspose xss | saferows /= [] = map (head) saferows : myTranspose headless
                | otherwise = []
                where saferows = filter (not.null) xss
                      headless = map tail saferows



data BST a = Empty
 | Node (BST a) a (BST a)
 deriving (Show,Eq)


contains :: Eq a => a-> BST a -> Bool
contains _ Empty = False
contains key (Node left nkey right) = key == nkey || contains key left || contains key right


smallest :: BST a -> Maybe a
smallest Empty = Nothing
smallest (Node Empty val _) = Just val          -- this means we cant go left anymore so the current node is the smallest
smallest (Node left val _) = smallest left      -- continue going left to find the smallest


insert :: Ord a => a -> BST a -> BST a 
insert key Empty = Node Empty key Empty
insert key (Node left nkey right) | key == nkey = Node left nkey right
                                  | key > nkey = Node left nkey (insert key right)
                                  | key < nkey = Node (insert key left) nkey right







bst2List :: BST a-> [a]
bst2List Empty = []
bst2List (Node left key right) = bst2List left ++ [key] ++ bst2List right


isOrdered :: (Ord a) => BST a-> Bool
isOrdered tree = all (\(x,y)->x<y) (zip lista (tail lista)) 
                where
                    lista = bst2List tree


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




fuseDigits :: [Int] -> Int
fuseDigits xs = foldl (\acc x -> acc*10 + x) 0 xs


singleElement :: Int -> Bool
singleElement n = (length (show n)) == 1

separateSingleDigits :: [Int] -> ([Int],[Int])
separateSingleDigits xs = foldl (\(x1,x2) x -> if (singleElement x) then (x1 ++ [x],x2) else (x1, x2 ++ [x])) ([],[]) xs

myPi :: Int -> Double
myPi n = sqrt(12*(sum (take n [ x/y | (x,y)<- zip (cycle [1,-1]) (map (^2) [1..])])))


qSort :: [Int] -> [Int]
qSort [] = []
qSort (x:xs) = qSort smaller ++ [x] ++ qSort bigger
                where
                    smaller = filter (<x) xs
                    bigger = filter (>x) xs


myGroup :: Eq a => [a] -> [[a]]
myGroup [] = []
myGroup xs = loop xs [] []
    where
        loop [] cur_group res = reverse (cur_group : res)
        loop (x:xs) [] res = loop xs [x] res
        loop (x:xs) cur_group res | elem x cur_group = loop xs (x:cur_group) res
                                  | otherwise = loop (x:xs) [] (cur_group : res)