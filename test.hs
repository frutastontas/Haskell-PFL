import Data.Char

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
palavras str = case dropWhile (==' ') str of
                    [] -> []
                    str' -> takeWhile (/=' ') str' : palavras (dropWhile (/=' ') str')


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