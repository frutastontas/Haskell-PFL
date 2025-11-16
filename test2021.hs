import Distribution.Simple.Utils (safeHead, safeTail)
maxpos :: [Int] -> Int
maxpos [] = 0
maxpos xs = loop xs 0 
    where 
        loop [] maximo = maximo
        loop (x:xs) maximo | maximo >= x = loop xs maximo
                           | otherwise = loop xs x


dups :: [a] -> [a]
dups [] = []
dups xs = loop xs 1 -- 1 serves as a kind of iterator
    where 
        loop [] _ = []
        loop (l:ls) itr | odd itr = l:l:loop ls (itr+1)
                        | otherwise = l:loop ls (itr+1)


transforma :: String -> String
transforma [] = []
transforma (x:xs) | elem x "aeiou" = x:'p':x:transforma xs
                  | otherwise = x:transforma xs


type Vector = [Int] 
type Matriz = [[Int]] 

transposta :: Matriz -> Matriz
transposta [] = []
transposta mat = row : transposta retiradas
                where 
                    row = map (head) mat
                    retiradas = filter (\lista -> lista /= []) (map (tail) mat)

prodInterno :: Vector -> Vector -> Int
prodInterno xs ys = sum (zipWith (*) xs ys)

prodMat :: Matriz -> Matriz -> Matriz
prodMat m1 m2 = prodAux m1 (transposta m2)

prodAux :: Matriz -> Matriz -> Matriz
prodAux [] []=[]
prodAux m1 m2 = [ [prodInterno r1 r2 | r2 <- m2]| r1 <- m1]


data Arv a = F | N a (Arv a) (Arv a) 
    deriving(Show,Eq)

calcularAltura :: Arv a -> Int
calcularAltura F = 0
calcularAltura (N _ left right) = 1 + max (calcularAltura left) (calcularAltura right)

alturas :: Arv a -> Arv Int
alturas F = F
alturas (N name left right) = N (calcularAltura no) (alturas left) (alturas right)
                        where no = (N name left right)

equilibrada :: Arv a -> Bool
equilibrada F = True
equilibrada arvore = equilibradaAux(alturas arvore)

equilibradaAux :: Arv Int -> Bool
equilibradaAux F = True
equilibradaAux (N a F F) = True 
equilibradaAux (N _ (N a1 la1 ra1) F) = a1 < 1
equilibradaAux (N _ F (N b1 lb1 rb1)) = b1 < 2
equilibradaAux (N _ (N a1 la1 ra1) (N b1 lb1 rb1)) = (abs(a1 - b1) < 2) && equilibradaAux (N a1 la1 ra1) && equilibradaAux (N b1 lb1 rb1)

f :: (a -> b -> c) -> b -> a -> c 
f func x1 x2 = func x2 x1