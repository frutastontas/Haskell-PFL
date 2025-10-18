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
