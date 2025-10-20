type Env = [(Name, Bool)]
type Name = Char

data Prop = Const Bool
 | Var Name
 | Not Prop
 | And Prop Prop
 | Imply Prop Prop
 | Or Prop Prop

booleans :: Int-> [[Bool]]
booleans 0 = [[]]
booleans n = [xs ++ [b] | xs <- booleans (n-1), b <- [False,True]]


environments :: [Name]-> [Env]
environments [] = [[]]
environments (x:xs) = [((x,b):env) | env <- environments xs, b <- [False,True]]



eval :: Env -> Prop -> Bool
eval env (Const b) = b
eval env (Var x)
    = case lookup x env of
        Just b -> b
        Nothing -> error "undefined variable"
eval env (Not p) = not (eval env p)
eval env (And p q)
    = eval env p && eval env q
eval env (Imply p q)
    = not (eval env p) || eval env q
eval env (Or p q)
    = eval env p || eval env q


vars :: Prop-> [Name]
vars (Const _) = []
vars (Var x) = [x]
vars (Not p) = vars p
vars (And p q) = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q
vars (Or p q) = vars p ++ vars q



table :: Prop-> [(Env,Bool)]
table prop = [ (env,eval env prop) | env <- (environments (vars prop))]


satisfies :: Prop-> [Env]
satisfies prop = [ (env) | env <- (environments (vars prop)),eval env prop == True ]