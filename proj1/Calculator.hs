{-
  A basic calculator for arithmetic expressions
  Based on the example in Chapter 8 of "Programming in Haskell"
  by Graham Hutton.

  Pedro Vasconcelos, 2025
-}
module Main where

import Parsing
import Data.Char
import Data.List

--
-- a data type for expressions
-- made up from integer numbers, + and *
--
type Name = String

type Env = [(Name, Integer)]

data Command = Assign Name Expr
            | Eval Expr

data Expr = Num Integer
          | Add Expr Expr
          | Mul Expr Expr
          | Sub Expr Expr
          | Mod Expr Expr
          | Div Expr Expr
          | Var Name
          deriving Show


getValue :: Env -> Name -> Integer
getValue (x:xs) str
  | str == cur_name = value
  | otherwise = getValue xs str
  where
    (cur_name, value) = x

-- a recursive evaluator for expressions
--
eval :: Env -> Expr -> Integer
eval env (Num n) = n
eval env (Add e1 e2) = eval env e1 + eval env e2 
eval env (Mul e1 e2) = eval env e1 * eval env e2 
eval env (Sub e1 e2) = eval env e1 - eval env e2 
eval env (Mod e1 e2) = eval env e1 `mod` eval env e2 
eval env (Div e1 e2) = eval env e1 `div` eval env e2 
eval env (Var s)     = case lookup s env of
                            Just val -> val
                            Nothing -> 0

-- | a parser for expressions
-- Grammar rules:
--
-- expr ::= term exprCont
-- exprCont ::= '+' term exprCont | epsilon

-- term ::= factor termCont
-- termCont ::= '*' factor termCont | epsilon

-- factor ::= natural | '(' expr ')'


command :: Parser Command
command = do 
            v <- variable
            char '='
            e <- expr
            return (Assign v e)
          <|>
            do
              e <- expr
              return (Eval e)

expr :: Parser Expr
expr = do t <- term
          exprCont t

exprCont :: Expr -> Parser Expr
exprCont acc = do 
                 char '+'
                 t <- term
                 exprCont (Add acc t)
               <|> 
               do 
                 char '-'
                 t <- term
                 exprCont (Sub acc t)
               <|> return acc
              
term :: Parser Expr
term = do f <- factor
          termCont f

termCont :: Expr -> Parser Expr
termCont acc =  do 
                   char '*'
                   f <- factor  
                   termCont (Mul acc f)
                 <|> do 
                        char '/'
                        f <- factor
                        termCont (Div acc f)
                 <|> do 
                        char '%'
                        f <- factor
                        termCont (Mod acc f)
                 <|> return acc

factor :: Parser Expr
factor = do n <- natural
            return (Num n)
          <|>
          do v <- variable
             return (Var v)
          <|>
          do char '('
             e <- expr
             char ')'
             return e

             

natural :: Parser Integer
natural = do xs <- many1 (satisfy isDigit)
             return (read xs)

variable :: Parser String
variable = do 
            xs <- many1 (satisfy isLetter)
            return xs


update :: Env -> Name -> Integer -> Env
update env name value = (name,value) : filter (\assgn -> fst assgn /= name ) env
----------------------------------------------------------------             
  
main :: IO ()
main
  = do txt <- getContents
       calculator [] (lines txt)

-- | read-eval-print loop
calculator :: Env -> [String] -> IO ()
calculator _ []  = return ()
calculator env (l:ls) = do 
                          let (result,newEnv) = execute env l
                          putStrLn result
                          calculator newEnv ls  


execute :: Env->String->(String, Env)
execute env txt
  = case parse command txt of
      [(cmd, "")] -> 
                  case cmd of
                    Assign name expr -> (show resultado, newEnv)
                                        where
                                          resultado = eval env expr
                                          newEnv = update env name resultado
                    Eval expr -> (show resultado, env)
                                  where 
                                    resultado = eval env expr
      _ -> ("parse error; try again" ,env) 