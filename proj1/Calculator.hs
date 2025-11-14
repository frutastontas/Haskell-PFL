{-
  A basic calculator for arithmetic expressions
  Based on the example in Chapter 8 of "Programming in Haskell"
  by Graham Hutton.

  Pedro Vasconcelos, 2025
-}
module Main where

import Parsing
import Data.Char


--
-- a data type for expressions
-- made up from integer numbers, + and *
--
type Name = String
type Env = [(Name, Integer)]

data Expr = Num Integer
          | Add Expr Expr
          | Mul Expr Expr
          | Sub Expr Expr
          | Mod Expr Expr
          | Div Expr Expr
          | Var Name
          deriving Show

data Command = Assign Name Expr
              | Eval Expr

-- a recursive evaluator for expressions
--
eval :: Env->Expr -> Integer
eval env (Num n) = n
eval env (Add e1 e2) = eval env e1 + eval env e2
eval env (Mul e1 e2) = eval env e1 * eval env e2
eval env (Sub e1 e2) = eval env e1 - eval env e2
eval env (Mod e1 e2) = eval env e1 `mod` eval env e2
eval env (Div e1 e2) = eval env e1 `div` eval env e2
eval env (Var name) = snd(head (filter (\env_var -> fst env_var == name) env))

-- | a parser for expressions
-- Grammar rules:
--
-- expr ::= term exprCont
-- exprCont ::= '+' term exprCont | epsilon

-- term ::= factor termCont
-- termCont ::= '*' factor termCont | epsilon

-- factor ::= natural | '(' expr ')'

command :: Parser Command
command = 
          do 
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
exprCont acc = do char '+'
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
termCont acc =  do char '*'
                   f <- factor  
                   termCont (Mul acc f)
                 <|>
                    do
                      char '/'
                      f <- factor
                      termCont (Div acc f)
                 <|>
                    do
                      char '%'
                      f <- factor
                      termCont (Mod acc f)
                 <|> return acc

factor :: Parser Expr
factor = do n <- natural
            return (Num n)
          <|>
            do
              v <- variable
              return (Var v)
          <|>
          do char '('
             e <- expr
             char ')'
             return e
             

natural :: Parser Integer
natural = do xs <- many1 (satisfy isDigit)
             return (read xs)

variable :: Parser Name
variable = do 
             xs <- many1 (satisfy isLetter)
             return xs

update :: Env -> Name -> Integer -> Env
update env name value = (name,value) : filter (\env_var -> fst env_var /= name) env

----------------------------------------------------------------             
  
main :: IO ()
main
  = do txt <- getContents
       calculator [] (lines txt)

-- | read-eval-print loop
calculator :: Env -> [String] -> IO ()
calculator env []  = return ()
calculator env (l:ls) = do
                       let (output, newEnv) = execute env l
                       putStrLn output
                       calculator newEnv ls  


execute :: Env-> String -> (String, Env)
execute env txt
  = case parse command txt of
      [ (cmd, "") ] -> case cmd of
                          Assign v e -> (show resultado, newEnv)
                                              where
                                                resultado = eval env e
                                                newEnv = update env v resultado
                          Eval e -> (show resultado, env)
                                              where
                                                resultado = eval env e                                                
      _ -> ("parse error; try again",env ) 