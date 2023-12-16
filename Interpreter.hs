module Interpreter where

import Lexer
import Parser

isValue :: Expr -> Bool
isValue BTrue = True
isValue BFalse = True
isValue (Num _) = True
isValue (Lam _ _ _) = True
isValue _ = False

subst :: String -> Expr -> Expr -> Expr
subst x n (Var v) = if x == v then n else Var v
subst x n (Lam v t b) = Lam v t (if x == v then b else subst x n b)
subst x n (App e1 e2) = App (subst x n e1) (subst x n e2)
subst x n (Add e1 e2) = Add (subst x n e1) (subst x n e2)
subst x n (Sub e1 e2) = Sub (subst x n e1) (subst x n e2)
subst x n (Mul e1 e2) = Mul (subst x n e1) (subst x n e2)
subst x n (Or e1 e2) = Or (subst x n e1) (subst x n e2)
subst x n (Not e) = Not (subst x n e)
subst x n (Maior e1 e2) = Maior (subst x n e1) (subst x n e2)
subst x n (MaiorOuIgual e1 e2) = MaiorOuIgual (subst x n e1) (subst x n e2)
subst x n (And e1 e2) = And (subst x n e1) (subst x n e2)
subst x n (If e1 e2 e3) = If (subst x n e1) (subst x n e2) (subst x n e3)
subst x n (Paren e) = Paren (subst x n e)
subst x n (Let v e1 e2) = Let v (subst x n e1) (if x == v then e2 else subst x n e2)
subst x n (For init cond update body) = For (subst x n init) (subst x n cond) (subst x n update) (subst x n body)


-- Padrões adicionados para corresponder às outras expressões
subst x n BTrue = BTrue
subst x n BFalse = BFalse
subst x n (Num num) = Num num
step :: Expr -> Maybe Expr
step (Add (Num n1) (Num n2)) = Just (Num (n1 + n2))
step (Sub (Num n1) (Num n2)) = Just (Num (n1 - n2))
step (Mul (Num n1) (Num n2)) = Just (Num (n1 * n2))
step (Or BTrue _) = Just BTrue
step (Or BFalse e2) = Just e2
step (Maior (Num n1) (Num n2)) = if n1 > n2 then Just BTrue else Just BFalse
step (MaiorOuIgual (Num n1) (Num n2)) = if n1 >= n2 then Just BTrue else Just BFalse
step (Not BTrue) = Just BFalse
step (Not BFalse) = Just BTrue
step (And BFalse _) = Just BFalse
step (And BTrue e) = Just e
step (And e1 e2) = Just (And e1 e2)
step (If BTrue e1 _) = Just e1
step (If BFalse _ e2) = Just e2
step (If e e1 e2) = Just (If e e1 e2)
step (App (Lam x t b) e2) | isValue e2 = Just (subst x e2 b)
                         | otherwise = fmap (\e2' -> App (Lam x t b) e2') (step e2)
step (Let v e1 e2) | isValue e1 = Just (subst v e1 e2)
                   | otherwise = fmap (\e1' -> Let v e1' e2) (step e1)


step (For init cond update body) | isValue init && isValue cond && isValue update && isValue body =
  case eval cond of
    BTrue -> Just (Let "it" init (If cond body BFalse))
    BFalse -> Just BFalse


step (Paren e) = Just e
step _ = Nothing

eval :: Expr -> Expr
eval e | isValue e = e
       | otherwise = case step e of
                       Just e' -> eval e'
                       Nothing -> error ("Evaluation error: " ++ show e)
                       

