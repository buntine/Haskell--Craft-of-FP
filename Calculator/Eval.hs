
-- 	
-- 	Eval.lhs

-- 	Evaluating expressions and commands

-- 	(c) Simon Thompson, 1998.

module Eval where

import Types
import Store

eval :: Expr -> Store -> Int

eval (Lit n) st = n
eval (Var v) st = value st v
eval (Op op e1 e2) st
  = opValue op v1 v2
    where
    v1 = eval e1 st
    v2 = eval e2 st

opValue :: Ops -> Int -> Int -> Int

opValue Add = (+)
opValue Sub = (-) 
opValue Mul = (*) 
opValue Div = div 
opValue Mod = mod

command :: Command -> Store -> (Int,Store)

command Null st     = (0 , st)
command (Eval e) st = (eval e st , st)
command (Assign v e) st 
  = (val , newSt)
    where
    val   = eval e st
    newSt = update st v val

