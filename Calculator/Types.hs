

-- 	Types.lhs

-- 	Types for the calculator

-- 	(c) Simon Thompson, 1998.

module Types where

data Expr = Lit Int | Var Var | Op Ops Expr Expr	deriving (Eq,Show)

data Ops  = Add | Sub | Mul | Div | Mod	 		deriving (Eq,Show)

type Var  = Char				

data Command = Eval Expr | Assign Var Expr | Null	deriving (Eq,Show)



