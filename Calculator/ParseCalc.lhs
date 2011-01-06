

	ParseCalc.lhs

	Parsing expressions and commands

	(c) Simon Thompson, 1998

>	module ParseCalc where

>	import Types
>	import ParseLib

A parser for expressions					
 
 
The parser has three components, corresponding to the three	
clauses in the definition of the syntactic type.		
 
>       parseExpr :: Parse Char Expr
>       parseExpr = (litParse `alt` varParse) `alt` opExpParse
 
Spotting variables.						
 
>       varParse :: Parse Char Expr
>       varParse = spot isVar `build` Var

>       isVar :: Char -> Bool
>       isVar x = ('a' <= x && x <= 'z')
 
Parsing (fully bracketed) operator applications.		
 
>       opExpParse 
>         = (token '(' >*>
>            parseExpr >*>
>            spot isOp >*>
>            parseExpr >*>
>            token ')') 
>            `build` makeExpr

>       makeExpr (_,(e1,(bop,(e2,_)))) = Op (charToOp bop) e1 e2

>       isOp :: Char -> Bool
>       isOp ch = elem ch "+-*/%"

>       charToOp :: Char -> Ops
>       charToOp ch 
>	  = case ch of
>             '+' -> Add
>             '-' -> Sub
>             '*' -> Mul
>             '/' -> Div
>             '%' -> Mod

 
A number is a list of digits with an optional ~ at the front. 
 
>       litParse 
>         = ((optional (token '~')) >*>
>            (neList (spot isDigit)))
>            `build` (charListToExpr.join) 
>            where
>            join = uncurry (++)

Converting strings representing numbers into numbers
 
>       charListToExpr :: [Char] -> Expr
>       charListToExpr = Lit . charListToInt 

>       charListToInt :: [Char] -> Int
>	charListToInt ('~':rest) = - (charListToNat rest)
>	charListToInt other = charListToNat other

>       charListToNat :: [Char] -> Int
>	charListToNat [] = 0
>       charListToNat (ch:rest) 
>         = charToNat ch * 10^(length rest) + charListToNat rest

>	charToNat :: Char -> Int
>	charToNat ch 
>         | ord ch < ord '0' + 10        = ord ch - ord '0'
>         | otherwise                    = ord '0'						

 
The top-level parser						
 
>       topLevel :: Parse a b -> [a] -> b
>       topLevel p inp
>         = case results of
>             [] -> error "parse unsuccessful"
>             _  -> head results
>           where
>           results = [ found | (found,[]) <- p inp ]

A parse for the type of commands.						
 
>       parseCommand :: Parse Char Command
>       parseCommand 
>         = ((parseExpr `build` Eval)
>	    `alt`
>	    (((spot isVar) >*> 
>            (token ':') >*> 
>            parseExpr) `build` makeComm))
>            `alt`
>	     endOfInput Null

>	makeComm (v,(_,e)) = Assign v e

This is the function which gets used in a top-level interaction.....

>	calcLine :: String -> Command

>	calcLine = topLevel parseCommand
 

