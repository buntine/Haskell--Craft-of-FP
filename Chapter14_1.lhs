
	Haskell: The Craft of Functional Programming
	Simon Thompson
	(c) Addison-Wesley, 1999.

	Chapter 14, part 1

>	module Chapter14_1 where

>	import Prelude hiding (Either(..),either,Maybe(..),maybe)

Algebraic types
^^^^^^^^^^^^^^^

Introducing algebraic types
^^^^^^^^^^^^^^^^^^^^^^^^^^^

We give a sequence of examples of increasing complexity ...

Enumerated types
^^^^^^^^^^^^^^^^
Two enumerated types

>	data Temp   = Cold | Hot
>	data Season = Spring | Summer | Autumn | Winter

A function over Season, defined using pattern matching.

>	weather :: Season -> Temp

>	weather Summer = Hot
>	weather _      = Cold

The Ordering type, as used in the class Ord.

	data Ordering = LT | EQ | GT

Declaring Temp an instance of Eq.

>	instance Eq Temp where
>	  Cold == Cold  = True
>	  Hot  == Hot   = True
>	  _    == _     = False

Product types
^^^^^^^^^^^^^

A person is represented by their name and age ...

>	data People = Person Name Age

where Name and Age are the appropriate synonyms.

>	type Name = String
>	type Age  = Int

>	jemima, ronnie :: People
>	jemima = Person "Electric Aunt Jemima" 77
>	ronnie = Person "Ronnie" 14

Turning a person into a string.

>	showPerson :: People -> String
>	showPerson (Person st n) = st ++ " -- " ++ show n

An alternative to Age,

>	data NewAge = Years Int


Alternatives
^^^^^^^^^^^^

A shape in a simple geometrical program is either a circle or a
rectangle. These alternatives are given by the type

>	data Shape = Circle Float |
>	             Rectangle Float Float

>	shape1 = Circle 3.0
>	shape2 = Rectangle 45.9 87.6

Pattern matching allows us to define functions by cases, as in,

>	isRound :: Shape -> Bool
>	isRound (Circle _)      = True
>	isRound (Rectangle _ _) = False

and also lets us use the components of the elements:

>	area :: Shape -> Float
>	area (Circle r)      = pi*r*r
>	area (Rectangle h w) = h*w

Derived instances ...

	data Season = Spring | Summer | Autumn | Winter 
	              deriving (Eq,Ord,Enum,Show,Read)


	data Shape  = Circle Float | 
	              Rectangle Float Float 
	              deriving (Eq,Ord,Show,Read)

Recursive algebraic types
^^^^^^^^^^^^^^^^^^^^^^^^^

Expressions
^^^^^^^^^^^

Representing an integer expression.

>	data Expr = Lit Int |
>	            Add Expr Expr |
>	            Sub Expr Expr

Three examples from Expr.

>	expr1 = Lit 2
>	expr2 = Add (Lit 2) (Lit 3)
>	expr3 = Add (Sub (Lit 3) (Lit 1)) (Lit 3)  

Evaluating an expression.

>	eval :: Expr -> Int

>	eval (Lit n)     = n
>	eval (Add e1 e2) = (eval e1) + (eval e2)
>	eval (Sub e1 e2) = (eval e1) - (eval e2)

Showing an expression.

	instance Show Expr where

	  show (Lit n) = show n
	  show (Add e1 e2) 
	    = "(" ++ show e1 ++ "+" ++ show e2 ++ ")"
	  show (Sub e1 e2) 
	    = "(" ++ show e1 ++ "-" ++ show e2 ++ ")"


Trees of integers
^^^^^^^^^^^^^^^^^

The type definition.

>	data NTree = NilT |
>	             NodeT Int NTree NTree

Example trees

>	treeEx1 = NodeT 10 NilT NilT
>	treeEx2 = NodeT 17 (NodeT 14 NilT NilT) (NodeT 20 NilT NilT)

Definitions of many functions are primitive recursive. For instance,

>	sumTree,depth :: NTree -> Int

>	sumTree NilT           = 0
>	sumTree (NodeT n t1 t2) = n + sumTree t1 + sumTree t2

>	depth NilT             = 0
>	depth (NodeT n t1 t2)   = 1 + max (depth t1) (depth t2)

How many times does an integer occur in a tree?

>	occurs :: NTree -> Int -> Int

>	occurs NilT p = 0
>	occurs (NodeT n t1 t2) p
>	  | n==p        = 1 + occurs t1 p + occurs t2 p
>	  | otherwise   =     occurs t1 p + occurs t2 p


Rearranging expressions
^^^^^^^^^^^^^^^^^^^^^^^

Right-associating additions in expressions.

>	assoc :: Expr -> Expr

>	assoc (Add (Add e1 e2) e3)
>	  = assoc (Add e1 (Add e2 e3)) 
>	assoc (Add e1 e2) 
>	  = Add (assoc e1) (assoc e2) 
>	assoc (Sub e1 e2) 
>	  = Sub (assoc e1) (assoc e2)
>	assoc (Lit n) 
>	  = Lit n
 

Infix constructors
^^^^^^^^^^^^^^^^^^

An alternative definition of Expr.

>	data Expr' = Lit' Int |
>	             Expr' :+: Expr' |
>	             Expr' :-: Expr'



Mutual Recursion
^^^^^^^^^^^^^^^^

Mutually recursive types ...

	data Person = Adult Name Address Biog |
	              Child Name
	data Biog   = Parent String [Person] |
	              NonParent String

... and functions.

	showPerson (Adult nm ad bio) 
	  = show nm ++ show ad ++ showBiog bio
	showBiog (Parent st perList)
	  = st ++ concat (map showPerson perList)

Alternative definition of Expr (as used later in the calculator case
study.

data Expr = Lit Int |
            Op Ops Expr Expr

data Ops  = Add | Sub | Mul | Div 

It is possible to extend the type Expr so that it contains
conditional expressions, \texttt{If b e1 e2}.

data Expr = Lit Int |
            Op Ops Expr Expr |
            If BExp Expr Expr

Boolean expressions.

>	data BExp = BoolLit Bool |
>	            And BExp BExp |
>	            Not BExp |
>	            Equal Expr Expr |
>	            Greater Expr Expr





