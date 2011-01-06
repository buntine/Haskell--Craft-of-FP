
	Haskell: The Craft of Functional Programming
	Simon Thompson
	(c) Addison-Wesley, 1999.

	Chapter 8


Reasoning about programs
^^^^^^^^^^^^^^^^^^^^^^^^

>	module Chapter8 where

>	import Prelude hiding (sum,length,(++),reverse,unzip)


Testing and verification
^^^^^^^^^^^^^^^^^^^^^^^^
A function supposed to give the maximum of three (integer) values.

>	mysteryMax :: Int -> Int -> Int -> Int
>	mysteryMax x y z
>	  | x > y && x > z      = x
>	  | y > x && y > z      = y
>	  | otherwise           = z

Definedness and termination
^^^^^^^^^^^^^^^^^^^^^^^^^^^

A factorial function, giving an undefined result on negative integers.

>	fact :: Int -> Int
>	fact n
>	  | n==0        = 1
>	  | otherwise   = n * fact (n-1)

An infinite list

>	posInts :: [Int]
>	posInts = [1, 2 .. ]


Induction
^^^^^^^^^

The sum function, defined recursively.

>	sum :: [Int] -> Int

>	sum []     = 0					-- (sum.1)
>	sum (x:xs) = x + sum xs				-- (sum.2)

Double every element of an integer list.

>	doubleAll :: [Int] -> [Int]

>	doubleAll []     = []				-- (doubleAll.1)
>	doubleAll (z:zs) = 2*z : doubleAll zs		-- (doubleAll.2)

The property linking the two:
	sum (doubleAll xs) = 2 * sum xs			-- (sum+dblAll)


Other functions used in the examples
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The definitions given here use explicit recursion, rather than applying 
higher-order functions as may happen in the Prelude definitions.

>	length :: [a] -> Int

>	length []     = 0				-- (length.1)
>	length (z:zs) = 1 + length zs			-- (length.2)
 
>	(++) :: [a] -> [a] -> [a]

>	[]     ++ zs = zs				-- (++.1)
>	(w:ws) ++ zs = w:(ws++zs)			-- (++.2)

>	reverse :: [a] -> [a]

>	reverse []     = []				-- (reverse.1)
>	reverse (z:zs) = reverse zs ++ [z]		-- (reverse.2)

>	unzip :: [(a,b)] -> ([a],[b])

>	unzip [] = ([],[])
>	unzip ((x,y):ps) 
>	  = (x:xs,y:ys)
>	    where
>	    (xs,ys) = unzip ps                   


Generalizing the proof goal
^^^^^^^^^^^^^^^^^^^^^^^^^^^

The shunting function

>	shunt :: [a] -> [a] -> [a]

>	shunt []     ys = ys				-- (shunt.1)
>	shunt (x:xs) ys = shunt xs (x:ys) 		-- (shunt.2)

>	rev :: [a] -> [a]

>	rev xs = shunt xs []				-- (rev.1)

An alternative definition of the factorial function.

>	fac2 :: Int -> Int

>	fac2 n = facAux n 1

>	facAux :: Int -> Int -> Int

>	facAux 0 p = p
>	facAux n p = facAux (n-1) (n*p)
