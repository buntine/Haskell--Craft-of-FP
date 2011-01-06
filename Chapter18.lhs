
	Haskell: The Craft of Functional Programming
	Simon Thompson
	(c) Addison-Wesley, 1999.

	Chapter 18

Programming with actions
^^^^^^^^^^^^^^^^^^^^^^^^

>	module Chapter18 where

>	import Prelude hiding (lookup)

>	import IO		-- for isEOF (see note below, aslo)

>	isEOF = hugsIsEOF	-- this should be commented out in later
>				-- versions; it is here because Hugs 1.4
>				-- doesn't support isEOF

The basics of input/output
^^^^^^^^^^^^^^^^^^^^^^^^^^

Reading input is done by getLine and getChar: see Prelude for details.

	getLine :: IO String
	getChar :: IO Char

Text strings are written using 
	
	putStr :: String -> IO ()
	putStrLn :: String -> IO ()

A hello, world program

>	helloWorld :: IO ()
>	helloWorld = putStr "Hello, World!"

Writing values in general

	print :: Show a => a -> IO ()


The do notation: a series of sequencing examples.
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Put a string and newline.

	putStrLn :: String -> IO ()
	putStrLn str = do putStr str
	                  putStr "\n"

Put four times.

>	put4times :: String -> IO ()
>	put4times str 
>	  = do putStrLn str
>	       putStrLn str
>	       putStrLn str
>	       putStrLn str

Put n times

>	putNtimes :: Int -> String -> IO ()
>	putNtimes n str
>	  = if n <= 1 
>	       then putStrLn str
>	       else do putStrLn str
>	               putNtimes (n-1) str

Read two lines, then write a message.

>	read2lines :: IO ()
>	read2lines 
>	  = do getLine
>	       getLine
>	       putStrLn "Two lines read."

Read then write.

>	getNput :: IO ()
>	getNput = do line <- getLine
>	             putStrLn line

Read, process then write.

>	reverse2lines :: IO ()
>	reverse2lines
>	  = do line1 <- getLine
>	       line2 <- getLine
>	       putStrLn (reverse line2)
>	       putStrLn (reverse line1)

Last example redefined to use a local definition.

>	reverse2lines' :: IO ()
>	reverse2lines'
>	  = do line1 <- getLine
>	       line2 <- getLine
>	       let rev1 = reverse line1
>	       let rev2 = reverse line2
>	       putStrLn rev2
>	       putStrLn rev1

Reading an Int.

>	getInt :: IO Int
>	getInt = do line <- getLine
>	            return (read line :: Int) 


Iteration and recursion
^^^^^^^^^^^^^^^^^^^^^^^

A while loop.

>	while :: IO Bool -> IO () -> IO ()

>	while test action
>	  = do res <- test
>	       if res then do action
>	                      while test action
>	              else return ()

Copying input to output.

>	copyInputToOutput :: IO ()
>	copyInputToOutput
>	  = while (do res <- isEOF
>	              return (not res))
>	          (do line <- getLine
>	              putStrLn line)

An important example: refer to the text to see why it fails to work as
required. (The incorrect version is primed.)

>	goUntilEmpty' :: IO ()
>	goUntilEmpty'
>	 = do line <- getLine
>	      while (return (line /= []))
>	            (do putStrLn line
>	                line <- getLine
>	                return ())

The correct program: the key is to think recursively.

>	goUntilEmpty :: IO ()
>	goUntilEmpty
>	  = do line <- getLine
>	       if (line == [])
>	          then return () 
>	          else (do putStrLn line
>	                   goUntilEmpty)


Adding a sequence of integers

>	sumInts :: IO Int

>	sumInts
>	  = do n <- getInt
>	       if n==0 
>	          then return 0
>	          else (do m <- sumInts
>	                   return (n+m))

Addiing a sequence of integers, courteously.

>	sumInteract :: IO ()
>	sumInteract
>	  = do putStrLn "Enter integers one per line"
>	       putStrLn "These will be summed until zero is entered"
>	       sum <- sumInts
>	       putStr "The sum was "
>	       print sum



The calculator
^^^^^^^^^^^^^^

This is available separately.


Input and output as lazy lists
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Reverse all the lines in the input.

>	listIOprog :: String -> String

>	listIOprog = unlines . map reverse . lines



Monads for Functional Programming
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The definition of the Monad class
	class Monad m where
	  (>>=)  :: m a -> (a -> m b) -> m b
	  return :: a -> m a
	  fail   :: String -> m a

Kelisli composition for monadic functions.

(>@>) :: Monad m => (a -> m b) ->
                    (b -> m c) ->
                    (a -> m c)

f >@> g = \ x -> (f x) >>= g


Some examples of monads
^^^^^^^^^^^^^^^^^^^^^^^

Some examples from the standard prelude.

The list monad

	instance Monad [] where
	  xs >>= f  = concat (map f xs)
	  return x  = [x]
	  zero      = []

The Maybe monad

	instance Monad Maybe where
	  (Just x) >>= k  =  k x
	  Nothing  >>= k  =  Nothing
	  return          =  Just

The identity monad

>	data Id a = Id a 

>	instance Monad Id where
>	  return         = Id
>	  (>>=) (Id x) f = f x

The parsing monad

	data SParse a b = SParse (Parse a b)

	instance Monad (SParse a) where
	  return x = SParse (succeed x)
	  zero     = SParse fail
	  (SParse pr) >>= f 
	    = SParse (\s -> concat [ sparse (f x) rest | (x,rest) <- pr st ])

	sparse :: SParse a b -> Parse a b
	sparse (SParse pr) = pr

A state monad (the state need not be a table; this example is designed
to support the example discussed below.)

>	type Table a = [a]

>	data State a b = State (Table a -> (Table a , b))

>	instance Monad (State a) where

>	  return x = State (\tab -> (tab,x))

>	  (State st) >>= f 
>	    = State (\tab -> let 
>	                     (newTab,y)    = st tab
>	                     (State trans) = f y 
>	                     in
>	                     trans newTab)


Example: Monadic computation over trees
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

A type of binary trees.

>	data Tree a = Nil | Node a (Tree a) (Tree a)

Summing a tree of integers

A direct solution:

>	sTree :: Tree Int -> Int

>	sTree Nil            = 0
>	sTree (Node n t1 t2) = n + sTree t1 + sTree t2

A monadic solution: first giving a value of type Id Int ...

>	sumTree :: Tree Int -> Id Int

>	sumTree Nil = return 0

>	sumTree (Node n t1 t2)
>	  = do num <- return n
>	       s1  <- sumTree t1
>	       s2  <- sumTree t2
>	       return (num + s1 + s2)

... then adapted to give an Int solution

>	sTree' :: Tree Int -> Int

>	sTree' = extract . sumTree

where the value is extracted from the Id monad thus:

>	extract :: Id a -> a
>	extract (Id x) = x


Using a state monad in a tree calculation
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The top level function ...

>	numTree :: Eq a => Tree a -> Tree Int

... and the function which does all the work:

>	numberTree :: Eq a => Tree a -> State a (Tree Int)

Its structure mirrors exactly the structure of the earlier program to
sum the tree.

>	numberTree Nil = return Nil

>	numberTree (Node x t1 t2)
>	  = do num <- numberNode x
>	       nt1 <- numberTree t1
>	       nt2 <- numberTree t2
>	       return (Node num nt1 nt2)

The work of the algorithm is done node by node, hence the function

>	numberNode :: Eq a => a -> State a Int

>	numberNode x = State (nNode x)

>	nNode :: Eq a => a -> (Table a -> (Table a , Int))
>	nNode x table
>	  | elem x table        = (table      , lookup x table)
>	  | otherwise           = (table++[x] , length table)
 
Looking up a value in the table; will side-effect the table if the value
is not present.

>	lookup :: Eq a => a -> Table a -> Int

>	lookup = lookup 	-- dummy definition: 
>				-- exercise for the reader

Extracting a value froma state monad.

>	extractSt :: State a b -> b
>	extractSt (State st) = snd (st [])

The top-level function defined eventually.

>	numTree = extractSt . numberTree


