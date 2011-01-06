--------------------------------------------------------------------------
--                                                                      --
--	 Tree and a State monad					        --
--                                                                      --
--	(c) Simon Thompson, 1995, 1998.					--
--                                                                      --
--------------------------------------------------------------------------

import Prelude hiding (lookup)

--------------------------------------------------------------------------
--       Type of trees							--
--------------------------------------------------------------------------

data Tree a = Nil | Node a (Tree a) (Tree a) 
              deriving (Eq,Show)

--------------------------------------------------------------------------
--       A state monad							--
--------------------------------------------------------------------------

data State a b = State (Table a -> (Table a , b))

type Table a = [a]

instance Monad (State a) where
  return x = State (\tab -> (tab,x))
  (State st) >>= f 
    = State (\tab -> let 
	          (newTab,y) = st tab
		  (State trans) = f y 
	          in
	          trans newTab)

extract :: State a b -> b

extract (State st) = snd (st [])

--------------------------------------------------------------------------
--	 Assigning unique natural numbers to the members of a tree.	--
--------------------------------------------------------------------------

numTree :: Eq a => Tree a -> Tree Int
numTree = extract . numberTree

numberTree :: Eq a => Tree a -> State a (Tree Int)

numberTree Nil = return Nil

numberTree (Node x t1 t2)
  = do num <- numberNode x
       nt1 <- numberTree t1
       nt2 <- numberTree t2
       return (Node num nt1 nt2)

--------------------------------------------------------------------------
--      Numbering a Node involves a lookup, which in turn will modify 	--
--	the state in case the value is seen for the first time.		--
--------------------------------------------------------------------------

numberNode :: Eq a => a -> State a Int

numberNode x 
  = State (\ table -> if elem x table
                         then (table , lookup x table)
                         else (table++[x] , length table) )

lookup :: Eq a => a -> Table a -> Int

lookup x table = look x table 0

look :: Eq a => a -> Table a -> Int -> Int

look x [] n = error "table lookup"
look x (y:ys) n
  | x==y	= n
  | otherwise	= look x ys (n+1)

--------------------------------------------------------------------------
--	Examples							--
--------------------------------------------------------------------------

example :: Tree Char

example = Node 'z' ex1 ex2

ex1 = Node 'f' ex2 ex2

ex2 = Node 'q' (Node 'z' Nil Nil) (Node 'e' Nil Nil)

data Children = Ahmet | Dweezil | Moon 
		deriving Eq

zapTree :: Tree Children

zapTree = Node Moon (Node Ahmet Nil Nil)
		    (Node Dweezil (Node Ahmet Nil Nil)
				  (Node Moon Nil Nil))

