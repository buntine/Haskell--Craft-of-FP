
--         UseTree.lhs

-- 	Using the search tree ADT					
--                                                                       
--         (c) Simon Thompson, 1995, 1998.


module UseTree where

import Tree					
--            
-- The size function  definable using the operations of the	
--  	abstype.							
--  

size :: Tree a -> Int
size t 
  | isNil t 	= 0
  | otherwise 	= 1 + size (leftSub t) + size (rightSub t)

--  
-- Finding the nth element of a tree.				
--  

indexT :: Int -> Tree a -> a

indexT n t 
  | isNil t 	= error "indexT"
  | n < st1 	= indexT n t1
  | n == st1 	= v
  | otherwise 	= indexT (n-st1-1) t2
      where
      v   = treeVal t
      t1  = leftSub t
      t2  = rightSub t
      st1 = size t1

