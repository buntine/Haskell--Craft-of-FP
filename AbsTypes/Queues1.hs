
--         Queues1.lhs

--         An abstract data type of queues, implemented as a list, with
--         new elements added at the end of the list.						
--                                                                       
--         (c) Simon Thompson, 1998.					
--                                                                       

module Queue 
  ( Queue , 
    emptyQ ,       --  Queue a
    isEmptyQ ,     --  Queue a -> Bool 
    addQ ,         --  a -> Queue a -> Queue a
    remQ           --  Queue a -> (  a , Queue a )
   ) where 

newtype Queue a = Qu [a]
--  
emptyQ :: Queue a

emptyQ = Qu []

isEmptyQ :: Queue a -> Bool

isEmptyQ (Qu []) = True
isEmptyQ _       = False

addQ   :: a -> Queue a -> Queue a

addQ x (Qu xs) = Qu (xs++[x])

remQ   :: Queue a -> (  a , Queue a )

remQ q@(Qu xs)
  | not (isEmptyQ q)   = (head xs , Qu (tail xs))
  | otherwise          = error "remQ"

