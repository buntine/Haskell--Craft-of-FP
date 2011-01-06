
--         Queues2.lhs

--         An abstract data type of queues, implemnted as a list, with
--         new elements added at the beginning of the list.						
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

addQ x (Qu xs) = Qu (x:xs)

remQ   :: Queue a -> (  a , Queue a )

remQ q@(Qu xs)
  | not (isEmptyQ q)   = (last xs , Qu (init xs))
  | otherwise          = error "remQ"

