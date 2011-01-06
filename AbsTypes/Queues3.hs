
--         Queues3.lhs

--         An abstract data type of queues, implemnted as two lists, with
--         new elements added at the beginning of the second list.						
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

data Queue a = Qu [a] [a]

emptyQ :: Queue a

emptyQ = Qu [] []

isEmptyQ :: Queue a -> Bool

isEmptyQ (Qu [] []) = True
isEmptyQ _          = False

addQ   :: a -> Queue a -> Queue a

addQ x (Qu xs ys) = Qu xs (x:ys)

remQ   :: Queue a -> (  a , Queue a )

remQ (Qu (x:xs) ys)   = (x , Qu xs ys)
remQ (Qu [] ys)       = remQ (Qu (reverse ys) [])
remQ (Qu [] [])       = error "remQ"

