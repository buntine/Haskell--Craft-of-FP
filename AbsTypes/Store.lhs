                                                              
	Store.lhs

        An abstract data type of stores of integers, implemented as
        a list of pairs of variables and values.			
                                                                
        (c) Simon Thompson, 1998.					


>       module Store 
>          ( Store, 
>            initial,     -- Store
>            value,       -- Store -> Var -> Int
>            update       -- Store -> Var -> Int -> Store
>           ) where

Var is the type of variables.					

>       type Var = Char

The implementation is given by a newtype declaration, with one
constructor, taking an argument of type [ (Int,Var) ].

>       data Store = Sto [ (Int,Var) ] 

>       instance Eq Store where 
>         (Sto sto1) == (Sto sto2) = (sto1 == sto2)					

>	instance Show Store where
>	  showsPrec n (Sto sto) = showsPrec n sto					
 
>       initial :: Store 

>       initial = Sto []

>       value  :: Store -> Var -> Int

>       value (Sto []) v         = 0
>       value (Sto ((n,w):sto)) v 
>         | v==w            = n
>         | otherwise       = value (Sto sto) v

>       update  :: Store -> Var -> Int -> Store

>       update (Sto sto) v n = Sto ((n,v):sto)

