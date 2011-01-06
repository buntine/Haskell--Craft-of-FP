                                                              
	 StoreFun.lhs

         An abstract data type of stores of integers, implemented as functions.			
                                                                
         (c) Simon Thompson, 1998.					


An alternative implementation of Store.lhs. Note that although
it is equivalent to the list implementation as far as the operations
initial, value, update are concerned, it is not possible to compare for
equality or to show as a String.

>       module StoreFun 
>          ( Store, 
>            initial,     -- Store
>            value,       -- Store -> Var -> Int
>            update       -- Store -> Var -> Int -> Store
>           ) where

Var is the type of variables.					

>       type Var = Char

>       newtype Store = Sto (Var -> Int) 					
 
>       initial :: Store 

>       initial = Sto (\v -> 0)

>       value :: Store -> Var -> Int

>       value (Sto sto) v = sto v

>       update  :: Store -> Var -> Int -> Store

>       update (Sto sto) v n 
>         = Sto (\w -> if v==w then n else sto w)

