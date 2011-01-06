                                                              
	 UseStoreFun.lhs

         Using an abstract data type StoreFun of stores of integers.			
                                                                
         (c) Simon Thompson, 1998.					


>       module UseStoreFun where

>	import StoreFun

A complex store.

> 	store = update (update (update initial 'a' 4) 'b' 5) 'a' 3

Lookup 'a' in store3; can see that 'a' has the value 3 rather than 4.

>	find  = value store 'a'
