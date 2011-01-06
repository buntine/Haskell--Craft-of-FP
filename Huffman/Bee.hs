module Bee where 

import Ant hiding ( anteater )
	   renaming ( aardvark to honeyEater )

beekeeper y = honeyEater y + 1


--------------------------------------------------------------------------
--	Note that renaming has been removed in Haskell 1.3; it can	--
--	be simulated by importing in qualified form and then naming	--
--	thus								--
--									--
--		honeyEater = Ant.aardvark				--
--------------------------------------------------------------------------
