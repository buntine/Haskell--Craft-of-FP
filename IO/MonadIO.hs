--------------------------------------------------------------------------
--                                                                      --
--	FirstEd.hs Using Monads for I/O					--
--	Haskell 1.4 version	  					--
--                                                                      --
--	(c) Simon Thompson, 1995,1998.					--
--                                                                      --
--------------------------------------------------------------------------

import IO	-- not good enough! isEOF not implemented properly 
		-- in Hugs 1.4 yet!

--------------------------------------------------------------------------
--	To read a line							--
--		getLine :: IO String					--
--                                                                      --
--      To write a string						--
--		putStr :: String -> IO ()				--
--									--
--	The `then' operation						--
--		(>>=) :: IO t -> (t -> IO u) -> IO u			--
--------------------------------------------------------------------------

--------------------------------------------------------------------------
--      To write a line                                                 --
--	That is write a String with a newline appended.			--
--------------------------------------------------------------------------

putLine :: String -> IO ()

putLine line = putStr (line ++ "\n")

--------------------------------------------------------------------------
--      Read then write							--
--------------------------------------------------------------------------

readWrite :: IO ()

readWrite = do line <- getLine
               putLine line

--------------------------------------------------------------------------
--      Read, reverse then write					--
--------------------------------------------------------------------------

readRevWrite :: IO ()

readRevWrite
  = do line <- getLine
       putLine (reverse line)

--------------------------------------------------------------------------
--      Return a value without doing any IO				--
--		return :: t -> IO t					--
--									--
--	Sequence without passing values between				--
--		(>>) :: IO t -> IO u -> IO u				--
--		f >> g = f >>= const g					--
--------------------------------------------------------------------------

--------------------------------------------------------------------------
--      Apply a function and return its result				--
--------------------------------------------------------------------------

apply :: (t -> u) -> t -> IO u
apply f a = return (f a)

--------------------------------------------------------------------------
--	Read, reverse then write (revisited)				--
--------------------------------------------------------------------------

readRevWrite' :: IO ()
readRevWrite' = (getLine >>= apply reverse) >>= putLine

--------------------------------------------------------------------------
--      Making a String transformer into an interaction.		--
--		interact :: (String -> String) -> IO ()			--
--------------------------------------------------------------------------

--------------------------------------------------------------------------
--      Iteration							--
--------------------------------------------------------------------------

while :: IO Bool -> IO () -> IO ()

while test oper
  = do res <- test
       if res then do oper
                      while test oper
              else return ()

--------------------------------------------------------------------------
--      Testing for the end of input					--
--		isEOF :: IO Bool					--
--      This version uses the NONSTANDARD hugsIsEOF for IO.hs		--
--	It's not clear how you use this interactively; I remember now   --
--	that C I/O is peculiar!						--
--------------------------------------------------------------------------

isEOF = hugsIsEOF

--------------------------------------------------------------------------
--	Copy lines until end of file.					--
--------------------------------------------------------------------------

copyInputToOutput :: IO ()
copyInputToOutput
  = while (do res <- isEOF
              return (not res))
          (do line <- getLine
              putLine (reverse line))
 

-- reverse lines until you hit an empty line
-- can't see an easy way of building this with 
-- combinators rather than recursion ...
-- ... goUntilEmpty is a failed attempt.

reverseUntilEmpty :: IO ()

reverseUntilEmpty
  = do line <- getLine
       if (line == [])
          then return ()
          else (do putLine (reverse line)
                   reverseUntilEmpty)
            
-- following version doesn't work: 
-- forever outputs the first line read

goUntilEmpty :: IO ()
goUntilEmpty
 = do line <- getLine
      while (return (line /= []))
            (do putLine line
                line <- getLine
                return ())

-- get an integer on a line on its own 
-- not particularly robust: gives an error
--	Program error: PreludeText.read: no parse
-- in case there is anything untoward in the input,
-- including perhaps multiple Ints per line.

getInt :: IO Int

getInt = do line <- getLine
            return (read line :: Int)

-- sum integers until 0 is input

sumInts :: IO Int

sumInts
  = do n <- getInt
       if n==0 
          then return 0
          else (do m <- sumInts
                   return (n+m))

-- an interactive wrapper for sumInts

sumInteract :: IO ()

sumInteract
  = do putLine "Enter integers one per line"
       putLine "These will be summed until zero is entered"
       sum <- sumInts
       putStr "The sum was "
       putLine (show sum)
       
