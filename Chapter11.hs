-----------------------------------------------------------------------
--	Haskell: The Craft of Functional Programming
--	Simon Thompson
--	(c) Addison-Wesley, 1999.
--
--	Chapter 11
-----------------------------------------------------------------------

module Chapter11 where

-- Program development
-- ^^^^^^^^^^^^^^^^^^^

-- Development in practice
-- ^^^^^^^^^^^^^^^^^^^^^^^
-- Defining the .. notation (not executable code).
-- 
-- [m .. n]
--   | m>n         = []
--   | otherwise   = m : [m+1 .. n]

-- [1 .. n] 
--   | 1>n         = []
--   | otherwise   = [1 .. n-1] ++ [n]

-- A simple palindrome check.

simplePalCheck :: String -> Bool
simplePalCheck st = (reverse st == st)

-- The full check

palCheck = simplePalCheck . clean

-- where the clean function combines mapping (capitals to smalls) and
-- filtering (removing punctuation)

clean :: String -> String 

clean = map toSmall . filter notPunct

toSmall  = toSmall	-- dummy definition
notPunct = notPunct	-- dummy definition

-- Auxiliary functions
-- ^^^^^^^^^^^^^^^^^^^

-- When is one string a subsequence of another? 

subseq :: String -> String -> Bool

subseq []    _  = True
subseq (_:_) [] = False
subseq (x:xs) (y:ys)
  = subseq (x:xs) ys || frontseq (x:xs) (y:ys)

-- When is one strong a subsequece of another, starting at the front?

frontseq :: String -> String -> Bool
frontseq []     _  = True
frontseq (_:_)  [] = False
frontseq (x:xs) (y:ys)
  = (x==y) && frontseq xs ys

