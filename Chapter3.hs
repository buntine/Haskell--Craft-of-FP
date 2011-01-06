------------------------------------------------------------------------------
--
--	Haskell: The Craft of Functional Programming
--	Simon Thompson
--	(c) Addison-Wesley, 1999.
--
--	Chapter 3
--
------------------------------------------------------------------------------

module Chapter3 where

-- The import statement which follows hides certain of the Prelude functions
-- so that they can be given the definitions they have in their book.

import Prelude hiding (max,toUpper,isDigit)


-- The Booleans.
-- ^^^^^^^^^^^^^

-- Exclusive or: this gives the result True if one of its arguments is True and
-- the other False, and gives the result False in other cases.

exOr :: Bool -> Bool -> Bool
exOr x y = (x || y) && not (x && y)

-- Using literals instead of variables in a definition; a simple example of
-- pattern matching to give another definition of `not', ...

myNot :: Bool -> Bool
myNot True  = False
myNot False = True

-- ... and of `exclusive or'.

exOr1 True  x = not x
exOr1 False x = x


-- Integers and guards.
-- ^^^^^^^^^^^^^^^^^^^^

-- A to test whether three Ints are equal.

threeEqual :: Int -> Int -> Int -> Bool
threeEqual m n p = (m==n) && (n==p)

-- The maximum of two integers; this is already defined in the Prelude, 
-- so its definition is hidden by the import statement at the top of this file.

max :: Int -> Int -> Int
max x y
  | x >= y      = x
  | otherwise   = y

-- The maximum of three integers.

maxThree :: Int -> Int -> Int -> Int
maxThree x y z
  | x >= y && x >= z    = x
  | y >= z              = y
  | otherwise           = z

-- An alternative definition of max which uses if ... then ... else ...

max' :: Int -> Int -> Int
max' x y
  = if x >= y then x else y


-- Characters.
-- ^^^^^^^^^^^

-- Converting lower-case letters to upper-case; does something odd if you apply
-- it to anythig else: how would you modify it to return anything else
-- unchanged?
 
toUpper :: Char -> Char
toUpper ch = chr (ord ch + offset)

offset = ord 'A' - ord 'a'

-- A check whether a character is a digit (already defined in the Prelude)

isDigit :: Char -> Bool
isDigit ch = ('0' <= ch) && (ch <= '9')


-- Some syntax.
-- ^^^^^^^^^^^^

-- Layout: two definitions on one line, separated by a `;'.

answer = 42 ;   facSix = 720 

-- Adding two integers: you can use longer names for variables than x and y!

addTwo :: Int -> Int -> Int
addTwo first second = first+second

-- Defining an operators for yourself: another version of max!

(&&&) :: Int -> Int -> Int
x &&& y 
  | x > y       = y
  | otherwise   = x
