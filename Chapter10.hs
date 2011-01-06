-----------------------------------------------------------------------
-- 	Haskell: The Craft of Functional Programming
-- 	Simon Thompson
-- 	(c) Addison-Wesley, 1999.
-- 
-- 	Chapter 10
-----------------------------------------------------------------------



-- Functions as values
-- ^^^^^^^^^^^^^^^^^^^

module Chapter10 where

import Prelude hiding (succ,curry,uncurry,flip)
import Pictures hiding (flipH,rotate,flipV,sideBySide,invertColour,
			superimpose,printPicture)
import Chapter9 hiding (concat,map,filter,zipWith,and,foldr1,foldr,
			doubleAll,flipV,sideBySide,getWord)
import qualified Chapter7

-- A fixity declaration for the forward composition operator, >.>

infixl 9 >.>

-- Function-level definitions
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^
-- Revisiting the Picture example

rotate :: Picture -> Picture
rotate = flipV . flipH

flipH :: Picture -> Picture
flipH = reverse


-- Function composition and forward composition
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

-- A composition operator taking its arguments in the opposite order to `.'.


(>.>) :: (a -> b) -> (b -> c) -> (a -> c)

g >.> f = f . g

-- Another definition of rotate using >.>

rotate' = flipH >.> flipV  


-- Functions as values and results
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

-- Compose a function with itself: apply it twice, in other words.

twice :: (a -> a) -> (a -> a)
twice f = (f . f)

succ :: Int -> Int
succ n = n+1

-- We can generalize twice so that we pass a parameter giving the number
-- of times the functional argument is to be composed with itself:

iter :: Int -> (a -> a) -> (a -> a)

iter n f 
  | n>0         = f . iter (n-1) f
  | otherwise   = id

-- An alternative definition of iter:

iter' n f = foldr (.) id (replicate n f)


-- Expressions defining functions
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

-- A function returning a function, namely the function to `add n to its
-- argument'.

addNum :: Int -> (Int -> Int)
addNum n = addN
           where
           addN m = n+m

-- Lambda notation
-- ^^^^^^^^^^^^^^^

-- An alternative definition of addNum using a lambda expression.

addNum' :: Int -> (Int -> Int)

addNum' n = (\m -> n+m)

-- The `plumbing' function:

comp2 :: (a -> b) -> (b -> b -> c) -> (a -> a -> c)

comp2 f g = (\x y -> g (f x) (f y))

-- Using the `plumbing' function

plumbingExample = comp2 sq add 3 4
		  where
		  sq x    = x*x
		  add y z = y+z

 
-- Partial Application
-- ^^^^^^^^^^^^^^^^^^^

-- The function multiply multiplies together two arguments.

multiply :: Int -> Int -> Int
multiply x y = x*y

-- Double all elements of an integer list.

doubleAll :: [Int] -> [Int]
doubleAll = map (multiply 2)

-- Another definition of addNum, using partial application to achieve the
-- `function as result'.

addNum'' n m = n+m

-- Revisiting the Pictures example, yet again.

flipV :: Picture -> Picture
flipV      = map reverse

sideBySide :: Picture -> Picture -> Picture
sideBySide = zipWith (++)

-- An example function of type (Int -> Int) -> Int

g :: (Int -> Int) -> Int
g h = (h 0) + (h 1)


-- How many arguments do functions have?
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

-- Three examples from the text processing functions first seen in Chapter 7.

dropSpace = dropWhile (member whitespace)
dropWord  = dropWhile (not . member whitespace)
getWord   = takeWhile (not . member whitespace)

-- Auxiliary definitions ...
 
member xs x = elem x xs

-- Operator  Sections
-- ^^^^^^^^^^^^^^^^^

-- Example of a function defined using partial application and operator sections.

egFun :: [Int] -> [Int]

egFun = filter (>0) . map (+1)


-- Revisiting the Picture example
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

-- Some of the functions are already (re)defined in this script.
-- Among the other functions mentioned were 

invertColour :: Picture -> Picture
invertColour = map (map invert)

superimpose  :: Picture -> Picture -> Picture
superimpose = zipWith (zipWith combineChar)

-- The definition of combineChar is left as an exercise: it's a dummy definition
-- here.

combineChar :: Char -> Char -> Char
combineChar = combineChar

-- Printing a picture: uses putStr after a newline has been added at the end of
-- every line and the lines are joined into a single string.

printPicture :: Picture -> IO ()
printPicture = putStr . concat . map (++"\n")


-- Further examples
-- ^^^^^^^^^^^^^^^^
-- Revisiting earlier examples ...

-- Double all integers in a list,

doubleAll' :: [Int] -> [Int]
doubleAll' = map (*2)

-- get the even numbers in a list of integers,

getEvens :: [Int] -> [Int]
getEvens = filter ((==0).(`mod` 2))

-- get a word from the start of a string.

getWord' = getUntil (`elem` whitespace)
 

-- Currying and uncurrying
-- ^^^^^^^^^^^^^^^^^^^^^^^

-- An uncurried function to multiply together the two itegers in a pair.

multiplyUC :: (Int,Int) -> Int
multiplyUC (x,y) = x*y

-- Turn an uncurried function into a curried version,

curry :: ((a,b) -> c) -> (a -> b -> c)
curry g x y = g (x,y)

-- and vice versa.

uncurry :: (a -> b -> c) -> ((a,b) -> c)
uncurry f (x,y) = f x y

-- Change the order of arguments of a two argument curried function.

flip :: (a -> b -> c) -> (b -> a -> c)
flip f x y = f y x


-- Example: creating an index
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^

-- The basic type symonyms

type Doc  = String
type Line = String
type Word = String

-- The type of the top-level function

makeIndex :: Doc -> [ ([Int],Word) ]

-- The top-level definition

makeIndex
  = lines       >.>     --   Doc            -> [Line]
    numLines    >.>     --   [Line]         -> [(Int,Line)] 
    allNumWords >.>     --   [(Int,Line)]   -> [(Int,Word)]
    sortLs      >.>     --   [(Int,Word)]   -> [(Int,Word)]
    makeLists   >.>     --   [(Int,Word)]   -> [([Int],Word)]
    amalgamate  >.>     --   [([Int],Word)] -> [([Int],Word)]
    shorten             --   [([Int],Word)] -> [([Int],Word)]

-- Implementing the component functions
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
 
-- Attach a number to each line.

numLines :: [Line] -> [ ( Int , Line ) ]
numLines linels
  = zip [1 .. length linels] linels

-- Associate each word with a line number

numWords :: ( Int , Line ) -> [ ( Int , Word ) ]

numWords (number , line)
  = [ (number , word) | word <- Chapter7.splitWords line ]

-- The definition uses splitWords from Chapter 7, modified to use a different
-- version of whitespace. For this to take effect, need to make the modification
-- in the Chapter7.lhs file.

whitespace :: String
whitespace = " \n\t;:.,\'\"!?()-"

-- Apply numWords to each integer,line pair.

allNumWords :: [ ( Int , Line ) ] -> [ ( Int , Word ) ]
allNumWords = concat . map numWords

-- The list must next be
-- sorted by word order, and lists of lines on which a word appears be built.
-- The ordering relation on pairs of numbers and 
-- words is given by

orderPair :: ( Int , Word ) -> ( Int , Word ) -> Bool
orderPair ( n1 , w1 ) ( n2 , w2 )
  = w1 < w2 || ( w1 == w2 && n1 < n2 )

-- Sorting the list using the orderPair ordering on pairs.

sortLs :: [ ( Int , Word ) ] -> [ ( Int , Word ) ]

sortLs []     = []
sortLs (p:ps)
  = sortLs smaller ++ [p] ++ sortLs larger
    where
    smaller = [ q | q<-ps , orderPair q p ]
    larger  = [ q | q<-ps , orderPair p q ]

-- The entries for the same word need to be accumulated together.
-- First each entry is converted to having a list of line numbers associated with
-- it, thus

makeLists ::  [ (Int,Word) ] -> [ ([Int],Word) ]
makeLists 
  = map mklis 
    where
    mklis ( n , st ) = ( [n] , st )

-- After this, the lists associated with the same words are amalgamated.

amalgamate :: [ ([Int],Word) ] -> [ ([Int],Word) ]

amalgamate [] = []
amalgamate [p] = [p]
amalgamate ((l1,w1):(l2,w2):rest)
  | w1 /= w2    = (l1,w1) : amalgamate ((l2,w2):rest)
  | otherwise   = amalgamate ((l1++l2,w1):rest)

-- Remove all the short words.

shorten :: [([Int],Word)] -> [([Int],Word)]

shorten 
  = filter sizer 
    where
    sizer (nl,wd) = length wd > 3


-- Verification and general functions
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

-- All the functions used in this section have been defined earlier.

