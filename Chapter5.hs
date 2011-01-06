-------------------------------------------------------------------------
-- 	Haskell: The Craft of Functional Programming
-- 	Simon Thompson
-- 	(c) Addison-Wesley, 1999.
-- 
-- 	Chapter 5
-------------------------------------------------------------------------

module Chapter5 where

import Prelude hiding (id)

-- Data types: tuples and lists
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^

-- Introducing tuples, lists and strings
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

type ShopItem = (String,Int)
type Basket   = [ShopItem]

basket1 :: Basket
basket1 = [ ("Salt: 1kg",139) , ("Plain crisps",25) , ("Gin: 1lt",1099) ]

basket2 :: Basket
basket2 = []

basket3 :: Basket
basket3 = [ ("Salt: 1kg",139) , ("Plain crisps",25) , ("Plain crisps",25) ]


-- Tuple types
-- ^^^^^^^^^^^

-- Minimum and maximum of two integers.

minAndMax :: Int -> Int -> (Int,Int)
minAndMax x y
  | x>=y        = (y,x)
  | otherwise   = (x,y)

-- Adding a pair of intgers.

addPair :: (Int,Int) -> Int
addPair (x,y) = x+y

-- Shifting around the structure of an ((Int,Int),Int).

shift :: ((Int,Int),Int) -> (Int,(Int,Int))
shift ((x,y),z) = (x,(y,z))

-- Selecting parts of a tuple

name  :: ShopItem -> String
price :: ShopItem -> Int

name  (n,p) = n
price (n,p) = p

-- Adding a pair using the built-in selectors, fst and snd.

addPair' :: (Int,Int) -> Int
addPair' p = fst p + snd p

-- Fibonacci numbers: an efficient function, fastFib.

fibStep :: (Int,Int) -> (Int,Int)
fibStep (u,v) = (v,u+v)

fibPair :: Int -> (Int,Int)
fibPair n
  | n==0        = (0,1)
  | otherwise   = fibStep (fibPair (n-1))

fastFib :: Int -> Int
fastFib = fst . fibPair

fibTwoStep :: Int -> Int -> (Int,Int)
fibTwoStep x y = (y,x+y)



-- Lists in Haskell
-- ^^^^^^^^^^^^^^^^

-- Various examples of lists

list1 :: [Int]
list1 = [1,2,3,4,1,4]

list2 :: [Bool]
list2 = [True]

list3 :: String
list3 = ['a','a','b']

list4 :: String
list4 = "aab"

list5 :: [ Int -> Int ]
list5 = [fastFib,fastFib]

list6  :: [ [Int] ]
list6 = [[12,2],[2,12],[]]

list7 :: [Int]
list7 = [2 .. 7]

list8 :: [Float]
list8 = [3.1 .. 7.0]

list9 :: String
list9 = ['a' .. 'm']

list10 :: [Int]
list10 = [7,6 .. 3]

list11 :: [Float]
list11 = [0.0,0.3 .. 1.0]

list12 :: String
list12 = ['a','c' .. 'n']


-- List comprehensions
-- ^^^^^^^^^^^^^^^^^^^
-- Examples of list comprehensions

ex :: [Int]
ex = [2,4,7]

comp1 :: [Int]
comp1 = [ 2*n | n<-ex]

comp2 :: [Bool]
comp2 = [ isEven n | n<-ex ]

isEven :: Int -> Bool
isEven n = (n `mod` 2 == 0)

comp3 :: [Int]
comp3 = [ 2*n | n <- ex , isEven n , n>3 ]

-- Add all the pairs in a list of pairs.

addPairs :: [(Int,Int)] -> [Int]
addPairs pairList = [ m+n | (m,n) <- pairList ]

-- Return only the sums of pairs which are increasing.

addOrdPairs :: [(Int,Int)] -> [Int]
addOrdPairs pairList = [ m+n | (m,n) <- pairList , m<n ]

-- Return only the digits in a String.

digits :: String -> String
digits st = [ ch | ch<-st , isDigit ch ] 

-- Are all the integers in a list even? or odd?

allEven, allOdd :: [Int] -> Bool
allEven xs = (xs == [x | x<-xs, isEven x])
allOdd xs  = ([] == [x | x<-xs, isEven x])


-- A library database
-- ^^^^^^^^^^^^^^^^^^

-- Types

type Person = String
type Book   = String

type Database = [ (Person , Book) ]

-- An example database.

exampleBase :: Database
exampleBase 
  = [ ("Alice" , "Tintin")  , ("Anna" , "Little Women") ,
      ("Alice" , "Asterix") , ("Rory" , "Tintin") ]

-- The books borrowed by a particular person in the given database.

books       :: Database -> Person -> [Book]
books dBase findPerson
  = [ book | (person,book) <- dBase , person==findPerson ]

-- Making a loan is done by adding a pair to the database.

makeLoan   :: Database -> Person -> Book -> Database
makeLoan dBase pers bk = [ (pers,bk) ] ++ dBase

-- To return a loan.

returnLoan   :: Database -> Person -> Book -> Database
returnLoan dBase pers bk
  = [ pair | pair <- dBase , pair /= (pers,bk) ]

-- Testing the database.

-- Commented out because borrowed is not defined here.

-- test1 :: Bool
-- test1 = borrowed exampleBase "Asterix"

test2 :: Database
test2 = makeLoan exampleBase "Alice" "Rotten Romans"


-- Generic functions: polymorphism
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
 
-- The polymorphic identity function.

id :: a -> a
id x = x

-- A mystery function.

mystery :: (Bool,a) -> Char
mystery (x,y) = if x then 'c' else 'd'


-- The String type
-- ^^^^^^^^^^^^^^^

-- Example strings

str1, str2, str3, str4, str5 :: String

str1 = "baboon"
str2 = ""
str3 = "\99a\116"
str4 = "gorilla\nhippo\nibex"
str5 = "1\t23\t456"

pstr1, pstr2, pstr3, pstr4, pstr5 :: IO ()

pstr1 = putStr str1
pstr2 = putStr str2
pstr3 = putStr str3
pstr4 = putStr str4
pstr5 = putStr str5

