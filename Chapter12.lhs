
	Haskell: The Craft of Functional Programming
	Simon Thompson
	(c) Addison-Wesley, 1999.

	Chapter 12


>	module Chapter12 where

Overloading and type classes
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Why overloading?
^^^^^^^^^^^^^^^^

Testing for membership of a Boolean list.

>	elemBool :: Bool -> [Bool] -> Bool

>	elemBool x [] = False
>	elemBool x (y:ys)
>	  = (x == y) || elemBool x ys

Testing for membership of a general list, with the equality function as a
parameter.

>	elemGen :: (a -> a -> Bool) -> a -> [a] -> Bool

>	elemGen eqFun x [] = False
>	elemGen eqFun x (y:ys)
>	  = (eqFun x y) || elemGen eqFun x ys


Introducing classes
^^^^^^^^^^^^^^^^^^^

Definitions of classes cannot be hidden, so the definitions etc. here are not
executable.

class Eq a where
  (==) :: a -> a -> Bool

Functions which use equality
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Testing for three values equal: more general than Int -> Int -> Int -> Bool.

>	allEqual :: Eq a => a -> a -> a -> Bool
>	allEqual m n p = (m==n) && (n==p)

elem :: Eq a => a -> [a] -> Bool
books :: Eq a => [ (a,b) ] -> a -> [b]

It is easier to see this typing if you remane books lookupFirst:

>	lookupFirst :: Eq a => [ (a,b) ] -> a -> [b]

>	lookupFirst ws x 
>	  = [ z | (y,z) <- ws , y==x ]

borrowed    :: Eq b => [ (a,b) ] -> b -> Bool
numBorrowed :: Eq a => [ (a,b) ] -> a -> Int


Signatures and Instances
^^^^^^^^^^^^^^^^^^^^^^^^

The Visible class:

>	class Visible a where
>	  toString :: a -> String
>	  size     :: a -> Int

A type is made a member or instance of a class by defining
the signature functions for the type. For example,

instance Eq Bool where
  True  == True  = True
  False == False = True
  _     == _     = False

Declaring examples of the Visible class

>	instance Visible Char where
>	  toString ch  = [ch]
>	  size _       = 1

>	instance Visible Bool where
>	  toString True  = "True"
>	  toString False = "False"
>	  size _         = 1

An instance declaration with a context.

>	instance Visible a => Visible [a] where
>	  toString = concat . map toString  
>	  size     = foldr (+) 1 . map size  


Default definitions
^^^^^^^^^^^^^^^^^^^

To return to our example of equality, the Haskell equality class is in fact
defined by

class Eq a where
  (==), (/=) :: a -> a -> Bool
  x /= y     = not (x==y)
  x == y     = not (x/=y)


Derived classes
^^^^^^^^^^^^^^^

Ordering is built on Eq.

class Eq a => Ord a where
  (<), (<=), (>), (>=) :: a -> a -> Bool
  max, min             :: a -> a -> a
  compare              :: a -> a -> Ordering


This is the same definition as in Chapter7, but now with an overloaded type.

>	iSort :: Ord a => [a] -> [a]

>	iSort []	= []
>	iSort (x:xs) = ins x (iSort xs)

To insert an element at the right place into a sorted list.

>	ins :: Ord a => a -> [a] -> [a]

>	ins x []    = [x]
>	ins x (y:ys)
>	  | x <= y	= x:(y:ys)
>	  | otherwise	= y : ins x ys


Multiple constraints
^^^^^^^^^^^^^^^^^^^^

Sorting visible objects ...

>	vSort :: (Ord a,Visible a) => [a] -> String

>	vSort = toString . iSort 

Similarly, 

>	vLookupFirst :: (Eq a,Visible b) => [(a,b)] -> a -> String

>	vLookupFirst xs x = toString (lookupFirst xs x)

Multiple constraints can occur in an instance declaration, such as

instance (Eq a,Eq b) => Eq (a,b) where
  (x,y) == (z,w)  =  x==z && y==w

Multiple constraints can also occur in the definition of a class,

>	class (Ord a,Visible a) => OrdVis a

Can then give vSort the type:

	vSort :: OrdVis a => [a] -> String

A tour of the built-in Haskell classes
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

For details of the code here, please see the standard Prelude and Libraries.


Types and Classes
^^^^^^^^^^^^^^^^^

The code in this section is not legal Haskell.

To evaluate the type of concat . map show, type

	:type concat . map show

to the Hugs prompt.
