--------------------------------------------------------------------------
-- 	Haskell: The Craft of Functional Programming
-- 	Simon Thompson
-- 	(c) Addison-Wesley, 1999.
-- 
-- 	Chapter 4
--------------------------------------------------------------------------

module Chapter4 where

-- Designing a program in Haskell
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

maxThree :: Int -> Int -> Int -> Int
maxThree x y z = (x `max` y) `max` z

middleNumber :: Int -> Int -> Int -> Int
middleNumber x y z
  | between y x z      = x
  | between x y z      = y
  | otherwise          = z

-- What follows here is a dummy definition of between; you need to replace this
-- with a proper definition for the function middleNumber to work.

between ::  Int -> Int -> Int -> Bool
between = between

-- Primitive recursion over Int
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^

-- The factorial of n is 1*2*...*(n-1)*n, so that factorial of four is 24.
-- It is often written n!

fac :: Int -> Int
fac n
  | n==0        = 1
  | n>0         = fac (n-1) * n
  | otherwise   = error "fac only defined on natural numbers"

--                                      n
-- Raising two to a power: power2 n is 2  in mathematical notation.

power2 :: Int -> Int
power2 n
  | n==0        = 1
  | n>0         = 2 * power2 (n-1)

-- The sum of the factorials up to a particular value, 0! + 1! + ... n!.

sumFacs :: Int -> Int
sumFacs n
  | n==0        = 1
  | n>0         = sumFacs (n-1) + fac n  

-- The sum of the values of a function up to a particular value: 
-- 	f 0 + f 1 + ... f n
-- from which you can reconstruct sumFacs: sumFacs n = sumFun fac n

sumFun :: (Int -> Int) -> Int -> Int
sumFun f n
  | n==0        = f 0
  | n>0         = sumFun f (n-1) + f n  

-- The maximum number of regions into which n lines can cut a plane.

regions :: Int -> Int 
regions n
  | n==0        = 1
  | n>0         = regions (n-1) + n

-- The Fibonacci numbers 0, 1, 1, 2, 3, 5, ..., u, v, u+v, ...

fib :: Int -> Int
fib n 
  | n==0        = 0
  | n==1        = 1
  | n>1         = fib (n-2) + fib (n-1)

-- Division of integers

remainder :: Int -> Int -> Int
remainder m n 
  | m<n         = m
  | otherwise   = remainder (m-n) n

divide    :: Int -> Int -> Int
divide m n
  | m<n         = 0
  | otherwise   = 1 + divide (m-n) n

-- Testing
-- ^^^^^^^

-- Does this function calculate the maximum of three numbers?

mysteryMax :: Int -> Int -> Int -> Int
mysteryMax x y z
  | x > y && x > z      = x
  | y > x && y > z      = y
  | otherwise           = z
