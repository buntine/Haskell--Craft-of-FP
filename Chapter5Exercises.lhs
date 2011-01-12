>   module Chapter5Exercises where
>   import Hugs.Prelude


Exercise 5.1

>   maxOccurs :: Int -> Int -> (Int, Int)
>   maxOccurs x y
>     | x==y       = (x, 2)
>     | x<y        = (y, 1)
>     | otherwise  = (x, 1)

>   maxThreeOccurs :: Int -> Int -> Int -> (Int, Int)
>   maxThreeOccurs x y z
>     | x==y && y==z  = (x, 3)
>     | x/=y          = maxOccurs (fst (maxOccurs x y)) z
>     | otherwise     = maxOccurs (fst (maxOccurs x z)) y


Exercise 5.2

>   -- wao, between, middleNumber, maxThree, minThree are all helper functions
>   -- that I am reusing from Chapter 3.
>   wao :: Int -> Int -> Int -> Bool
>   wao a b c = (a <= b) && (b <= c)

>   between :: Int -> Int -> Int -> Bool
>   between a b c = wao a b c || wao c b a

>   middleNumber :: Int -> Int -> Int -> Int
>   middleNumber a b c
>     | between b a c  = a
>     | between a b c  = b
>     | otherwise      = c

>   maxThree :: Int -> Int -> Int -> Int
>   maxThree a b c
>     | a >= b && a >= c  = a
>     | b >= c            = b
>     | otherwise         = c

>   minThree :: Int -> Int -> Int -> Int
>   minThree a b c
>     | a <= b && a <= c  = a
>     | b <= c            = b
>     | otherwise         = c

>   orderTriple :: (Int, Int, Int) -> (Int, Int, Int)
>   orderTriple (a, b, c) = (minThree a b c, middleNumber a b c, maxThree a b c)


Exercise 5.3
