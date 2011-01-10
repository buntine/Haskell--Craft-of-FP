>   module Chapter4Exercises where
>   import Hugs.Prelude


Exercise 4.1

>   maxThree :: Int -> Int -> Int -> Int
>   maxThree a b c
>     | a >= b && a >= c  = a
>     | b >= c            = b
>     | otherwise         = c

This one sucks because it's verbose and requires two invokations
of 'maxThree'.

>   maxFour1 :: Int -> Int -> Int -> Int -> Int
>   maxFour1 a b c d 
>     | a >= maxThree b c d  = a
>     | otherwise            = maxThree b c d

This one is OK, but requires an additional call to 'max' because
it does make use of the predefined function 'maxThree'.

>   maxFour2 :: Int -> Int -> Int -> Int -> Int
>   maxFour2 a b c d = max (max (max a b) c) d

This is the best one because it is concise and uses existing
functions in it's definition.

>   maxFour3 :: Int -> Int -> Int -> Int -> Int
>   maxFour3 a b c d = max (maxThree a b c) d


Exercise 4.2

>   wao :: Int -> Int -> Int -> Bool   -- aka: weakAscendingOrder
>   wao a b c = (a <= b) && (b <= c)

>   between :: Int -> Int -> Int -> Bool
>   between a b c = wao a b c || wao c b a

>   middleNumber :: Int -> Int -> Int -> Int
>   middleNumber a b c
>     | between b a c  = a
>     | between a b c  = b
>     | otherwise      = c


Exercise 4.3

>   -- From Exercise 3.8
>   threeEqual :: Int -> Int -> Int -> Bool
>   threeEqual a b c = (a==b) && (b==c)

>   -- From Exercise 3.7
>   threeDiff :: Int -> Int -> Int -> Bool
>   threeDiff a b c = (a/=b) && (b/=c) && (a/=c)

>   howManyEqual :: Int -> Int -> Int -> Int
>   howManyEqual a b c
>     | threeEqual a b c  = 3
>     | threeDiff a b c   = 0
>     | otherwise         = 2


Exercise 4.4

>   fourEqual :: Int -> Int -> Int -> Int -> Bool
>   fourEqual a b c d = (a==b) && (threeEqual b c d)

>   fourDiff :: Int -> Int -> Int -> Int -> Bool
>   fourDiff a b c d = (a/=b) && (a/=c) && (a/=d) && (threeDiff b c d)

>   threeOfFourEqual :: Int -> Int -> Int -> Int -> Bool
>   threeOfFourEqual a b c d = (threeEqual a b c) || (threeEqual a b d) ||
>                              (threeEqual a c d) || (threeEqual b c d)

>   howManyOfFourEqual :: Int -> Int -> Int -> Int -> Int
>   howManyOfFourEqual a b c d
>     | fourEqual a b c d         = 4
>     | fourDiff a b c d          = 0
>     | threeOfFourEqual a b c d  = 3
>     | otherwise                 = 2


Exercise 4.5

>   rangeProduct :: Int -> Int -> Int
>   rangeProduct a b
>     | a < b      = a * rangeProduct (a + 1) b
>     | a == b     = a
>     | otherwise  = 0


Exercise 4.6

>   fac :: Int -> Int
>   fac n = rangeProduct 1 n


Exercise 4.7

>   mult :: Int -> Int -> Int
>   mult 0 y = 0
>   mult x y = mult (x - 1) y + y


Exercise 4.8

>   findroot :: Int -> Int -> Int
>   findroot n s
>     | s^2 > n    = s - 1
>     | otherwise  = findroot n (s + 1)

>   intsqrt :: Int -> Int
>   intsqrt n = findroot n 1


Exercise 4.9

>   farg :: Int -> Int
>   farg 0 = 0
>   farg 1 = 16
>   farg 2 = 22
>   farg 3 = 765
>   farg 4 = 87
>   farg 5 = 987
>   farg _ = 0

>   pickMax :: Int -> Int
>   pickMax 0 = farg 0
>   pickMax n = max (farg n) (pickMax (n - 1))


Exercise 4.10

>   forg :: Int -> Int
>   forg 0 = 1
>   forg 1 = 2
>   forg 2 = 3
>   forg 3 = 4
>   forg 4 = 0
>   forg 5 = 6
>   forg _ = 0

>   anyZero :: Int -> Bool
>   anyZero 0 = forg 0 == 0
>   anyZero n = forg n == 0 || anyZero (n - 1)


Exercise 4.11

>   sumFun :: (Int -> Int) -> Int -> Int
>   sumFun f n
>     | n==0  = f 0
>     | n>0   = sumFun f (n - 1) + f n

>   regions :: Int -> Int
>   regions n = sumFun (\x -> x) n


Exercise 4.13

>   hcf :: Int -> Int -> Int
>   hcf x 0 = x
>   hcf x y = hcf y (rem x y)
