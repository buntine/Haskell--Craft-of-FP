>   module Chapter3Exercises where
>   import Hugs.Prelude
>   import Char


Exercise 3.1

>   exOr2 :: Bool -> Bool -> Bool
>   exOr2 True y = not y
>   exOr2 x True = not x
>   exOr2 x y = False


Exercise 3.3

>   exOr3 :: Bool -> Bool -> Bool
>   exOr3 True True = False
>   exOr3 True False = True
>   exOr3 False True = True
>   exOr3 False False = False


Exercise 3.4

>   nAnd1 :: Bool -> Bool -> Bool
>   nAnd1 True True = False
>   nAnd1 x y = True

>   nAnd2 :: Bool -> Bool -> Bool
>   nAnd2 x y = not (x && y)


Exercise 3.6

>   mystery :: Int -> Int -> Int -> Bool
>   mystery a b c = not ((a==b) && (b==c))


Exercise 3.7

>   threeDiff :: Int -> Int -> Int -> Bool
>   threeDiff a b c = (a/=b) && (b/=c) && (a/=c)


Exercise 3.8

>   threeEqual :: Int -> Int -> Int -> Bool
>   threeEqual a b c = (a==b) && (b==c)

>   fourEqual1 :: Int -> Int -> Int -> Int -> Bool
>   fourEqual1 a b c d = (a==b) && (b==c) && (c==d)

>   fourEqual2 :: Int -> Int -> Int -> Int -> Bool
>   fourEqual2 a b c d = (a==b) && (threeEqual b c d)


Exercise 3.11

>   min :: Int -> Int -> Int
>   min x y
>     | x <= y     = x
>     | otherwise  = y

>   minThree :: Int -> Int -> Int -> Int
>   minThree a b c
>     | a <= b && a <= c  = a
>     | b <= c            = b
>     | otherwise         = c


Exercise 3.12

>   offset :: Int
>   offset = ord 'A' - ord 'a'
>   smallToUpper :: Char -> Char
>   smallToUpper ch
>     | (ch >= 'a') && ('z' >= ch)  = chr (ord ch + offset)
>     | otherwise                   = ch


Exercise 3.13

>   charToNum :: Char -> Int
>   charToNum ch
>     | (ch >= '0') && ('9' >= ch)  = ord ch - (ord '0')
>     | otherwise                   = 0


Exercise 3.14

>   averageThree :: Int -> Int -> Int -> Float
>   averageThree a b c = fromInt (a + b + c) / 3

>   numAboveAverage :: Int -> Int -> Int -> Int
>   numAboveAverage a b c
>     | fromInt a > (averageThree a b c) && fromInt b > (averageThree a b c)  = 2
>     | fromInt a > (averageThree a b c) && fromInt c > (averageThree a b c)  = 2
>     | fromInt b > (averageThree a b c) && fromInt c > (averageThree a b c)  = 2
>     | fromInt a > (averageThree a b c)                                      = 1
>     | fromInt b > (averageThree a b c)                                      = 1
>     | fromInt c > (averageThree a b c)                                      = 1


Exercise 3.15

 Equation: a*x^2 + b*x + c == 0.0

>   numNDroots :: Float -> Float -> Float -> Int
>   numNDroots a b c
>     | b^2 > (4.0 * a * c)   = 2
>     | b^2 == (4.0 * a * c)  = 1
>     | otherwise             = 0


Exercise 3.16

>   numDroots :: Float -> Float -> Int
>   numDroots b c
>     | b /= 0.0        = 1
>     | c /= 0.0        = 0
>     | otherwise       = 3

>   numRoots :: Float -> Float -> Float -> Int
>   numRoots a b c
>     | a /= 0     = numNDroots a b c
>     | otherwise  = numDroots b c


Exercise 3.17

>   smallRoot :: Float -> Float -> Float -> Float
>   smallRoot a b c = ((-b) - (sqrt (b^2 - (4.0*a*c)))) / (2*a)
>   largeRoot :: Float -> Float -> Float -> Float
>   largeRoot a b c = ((-b) + (sqrt (b^2 - (4.0*a*c)))) / (2*a)

>   doRoot a b c f
>     | numRoots a b c == 3  = 0
>     | numRoots a b c >= 1  = f a b c
>     | otherwise            = 0

>   smallerRoot :: Float -> Float -> Float -> Float
>   smallerRoot a b c = doRoot a b c smallRoot

>   largerRoot :: Float -> Float -> Float -> Float
>   largerRoot a b c = doRoot a b c largeRoot
