>   module Chapter9Exercises where
>   import Prelude
>   import Char


Exercise 9.1

>   doubleAlla :: [Int] -> [Int]
>   doubleAlla lst = [n*2 | n<-lst]

>   doubleAllb :: [Int] -> [Int]
>   doubleAllb []     = []
>   doubleAllb (x:xs) = x*2 : (doubleAllb xs)

>   double :: Int -> Int
>   double x = x * 2

>   doubleAllc :: [Int] -> [Int]
>   doubleAllc lst = map double lst


Exercise 9.2

>   length2 :: [a] -> Int
>   length2 lst = sum (map (\n -> 1) lst)


Exercise 9.6

>   squares :: [Int] -> [Int]
>   squares ns = map (\n -> n*n) ns

>   sumsquares :: [Int] -> Int
>   sumsquares ns = sum (squares ns)

>   allgreaterzero :: [Int] -> Bool
>   allgreaterzero ns = and (map (\n -> n > 0) ns)


Exercise 9.7

>   minf :: (Int -> Int) -> Int -> Int
>   minf f 0 = f 0
>   minf f n = min (f n) (minf f (n - 1))

>   valsf :: (Int -> Int) -> Int -> [Int]
>   valsf f (-1) = []
>   valsf f n    = (f n) : valsf f (n - 1)

>   allf :: (Int -> Int) -> Int -> Bool
>   allf f 0 = True
>   allf f n
>     = and (zipWith (==) vs (replicate n (head vs)))
>     where
>     vs =  valsf f n

>   incorder :: [Int] -> Bool
>   incorder [] = True
>   incorder (x:xs)
>     | null xs   = True
>     | otherwise = (x <= (head xs)) && (incorder xs)

>   incf :: (Int -> Int) -> Int -> Bool
>   incf f n = incorder (map f [0 .. n])
