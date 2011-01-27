>   module Chapter7Exercises where
>   import Hugs.Prelude
>   import Char


Exercise 7.1

>   digits :: String -> String
>   digits s = [c | c<-s, isDigit c]

>   firstDigit :: String -> Int
>   firstDigit s
>     = case (digits s) of
>       []     -> 0
>       (x:_)  -> digitToInt x + 1

>   intify :: String -> [Int]
>   intify ints = [digitToInt c | c<-ints]


Exercise 7.2

>   twoDigitsSum :: String -> Int
>   twoDigitsSum s
>     = case ((intify . digits) s) of
>       []      -> 0
>       (x:[])  -> x
>       (x:xs)  -> x + (head xs)


Exercise 7.3

>   firstDigit2 :: String -> Int
>   firstDigit2 s
>     | null ds    = 0
>     | otherwise  = head ds + 1
>     where
>     ds = (intify . digits) s

>   twoDigitsSum2 :: String -> Int
>   twoDigitsSum2 s
>     | null ds         = 0
>     | length ds == 1  = head ds
>     | otherwise       = (ds!!0) + (ds!!1)
>     where
>     ds = (intify . digits) s


Exercise 7.4

>   product2 :: [Int] -> Int
>   product2 []     = 1
>   product2 (x:xs) = x * product xs


Exercise 7.5

>   and2, or2 :: [Bool] -> Bool

>   and2 []     = True
>   and2 (x:xs) = x && and2 xs

>   or2 []     = False
>   or2 (x:xs) = x || or2 xs


Exercise 7.6

>   elemNum :: Int -> [Int] -> Int
>   elemNum x [] = 0
>   elemNum x (y:ys)
>     | x == y     = 1 + (elemNum x ys)
>     | otherwise  = elemNum x ys


Exercise 7.7

>   unique :: [Int] -> [Int]
>   unique [] = []
>   unique (x:xs)
>     | xs /= stripped  = unique stripped
>     | otherwise       = x : unique stripped
>     where
>     stripped = [y | y<-xs, y /= x]


