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


Exercise 7.4

>   twoDigitsSum2 :: String -> Int
>   twoDigitsSum2 s
>     | null ds         = 0
>     | length ds == 1  = head ds
>     | otherwise       = (ds!!0) + (ds!!1)
>     where
>     ds = (intify . digits) s
