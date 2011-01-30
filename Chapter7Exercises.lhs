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



Exercise 7.8

>   reverse2 :: [a] -> [a]
>   reverse2 []     = []
>   reverse2 (x:xs) = concat [reverse2 xs, [x]]

>   unzip2 :: [(a,b)] -> ([a],[b])
>   unzip2 []     = ([],[])
>   unzip2 (x:xs) = ((fst x):(fst (unzip2 xs)),
>                    (snd x):(snd (unzip2 xs)))


QuickSort for fun

>   quicksort :: [Int] -> [Int]
>   quicksort [] = []
>   quicksort (x:xs)
>     = quicksort [y | y<-xs, y<=x] ++ [x] ++ quicksort [y | y<-xs, y>x]


Exercise 7.14

>   take2 :: Int -> [a] -> [a]
>   take2 0 _      = []
>   take2 _ []     = []
>   take2 n (x:xs) = x : take2 (n - 1) xs
>   take2 _ _      = error "Die in a fire"

>   drop2 :: Int -> [a] -> [a]
>   drop2 0 xs     = xs
>   drop2 _ []     = []
>   drop2 n (x:xs) = drop2 (n - 1) xs
>   drop2 _ _      = error "Melt in the abyss" 

>   splitAt2 :: Int -> [a] -> ([a],[a])
>   splitAt2 0 xs     = ([],xs)
>   splitAt2 _ []     = ([],[])
>   splitAt2 n (x:xs)
>     = (x : (fst result), snd result)
>       where
>       result = splitAt2 (n - 1) xs


Exercise 7.18

>   sublist :: Eq a => [a] -> [a] -> Bool
>   sublist [] _      = True
>   sublist _ []      = False
>   sublist (x:xs) ys = (elem x ys) && (sublist xs ys)

>   subs :: Eq a => [a] -> [a] -> Bool
>   subs [] _          = True
>   subs _ []          = False
>   subs (x:xs) (y:ys)
>     | null xs && x == y                  = True
>     | null ys && xs /= []                = False
>     | x == y && xx == yy && subs xxs yys = True
>     | subs (x:xs) ys                     = True
>     | otherwise                          = False
>       where
>       (xx:xxs) = xs
>       (yy:yys) = ys
