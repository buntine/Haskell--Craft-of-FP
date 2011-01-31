>   module Chapter7Exercises where
>   import Prelude hiding (getLine)
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

>   subseq :: Eq a => [a] -> [a] -> Bool
>   subseq [] _          = True
>   subseq _ []          = False
>   subseq (x:xs) (y:ys)
>     | null xs && x == y                    = True
>     | null ys && xs /= []                  = False
>     | x == y && xx == yy && subseq xxs yys = True
>     | subseq (x:xs) ys                     = True
>     | otherwise                            = False
>       where
>       (xx:xxs) = xs
>       (yy:yys) = ys


Text Processing

>   whitespace = ['\n', '\t', ' ']

>   getWord :: String -> String
>   getWord []     = []
>   getWord (x:xs)
>     | elem x whitespace = []
>     | otherwise         = x : getWord xs

>   dropWord :: String -> String
>   dropWord []     = []
>   dropWord (x:xs)
>     | elem x whitespace = (x:xs)
>     | otherwise         = dropWord xs

>   dropSpace :: String -> String
>   dropSpace []     = []
>   dropSpace (x:xs)
>     | elem x whitespace = dropSpace xs
>     | otherwise         = (x:xs)

>   type Word = String
>   type Line = [Word]
>   lineLen :: Int
>   lineLen = 35

>   splitWords :: String -> [Word]
>   splitWords s = (split . dropSpace) s

>   split :: String -> [Word]
>   split [] = []
>   split s  = (getWord s) : (split . dropSpace . dropWord) s

>   getLine :: Int -> [Word] -> Line
>   getLine len []     = []
>   getLine len (w:ws)
>     | length w <= len = w : restOfLine
>     | otherwise       = []
>       where
>       newLen     = len - (length w + 1)
>       restOfLine = getLine newLen ws


Exercise 7.19

>   dropLine :: Int -> [Word] -> Line
>   dropLine len []     = []
>   dropLine len (w:ws)
>     | length w <= len = restOfLine
>     | otherwise       = (w:ws)
>       where
>       newLen     = len - (length w + 1)
>       restOfLine = dropLine newLen ws

>   splitLines :: [Word] -> [Line]
>   splitLines [] = []
>   splitLines ws = getLine lineLen ws : splitLines (dropLine lineLen ws)

>   fill :: String -> [Line]
>   fill = splitLines .  splitWords


Exercise 7.20

>   joinLine :: Line -> String
>   joinLine []     = ""
>   joinLine (w:ws)
>     | null ws   = w
>     | otherwise = w ++ " " ++ (joinLine ws)


Exercise 7.21

>   joinLines :: [Line] -> String
>   joinLines []     = ""
>   joinLines (l:ls) = (joinLine l) ++ "\n" ++ (joinLines ls)


Exercise 7.23

>   spacesFor :: Line -> Int -> Int
>   spacesFor _ 0  = 1
>   spacesFor ws n
>     | div n (length ws) > 1 = 1 + (spacesFor ws (n - 1))
>     | otherwise             = 1

>   doJust :: Line -> Int -> String
>   doJust [] _     = ""
>   doJust (w:ws) s
>     | null ws   = w
>     | otherwise = w ++ chars ++ (doJust ws (s - spaces))
>       where
>       spaces = spacesFor ws s
>       chars  = replicate spaces ' '

>   justLine :: Line -> String
>   justLine [] = ""
>   justLine ws
>     = doJust ws spaces
>       where
>       spaces = lineLen - (foldl (\n w -> n + length w) 0 ws)

>   justLines :: [Line] -> String
>   justLines []     = ""
>   justLines (l:ls) 
>     | null ls   = joinLine l
>     | otherwise = (justLine l) ++ "\n" ++ (justLines ls)


Exercise 7.24
