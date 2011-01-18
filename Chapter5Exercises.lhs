>   module Chapter5Exercises where
>   import Hugs.Prelude hiding (elem)
>   import Char


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

>   cuzza :: Int -> Int -> Int
>   cuzza a b = a + b


For fun

>   addPairs :: [(Int, Int)] -> [Int]
>   addPairs lst = [x + y | (x,y)<-lst]


Exercise 5.8

>   doubleAll :: [Int] -> [Int]
>   doubleAll lst = [n*2 | n<-lst]


Exercise 5.9

>   offset :: Int
>   offset = ord 'A' - ord 'a'
>   smallToUpper :: Char -> Char
>   smallToUpper ch
>     | (ch >= 'a') && ('z' >= ch)  = chr (ord ch + offset)
>     | otherwise                   = ch

>   capitalize :: String -> String
>   capitalize s = [smallToUpper c | c<-s]

>   capitalizeLetters :: String -> String
>   capitalizeLetters s = [smallToUpper c | c<-s, isAlpha c]


Exercise 5.10

>   divisors :: Int -> [Int]
>   divisors n = [i | i<-[1 .. n], mod n i == 0]

>   isPrime :: Int -> Bool
>   isPrime n
>     | n >= 0     = length (divisors n) == 2
>     | otherwise  = False


Exercise 5.11

>   matches :: Int -> [Int] -> [Int]
>   matches i lst = [n | n<-lst, n == 1]

>   elem :: Int -> [Int] -> Bool
>   elem i lst = length (matches i lst) > 0


Exercise 5.13

>   type Person   = String
>   type Book     = String
>   type Database = [(Person, Book)]

>   makeLoan  :: Database -> Person -> Book -> Database
>   makeLoan db person book = [(person, book)] ++ db

>   returnLoan :: Database -> Person -> Book -> Database
>   returnLoan db person book = [pair | pair<-db, pair /= (person, book)]

>   books :: Database -> Person -> [Book]
>   books db person = [b | (p,b)<-db, p == person]

>   borrowers :: Database -> Book -> [Person]
>   borrowers db book = [p | (p,b)<-db, b == book]

>   borrowed :: Database -> Book -> Bool
>   borrowed db book = borrowers db book /= []

>   numBorrowed :: Database -> Person -> Int
>   numBorrowed db person = length (books db person)


Exercise 5.20

>   romanDigit :: Char -> String
>   romanDigit c = ["0", "I", "II", "III", "IV", "V", "VI", "VII", "IIX", "IX"]!!((ord c) - 48)


Exercise 5.21

>   onThreeLines :: String -> String -> String -> String
>   onThreeLines a b c = a++"\n"++b++"\n"++c


Exercise 5.22

>   onSeparateLines :: [String] -> String
>   onSeparateLines [] = ""
>   onSeparateLines lines
>     | length lines == 1  = head lines
>     | otherwise          = (head lines) ++ "\n" ++ (onSeparateLines (tail lines))


Exercise 5.23

>   duplicate :: String -> Int -> String
>   duplicate s 1 = s
>   duplicate s n
>     | n <= 0     = ""
>     | otherwise  = s ++ (duplicate s (n-1))


Exercise 5.26

>   fib :: Int -> Int
>   fib 0 = 0
>   fib 1 = 1
>   fib n = (fib (n - 1)) + (fib (n - 2))

>   fibs :: Int -> Int -> String
>   fibs n to
>     | n == to    = ""
>     | otherwise  = (show n) ++ "\t" ++ (show (fib n)) ++ "\n" ++ (fibs (n + 1) to)

>   fibTable :: Int -> String
>   fibTable n = "n\tfib n\n" ++ (fibs 0 (n + 1))
