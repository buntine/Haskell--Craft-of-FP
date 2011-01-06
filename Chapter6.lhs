
	Haskell: The Craft of Functional Programming
	Simon Thompson
	(c) Addison-Wesley, 1999.

	Chapter 6

>	module Chapter6 where

The Picture example, revisited.
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The type of pictures.

>	type Picture = [[Char]]

To flip a
picture in a horizontal mirror, 

>	flipH :: Picture -> Picture
>	flipH = reverse

and to place one picture above another it is sufficient to join the two lists of
lines together.

>	above :: Picture -> Picture -> Picture
>	above = (++)

To flip a picture in a vertical mirror.

>	flipV :: Picture -> Picture
>	flipV pic 
>	  = [ reverse line | line <- pic ]

To place two pictures side by side. 

>	sideBySide :: Picture -> Picture -> Picture
>	sideBySide picL picR
>	  = [ lineL ++ lineR | (lineL,lineR) <- zip picL picR ]

To invert the colour of a single character ...

>	invertChar :: Char -> Char
>	invertChar ch 
>	  = if ch=='.' then '#' else '.'

a line ...

>	invertLine :: [Char] -> [Char]
>	invertLine line 
>	  = [ invertChar ch | ch <- line ]

and a picture.

>	invertColour :: Picture -> Picture
>	invertColour pic 
>	  = [ invertLine line | line <- pic ]

Alternative definition of invertColour:

>	invertColour' :: Picture -> Picture
>	invertColour' pic 
>	  = [ [ invertChar ch | ch <- line ] | line <- pic ]


Extended exercise: positioned pictures
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Positions on a plane.

>	type Position = (Int,Int)

An Image is a picture with a position.

>	type Image = (Picture,Position)

makeImage :: Picture -> Position -> Image
changePosition :: Image -> Position -> Image
moveImage :: Image -> Int -> Int -> Image
printImage :: Image -> IO ()


Local definitions
^^^^^^^^^^^^^^^^^

The sum of the squares of two numbers.  

>	sumSquares :: Int -> Int -> Int

>	sumSquares n m 
>	  = sqN + sqM
>	    where
>	    sqN = n*n
>	    sqM = m*m

Add corresponding elements in two lists; lists truncated to the length of the
shorter one.

>	addPairwise :: [Int] -> [Int] -> [Int]
>	addPairwise intList1 intList2
>	  = [ m + n | (m,n) <- zip intList1 intList2 ]

A variant of addPairwise which doesn't truncate; see book for details of how
it works.

>	addPairwise' :: [Int] -> [Int] -> [Int]

>	addPairwise' intList1 intList2
>	  = front ++ rear
>	    where
>	    minLength = min (length intList1) (length intList2)
>	    front     = addPairwise (take minLength intList1) 
>	                            (take minLength intList2)
>	    rear      = drop minLength intList1 ++ drop minLength intList2

and a variant of this ...

>	addPairwise'' :: [Int] -> [Int] -> [Int]

>	addPairwise'' intList1 intList2
>	  = front ++ rear
>	    where
>	    minLength      = min (length intList1) (length intList2)
>	    front          = addPairwise front1 front2
>	    rear           = rear1 ++ rear2
>	    (front1,rear1) = splitAt minLength intList1
>	    (front2,rear2) = splitAt minLength intList2


Let expressions
^^^^^^^^^^^^^^^

Two examples which use `let'.

>	letEx1 :: Int
>	letEx1 = let x = 3+2 in x^2 + 2*x - 4

>	letEx2 :: Int
>	letEx2 = let x = 3+2 ; y = 5-1 in x^2 + 2*x - y


Scopes
^^^^^^
Is a value odd? even?

>	isOdd, isEven :: Int -> Bool

>	isOdd n 
>	  | n<=0        = False
>	  | otherwise   = isEven (n-1)

>	isEven n 
>	  | n<0         = False
>	  | n==0        = True
>	  | otherwise   = isOdd (n-1)


Extended exercise: supermarket billing
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Types of names, prices (pence) and bar-codes.

>	type Name    = String
>	type Price   = Int
>	type BarCode = Int

The database linking names prices and bar codes.

>	type Database = [ (BarCode,Name,Price) ]

The example database we use is

>	codeIndex :: Database
>	codeIndex = [ (4719, "Fish Fingers" , 121),
>	              (5643, "Nappies" , 1010),
>	              (3814, "Orange Jelly", 56),
>	              (1111, "Hula Hoops", 21),
>	              (1112, "Hula Hoops (Giant)", 133),
>	              (1234, "Dry Sherry, 1lt", 540)]

The lists of bar codes, and of Name,Price pairs.

>	type TillType = [BarCode]
>	type BillType = [(Name,Price)]

The length of a line in the bill.

>	lineLength :: Int
>	lineLength = 30


