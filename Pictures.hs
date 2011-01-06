--------------------------------------------------------------------
-- 
--   Pictures.hs
-- 
--   Simon Thompson
-- 
--   June 1998
--   Last modified 28 September 2000
-- 
-- An implementation of a type of rectangular pictures using lists of 
-- lists of characters.
-- 
--------------------------------------------------------------------

-- The basics
-- ^^^^^^^^^^

module Pictures where

type Picture = [[Char]]

-- The example used in Craft2e: a polygon which looks like a horse. Here
-- taken to be a 16 by 12 rectangle.

horse :: Picture

horse = [".......##...",
         ".....##..#..",
         "...##.....#.",
         "..#.......#.",
         "..#...#...#.",
         "..#...###.#.",
         ".#....#..##.",
         "..#...#.....",
         "...#...#....",
         "....#..#....",
         ".....#.#....",
         "......##...."]

-- A completely white picture.

white :: Picture

white = ["............",
         "............",
         "............",
         "............",
         "............",
         "............",
         "............",
         "............",
         "............",
         "............",
         "............",
         "............"]

-- Getting a picture onto the screen.

printPicture :: Picture -> IO ()

printPicture = putStr . concat . map (++"\n")


-- Transformations of pictures.
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^

-- Reflection in a vertical mirror.

flipV :: Picture -> Picture

flipV = map reverse

-- Reflection in a horizontal mirror.

flipH :: Picture -> Picture

flipH = reverse

-- Rotation through 180 degrees, by composing vertical and horizontal
-- reflection. Note that it can also be done by flipV.flipH, and that we
-- can prove equality of the two functions.

rotate :: Picture -> Picture

rotate = flipH . flipV

-- One picture above another. To maintain the rectangular property,
-- the pictures need to have the same width.

above :: Picture -> Picture -> Picture

above = (++)

-- One picture next to another. To maintain the rectangular property,
-- the pictures need to have the same height.

sideBySide :: Picture -> Picture -> Picture

sideBySide = zipWith (++)

-- Superimose one picture above another. Assume the pictures to be the same
-- size. The individual characters are combined using the combine function.

superimpose :: Picture -> Picture -> Picture

superimpose = zipWith (zipWith combine)

-- For the result to be '.' both components have to the '.'; otherwise
-- get the '#' character.

combine :: Char -> Char -> Char

combine topCh bottomCh
  = if (topCh == '.' && bottomCh == '.') 
    then '.'
    else '#'

-- Inverting the colours in a picture; done pointwise by invert...

invertColour :: Picture -> Picture

invertColour = map (map invert)

-- ... which works by making the result '.' unless the input is '.'.

invert :: Char -> Char

invert ch = if ch == '.' then '#' else '.'



