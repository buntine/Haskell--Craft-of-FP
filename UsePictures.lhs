Andrew Buntine, Jan 2011


Exercise 2.1

>   module UsePictures where
>   import Pictures

>   blackHorse :: Picture
>   blackHorse = invertColour horse

>   rotateHorse :: Picture
>   rotateHorse = rotate horse


Exercise 2.2

>   black :: Picture
>   black = superimpose blackHorse horse


Exercise 2.3

>   blackWhite1 :: Picture
>   blackWhite1 = above (sideBySide black white) (sideBySide white black) 

>   blackwhite :: Picture
>   blackwhite = sideBySide black white
>   blackWhite2 :: Picture
>   blackWhite2 = above blackwhite (invertColour blackwhite)


Exercise 2.4

>   topHorses :: Picture
>   topHorses = sideBySide horse blackHorse
>   bottomHorses :: Picture
>   bottomHorses = sideBySide blackHorse horse

>   horses1 :: Picture
>   horses1 = above topHorses bottomHorses

>   horses2 :: Picture
>   horses2 = above topHorses (invertColour (flipV bottomHorses))

>   horses3 :: Picture
>   horses3 = above topHorses (invertColour (rotate bottomHorses))


Exercise 2.5

>   horses4 :: Picture
>   horses4 = above topHorses (flipV (rotate bottomHorses))
