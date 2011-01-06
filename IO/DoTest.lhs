
	DOtest.lhs


>	test1 :: IO ()

>	test1 
>	  = do getLine
>              getLine
>              line3 <- getLine
>	       putStr (line3 ++ "\n")


>	put4times :: String -> IO ()

>	put4times str 
>	  = do putStrLn str
>              putStrLn str
>              putStrLn str
>              putStrLn str

>	putNtimes :: Int -> String -> IO ()

>	putNtimes n str
>	  = if n <= 1 
>	       then putStrLn str
>	       else do putStrLn str
>		       putNtimes (n-1) str

>	read2lines :: IO ()

>	read2lines 
>	  = do getLine
>	       getLine
>	       putStrLn "Two lines read."

>	reverse2lines :: IO ()
>	reverse2lines
>	  = do line1 <- getLine
>	       line2 <- getLine
>	       putStrLn (reverse line2)
>	       putStrLn (reverse line1)

>	addOneInt :: IO ()
>	addOneInt 
>	  = do line <- getLine
>	       putStrLn (show (1 + read line :: Int))
 

