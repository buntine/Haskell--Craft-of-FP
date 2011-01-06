

	Interact.lhs

	Top-level interaction loop for a calculator

>	module Interact where

>	import IO hiding ( bracket )

>	import Types
>	import Store
>	import ParseLib
>	import ParseCalc
>	import Eval
>	import MonadIO


>	calcStep :: Store -> IO Store

>	calcStep st
>	  = do line <- getLine
>	       comm <- return (calcLine line)
>	       (val , newSt) <- return (command comm st)
>	       print val
>	       return newSt


>	calcSteps :: Store -> IO ()

>	calcSteps st
>	  = while notEOF
>	          (do newSt <- calcStep st
>	              calcSteps newSt)

>	notEOF :: IO Bool
>	notEOF = do res <- isEOF
>	            return (not res)

>	mainCalc :: IO ()
>	mainCalc = calcSteps initial




