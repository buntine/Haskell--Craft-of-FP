-- 								
--         MakeCode.lhs							
-- 								
--         Huffman coding in Haskell.					
-- 								
--         (c) Simon Thompson, 1995, 1998.					
-- 							

module MakeCode ( codes, codeTable ) where

import Types
import Frequency ( frequency )
import MakeTree  ( makeTree )
import CodeTable ( codeTable )

-- Putting together frequency calculation and tree conversion	

codes :: [Char] -> Tree

codes = makeTree . frequency

