-- Main company source file.  

import Data.Char
import System.Environment

import AsmLexer
import AsmParser
import Interpreter

-- 
main :: IO ()
main  
 = do   -- Use the first command-line argument as the file name.
        [fileName]      <- getArgs

        -- Read in the source file.
        source          <- readFile fileName

  

        putStr (show (runProgram (asmParse (alexScanTokens source)) ([("n", 4)], [])))
