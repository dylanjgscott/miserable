-- Main company source file.  

import Data.Char
import System.Environment

import AsmLexer
import AsmParser
import Interpreter
import Assembly

-- 

getAsmArgs :: [String] -> [AsmNum]
getAsmArgs = (\x -> map read (tail x))

main :: IO ()
main  
 = do   -- Use the first command-line argument as the file name.
        args <- getArgs
        let fileName = head args
        source <- readFile fileName
        let asm = asmParse (alexScanTokens source)
        let asmArgs = getAsmArgs args
        print $ runProgram asm asmArgs
