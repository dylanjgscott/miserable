module Semantic where

-- From Package
import Program


-- Other Modules




-------------------------------------------------------------
--  All the Errors to catch:
--
--  Undefined Function:
--  "Error: function '<function name>' undefined."
--
--  Two functions with the same name:
--  "Error: '<function name>' redefined."
--
--  Mismatching number of arguments at function call
--  "Error: function '<function name>' expects <n> argument(s)."
--
--  Undefined Variable
--  "Error: variable '<variable name>' undefined."
--
--  Two variables and/or function arguments with the same name:
--  "Error: variable '<variable name>' redefined."
--
--  If a program does not define main:
--  "Error: No main function defined."
--
--  Syntax Error *** caught by Parser
--  "Syntax Error."
--
--
--
--  Returning a list of errors for now - might change to exceptions - need to check with rest of group..
---------------------------------------------------------




--  If a program does not define main: "Error: No main function defined."
--  Return: True -> no main
--          False -> main
noMainDefined :: Program -> Bool
noMainDefined p = True --TODO
--mainDefined (Program x) = hasMain x

--hasMain :: Functions -> Bool
--hasMain EmptyFunctions = False
--hasMain (Functions f fs) = if isMain f then True else hasMain fs

--isMain :: Function -> Bool
--isMain (Function (Id name) _ _ _) = if name == "main" then True else False



--  Two variables and/or function arguments with the same name: "Error: variable '<variable name>' redefined."
-- Return   True - id repeated
--          Fallse -> no repeats
repeatId :: Program -> Bool
repeatId p = True --TODO







--  Undefined Variable:  "Error: variable '<variable name>' undefined."
--  Return True -> undefined Var exists
--         False -> no undefined bools
--  what about every case???? 
undefinedVar :: Program -> Bool
undefinedVar p = True










--  Mismatching number of arguments at function call: "Error: function '<function name>' expects <n> argument(s)."
--  Return  True -> wrong number of args
--          False -> all is well
argMismatch :: Program -> Bool
argMismatch p = True




--  Two functions with the same name: "Error: '<function name>' redefined."
--  Return  True -> Two functions with same name exist
--          False -> no repeats
repeatFuncName :: Program -> Bool
repeatFuncName p = True



--  Undefined Function: "Error: function '<function name>' undefined."
--  Return  True -> undefined function exists
--          False -> no repeats
undefinedFunc :: Program -> Bool
undefinedFunc p = True



---------------------------------------------------------------
-- Wrapper function to call from Misery.hs
--
--
---------------------------------------------------------------


-- | This is the stub for the throw error version
--semanticCheck :: Program -> Bool
--semanticCheck p = False 





-- List Version
--
-- Actually the bellow might not work since I need to also pass back some values.... but it looks so neat...
--
semanticCheck :: Program -> [[Char]]
semanticCheck p = concat [[],
                    [ "Error: No main function defined." | (noMainDefined p)], 
                    [ "Error: variable '<variable name>' redefined." | (repeatId p)],
                    [ "Error: function '<function name>' undefined." | (undefinedFunc p)],
                    [ "Error: function '<function name>' expects <n> argument(s)." | (argMismatch p)],
                    [ "Error: variable '<variable name>' undefined." | (undefinedVar p)],
                    [ "Error: '<function name>' redefined." | (repeatFuncName p)]]






