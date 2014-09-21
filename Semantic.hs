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



-------------------------------------------------------------------------
--  If a program does not define main: "Error: No main function defined."
--  Return: True -> no main
--          False -> main
------------------------------------------------------------------------
noMainDefined :: Program -> Bool
noMainDefined [] = True
noMainDefined (f:fs) = if isMain f then False else noMainDefined fs

-- Check's the individual function for it's name
isMain :: Function -> Bool
isMain (Function name _ _ _) = if name == "main" then True else False


-------------------------------------------------------------------------
--  Two variables and/or function arguments with the same name: "Error: variable '<variable name>' redefined."
-- Return   True - id repeated + String -> id in question
--          False -> no repeats + String -> empty string .. for no real reason
-------------------------------------------------------------------------

repeatId :: Program -> (Bool, String)
repeatId p = (False, "TODO")







-------------------------------------------------------------------------
--  Undefined Variable:  "Error: variable '<variable name>' undefined."
--  Return True -> undefined Var exists + String -> id in question
--         False -> no undefined bools
--  what about every case???? 
-------------------------------------------------------------------------
undefinedVar :: Program -> (Bool, String)
undefinedVar p = (False, "TODO")










-------------------------------------------------------------------------
--  Mismatching number of arguments at function call: "Error: function '<function name>' expects <n> argument(s)."
--  Return  True -> wrong number of args + String Function Name + String -> num args as strings
--          False -> all is well
-------------------------------------------------------------------------
argMismatch :: Program -> (Bool, (String, String))
argMismatch p = (False, ("TODO", "9000"))





-------------------------------------------------------------------------
--  Two functions with the same name: "Error: '<function name>' redefined."
--  Return  True -> Two functions with same name exist + String -> repeated name
--          False -> no repeats
-------------------------------------------------------------------------
repeatFuncName :: Program -> (Bool, String)
repeatFuncName p = (False, "TODO")



-------------------------------------------------------------------------
--  Undefined Function: "Error: function '<function name>' undefined."
--  Return  True -> undefined function exists + String name
--          False -> no repeats
-------------------------------------------------------------------------
undefinedFunc :: Program -> (Bool, String)
undefinedFunc p = (False, "TODO")



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
                    [ ("Error: variable " ++  (snd (repeatId p))  ++ " redefined.")  | (fst (repeatId p))],
                    [ "Error: function " ++ (snd (undefinedFunc p)) ++ " undefined." | (fst (undefinedFunc p))],
                    [ "Error: function " ++ (fst (snd (argMismatch p))) ++ "  expects " ++ (snd (snd (argMismatch p))) ++ " argument(s)." | (fst (argMismatch p))],
                    [ "Error: variable " ++ (snd (undefinedVar p))  ++ " undefined." | (fst (undefinedVar p))],
                    [ "Error: " ++ (snd (repeatFuncName p)) ++ " redefined." | (fst (repeatFuncName p))]]






