module Semantic where

-- From Package
import Program


-- Other Modules
import Data.List -- nub, \\ and other list functions



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
--  Return: True = Error Message String
--          False = "" empty string
------------------------------------------------------------------------
noMainDefined           :: Program -> [Char]
noMainDefined []        = "Error: No main function defined.\n"
noMainDefined (f:fs)    = if isMain f then "" else noMainDefined fs

-- Check's the individual function for it's name
isMain                          :: Function -> Bool
isMain (Function name _ _ _)    = if name == "main" then True else False


-------------------------------------------------------------------------
-- Two variables and/or function arguments with the same name: "Error: variable '<variable name>' redefined."
-- Return   True - return Error strings containing name of all repeat IDs
--          False -> empty error string
-------------------------------------------------------------------------

repeatId        :: Program -> [Char]
repeatId []     = ""            -- Can't have repeat Id's in an empty program!!
repeatId (f:fs) = "" ++ (getFuncRepeatIdErrors f) ++ (repeatId fs) -- recurse to get error from all functions

-- | Function to get the individual functions errors
getFuncRepeatIdErrors   :: Function -> [Char]
getFuncRepeatIdErrors f = unlines (map (++ "' redefined.") (map ("Error: variable '" ++) (getRepeatIds f)))


-- | Get all the duplicats Ids
getRepeatIds    :: Function -> [Id]
getRepeatIds f  = ((getFuncArgs f) ++ (getFuncVars f)) \\ (nub ((getFuncArgs f) ++ (getFuncVars f)))

-- | Get Args for a a function
getFuncArgs                         :: Function -> [Id]     --getFuncArgs nil = []
getFuncArgs (Function _ args _ _)   = getArgIds args

-- | Get Id list for an arg
getArgIds               :: Args -> [Id]             --getArgIds nil = [] Not sure why this raises a warning...
getArgIds (Args ids)    = ids


-- | Get Vars for a function
getFuncVars                         :: Function -> [Id]
getFuncVars (Function _ _ vars _ )  = getVarIds vars

-- | get the Ids for a single Vars
getVarIds               :: Vars -> [Id]
getVarIds (Vars ids)    = ids


-------------------------------------------------------------------------
--  Undefined Variable:  "Error: variable '<variable name>' undefined."
--  Return True -> undefined Var exists + String -> id in question
--         False -> no undefined bools
--  what about every case???? 
-------------------------------------------------------------------------
undefinedVar :: Program -> [Char]
undefinedVar [] = ""
undefinedVar (f:fs) = "" ++ (getUndefinedVarsErrors f) ++ (undefinedVar fs)
--TODO

-- in each function
--      A: list of all declared Vars (including function param????)
--      B: list of all:
--                  in each statement - list of all ids /= ExpFn Id
--      Then:
--          [Declared] \\ [Statements]
-- could be multipl.
getUndefinedVarsErrors      :: Function -> [Char]
getUndefinedVarsErrors f    = unlines (map (++ "' undefined.") (map("Error: variable '" ++)(getFuncUndefinedVars f)))




-- | Get List of used vars that have not been declared.
getFuncUndefinedVars   :: Function -> [Id]
getFuncUndefinedVars f = (nub (getFuncUsedVars f)) \\ (getDeclaredVars f)

-- us getFuncVars + getFuncArgs from repeatID

-- | Get list of declared variables in a function.
getDeclaredVars     :: Function -> [Id]
getDeclaredVars f   = (getFuncArgs f) ++ (getFuncVars f)


-- | Get List of used Vars.
getFuncUsedVars                         :: Function -> [Id]
getFuncUsedVars (Function _ _ _ block)  = getBlocks_UsedVars block

-- | Get List of Blocks used in a function  - note block = [Statement]
getBlocks_UsedVars          :: [Statement] -> [Id]
getBlocks_UsedVars (b:bs)   = (getBlockUsedVars b) ++ (getBlocks_UsedVars bs)
getBlocks_UsedVars _        = []        

-- | Get List of Vars used in an individual block
getBlockUsedVars          :: Statement -> [Id]
getBlockUsedVars s        = (getAssignVars s) ++ (getIfId s) ++ (getIfElseVars s) ++ (getReturnId s)
                            -- Done                 Done            Done                Done
-- | Get all assigned vars in a block
getAssignVars                   :: Statement -> [Id]
getAssignVars (Assign id exp)   = [id] ++ (getExpVars exp)
--getAssignVars (Assign _ exp)    = (getExpVars exp)          -- This line should be redundant.
--getAssignVars (Assign id _)     = [id]                      -- THis line should also be redundant.
getAssignVars _                 = []


-- | Get all the vars in an expression
getExpVars      :: Exp -> [Id]
getExpVars e    =  (getExpIds e) ++ (getExpFunArgs e) ++ (getExpOpVars e) 
                    -- Done              Done                Done
-- | Get the id in an ExpId
getExpIds               :: Exp -> [Id]
getExpIds (ExpId id)    = [id]
getExpIds _             = [] --Need to put empty case last

-- | Get all args in a called function 
getExpFunArgs                   :: Exp -> [Id]
getExpFunArgs (ExpFun _ args)   = getArgIds args --From repeat IDs
getExpFunArgs _                 = []

-- | Get all the vars used in an ExpOp - expression operation.
getExpOpVars                    :: Exp -> [Id]
getExpOpVars (ExpOp _ e1 e2)    = (getExpVars e1) ++ (getExpVars e2)
getExpOpVars _                  = []

-- | Get all the vars from an If expression
getIfId                 :: Statement -> [Id]
getIfId (If id block)   = [id] ++ (getBlocks_UsedVars block)
getIfId _               = []


-- | Get all the vars from an IfElse expression
getIfElseVars                   :: Statement -> [Id]
getIfElseVars (IfElse id b1 b2) = [id] ++ (getBlocks_UsedVars b1) ++ (getBlocks_UsedVars b2)  
getIfElseVars _                 = []

-- | Get the returned Id 
getReturnId             :: Statement -> [Id]
getReturnId (Return id) = [id]
getReturnId _           = [] --should never happen...
-------------------------------------------------------------------------
--  Mismatching number of arguments at function call: "Error: function '<function name>' expects <n> argument(s)."
--  Return  True -> wrong number of args + String Function Name + String -> num args as strings
--          False -> all is well
-------------------------------------------------------------------------
argsMismatch :: Program -> [Char] ---(Bool, (String, String))
argsMismatch [] = ""
argsMismatch p = "" --"Error: function '<function name>' expects <n> argument(s).\n"



-- | Get list of tuples of function definitions and number of args
getProgFuncDefs			:: Program -> [(Id, Int)]
getProgFuncDefs	(f:fs)	= (getFuncDef f) ++ (getProgFuncDefs fs)

-- | Get list of all called functions and the number of args passed to each
getProgFuncCalls		:: Program -> [(Id, Int)]
getProgFuncCalls (f:fs)	= (getFuncCalls f) ++ (getProgFuncCalls fs)

-- | Get the Id and number of arguments of a given function
getFuncDef							:: Function -> [(Id, Int)]
getFuncDef (Function id args _ _)	= [(Id, (lenArgs args)]
getFuncDef _						= []

-- | Get the funtion id's and list of args for called functions within a function.
getFuncCalls						:: Function -> [(Id, Int)]
getFuncCalls (Function _ _ _ block)	= getFuncCalls_Blocks block  
getFuncCalls _						= []
-- | Get the function calls within a block
getFuncCalls_Blocks			:: [Statement] -> [(Id, Int)]
getFuncCalls_Blocks (b:bs)	= (getFuncCallsBlock b) ++ (getFuncCalls_Blocks bs)	
getFuncCalls_Block _		= []

-- | Get the function calls at the single Statement level
getFuncCallsBlock			:: Statement -> [(Id, Int)]
getFuncCallsBlock s			= (getFuncCallsAssign s) ++ (getFuncCallsIf s) ++ (getFuncCallsIfElse s)
								-- TODO						TODO					TODO
-- | Get Function calls within an Assignment
getFuncCallsAssign					:: Statement -> [(Id, Int)]
getFuncCallsAssign (Assign _ exp)	= getFuncCallExp exp
getFuncCallsAssign _				= []



-- List of all functions + num args
-- look at each call + compare - return true + name + num

--getFunctionArgs :: Program -> [(String, String)] -- -> [(String, Int)]
--getFunctionArgs (f:fs) = [(getFuncArgs f)] ++ getFunctionArgs fs


--getFuncArgs :: Function -> (String, String)
--getFuncArgs (Function name args vars _ ) = ((show name), (show (lenArgs args)))

-- | Return the number of arguments in a given args list of Ids
lenArgs :: Args -> Int
lenArgs (Args ids) = length ids



lenVars :: Vars -> Int
lenVars (Vars ids) = length ids
-------------------------------------------------------------------------
--  Two functions with the same name: "Error: '<function name>' redefined."
--  Return  True -> Two functions with same name exist + String -> repeated name
--          False -> no repeats
-------------------------------------------------------------------------
repeatFuncName :: Program -> [Char]
repeatFuncName [] = ""
repeatFuncName p = ""--"Error: '<function name>' redefined.\n"

-- Create list of all functions
-- look for duplicates - return true + names




-------------------------------------------------------------------------
--  Undefined Function: "Error: function '<function name>' undefined."
--  Return  True -> undefined function exists + String name
--          False -> no repeats
-------------------------------------------------------------------------
undefinedFunc :: Program -> [Char]
undefinedFunc [] = ""
undefinedFunc p = "" --"Error: function '<function name>' undefined.\n"


-- Create a list if functions
-- look through eah function body for a function...
-- if yes pass back Truw + name





---------------------------------------------------------------
-- Wrapper functions to call from Misery.hs
--      RETURN: True if no erros
--              Throw an error and terminate of they find a syntax error
--              Error will containd etails of bad syntax
--
---------------------------------------------------------------
semanticCheck :: Program -> Bool
semanticCheck p = if (null (getSemanticErrors p))
                    then True 
                    else (semanticError (getSemanticErrors p))




-- | Get the error string if they exist 
getSemanticErrors :: Program -> [Char]
getSemanticErrors [] = "Error: empty file..."                       -- Empty program...
getSemanticErrors p = "" 
                    ++ (noMainDefined p)           -- Working - not tested
                    ++ (undefinedFunc p)           -- TODO
                    ++ (repeatFuncName p)          -- TODO
                    ++ (argsMismatch p)            -- TODO
                    ++ (undefinedVar p)            -- Working - not tested
                    ++ (repeatId p)                -- Working - not tested
--"TEST ERROR\n" ++ "MORE TEST ERRORS\n"


-- | Error function to call if we fund an error

semanticError :: [Char] -> a
semanticError err = error ("\n" ++ err) --Newline to ensure we start erros on a fresh line. 



