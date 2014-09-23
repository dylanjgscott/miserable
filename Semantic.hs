module Semantic where

-- From Package
import Program
-- Other Modules
import Data.List -- nub, \\ and other list functions

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
getFuncArgs                         :: Function -> [Id]
getFuncArgs (Function _ args _ _)   = getArgIds args

-- | Get Id list for an arg
getArgIds               :: Args -> [Id]         
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

-- | Generated the error String
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
getFuncUsedVars (Function _ _ _ (Block statements))  = getBlocks_UsedVars statements

-- | Get List of Blocks used in a function  - note block = [Statement]
getBlocks_UsedVars          :: [Statement] -> [Id]
getBlocks_UsedVars (b:bs)   = (getBlockUsedVars b) ++ (getBlocks_UsedVars bs)
getBlocks_UsedVars _        = []        

-- | Get List of Vars used in an individual block
getBlockUsedVars          :: Statement -> [Id]
getBlockUsedVars s        = (getAssignVars s) ++ (getIfElseVars s) ++ (getReturnId s)
                            -- Done                 Done            Done                Done
-- | Get all assigned vars in a block
getAssignVars                   :: Statement -> [Id]
getAssignVars (Assign id exp)   = [id] ++ (getExpVars exp)
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
--getIfId                 :: Statement -> [Id]
--getIfId (If id block)   = [id] ++ (getBlocks_UsedVars block)
--getIfId _               = []

-- | Get all the vars from an IfElse expression
getIfElseVars                   :: Statement -> [Id]
getIfElseVars (IfElse id (Block b1) (Block b2)) = [id] ++ (getBlocks_UsedVars b1) ++ (getBlocks_UsedVars b2)  
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
argsMismatch p = "" ++ (genArgsMismatchError p)--"Error: function '<function name>' expects <n> argument(s).\n"

-- | Map function for error String composition
argMismatchString	:: (Id, Int) -> [Char] 
argMismatchString (id, int)	= ("Error: function '" ++ id ++ "' expects " ++ (show int) ++ " argument(s).") 

-- | Generate the erros String
genArgsMismatchError	:: Program -> [Char]
genArgsMismatchError p	= unlines (map (argMismatchString) (getArgsMismatch p)) 
-- | Get the erroneous calls
getArgsMismatch		:: Program -> [(Id, Int)]
getArgsMismatch p	= (nub (getProgFuncCalls p)) \\ (getProgFuncDefs p)

-- | Get list of tuples of function definitions and number of args
getProgFuncDefs			:: Program -> [(Id, Int)]
getProgFuncDefs	(f:fs)	= (getFuncDef f) ++ (getProgFuncDefs fs)
getProgFuncDefs _		= []
-- | Get list of all called functions and the number of args passed to each
getProgFuncCalls		:: Program -> [(Id, Int)]
getProgFuncCalls (f:fs)	= (getFuncCalls f) ++ (getProgFuncCalls fs)
getProgFuncCalls _		= []
-- | Get the Id and number of arguments of a given function
getFuncDef							:: Function -> [(Id, Int)]
getFuncDef (Function id args _ _)	= [(id, (lenArgs args))]
--getFuncDef _						= []

-- | Get the funtion id's and list of args for called functions within a function.
getFuncCalls						:: Function -> [(Id, Int)]
getFuncCalls (Function _ _ _ (Block block))	= getFuncCalls_Blocks block  
--getFuncCalls _						= []
-- | Get the function calls within a block
getFuncCalls_Blocks			:: [Statement] -> [(Id, Int)]
getFuncCalls_Blocks (b:bs)	= (getFuncCallsBlock b) ++ (getFuncCalls_Blocks bs)	
getFuncCalls_Blocks _		= []

-- | Get the function calls at the single Statement level
getFuncCallsBlock			:: Statement -> [(Id, Int)]
getFuncCallsBlock s			= (getFuncCallsAssign s) ++ (getFuncCallsIfElse s)
								-- Done						Done					Done
-- | Get Function calls within an Assignment
getFuncCallsAssign					:: Statement -> [(Id, Int)]
getFuncCallsAssign (Assign _ exp)	= getFuncCallExp exp
getFuncCallsAssign _				= []

-- | Get Function Calls in Exp
getFuncCallExp		:: Exp -> [(Id, Int)]
getFuncCallExp e	= (getExpFuncCalls e) ++ (getExpOpCalls e)


-- | Get the id and args from function calls
getExpFuncCalls						:: Exp -> [(Id, Int)]
getExpFuncCalls (ExpFun id args)	= [(id, (lenArgs args))]
getExpFuncCalls _					= []

-- | Get ExpOp function calls
getExpOpCalls					:: Exp -> [(Id, Int)]
getExpOpCalls (ExpOp _ e1 e2)	= (getFuncCallExp e1) ++ (getFuncCallExp e2)
getExpOpCalls _					= []

-- | Get function calls in an IF
--getFuncCallsIf					:: Statement -> [(Id, Int)]
--getFuncCallsIf (If _ block)		= getFuncCalls_Blocks block
--getFuncCallsIf _				= []

-- Get any function calls from an ifelse
getFuncCallsIfElse					:: Statement -> [(Id, Int)]
getFuncCallsIfElse (IfElse _ (Block b1) (Block b2))	= (getFuncCalls_Blocks b1) ++ (getFuncCalls_Blocks b2)
getFuncCallsIfElse _				= []

-- | Return the number of arguments in a given args list of Ids
lenArgs :: Args -> Int
lenArgs (Args ids) = length ids 


-- | Not sure I need...
lenVars :: Vars -> Int
lenVars (Vars ids) = length ids
-------------------------------------------------------------------------
--  Two functions with the same name: "Error: '<function name>' redefined."
--  Return  True -> Two functions with same name exist + String -> repeated name
--          False -> no repeats
-------------------------------------------------------------------------
repeatFuncName :: Program -> [Char]
repeatFuncName [] = ""
repeatFuncName p = "" ++ (genRepeatFuncErrors p)--"Error: '<function name>' redefined.\n"

-- | Generate the Error string
genRepeatFuncErrors		:: Program -> [Char]
genRepeatFuncErrors p	= unlines (map (++ "' redefined.") (map ("Error: function '" ++)(getRepeatFuncIds p)))

-- | Get list of repeated Functions
getRepeatFuncIds	:: Program -> [Id]
getRepeatFuncIds p	= (getProgFuncIds p) \\ (nub (getProgFuncIds p))

-- | Get list of function Ids
getProgFuncIds			:: Program -> [Id]
getProgFuncIds	(f:fs)	= (getFuncId f) ++ (getProgFuncIds fs)
getProgFuncIds _		= []

-- | Get the Id a given function
getFuncId							:: Function -> [Id]
getFuncId (Function id _ _ _)	= [id]

-------------------------------------------------------------------------
--  Undefined Function: "Error: function '<function name>' undefined."
--  Return  True -> undefined function exists + String name
--          False -> no repeats
-------------------------------------------------------------------------
undefinedFunc :: Program -> [Char]
undefinedFunc [] = ""
undefinedFunc p = "" --"Error: function '<function name>' undefined.\n"

-- | Generate the Error String
genUndefinedFuncErrors	:: Program -> [Char]
genUndefinedFuncErrors p = unlines (map (++ "' undefined.") (map ("Error: function '" ++)(getUndefinedFuncErrors p)))

-- | Get all the bad IDs
getUndefinedFuncErrors	:: Program -> [Id]
getUndefinedFuncErrors p = (nub (getListofCalledFuncIds (getProgFuncCalls p))) \\ (getProgFuncIds p)

getListofCalledFuncIds			:: [(Id, Int)] -> [Id]
getListofCalledFuncIds []		= []
getListofCalledFuncIds (l:ls)	= (fst l) : (getListofCalledFuncIds ls)

-- [(Id, Int)]


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
                    ++ (argsMismatch p)            -- Working - not tested
                    ++ (undefinedVar p)            -- Working - not tested
                    ++ (repeatId p)                -- Working - not tested
-- | Error function to call if we fund an error
semanticError :: [Char] -> a
semanticError err = error ("\n" ++ err) --Newline to ensure we start erros on a fresh line. 



