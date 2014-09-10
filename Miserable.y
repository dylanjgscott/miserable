
-- parser from the lecture Demo - this is dummy content for now so I can compile it. 











---------------------------------------------------------------------
-- This is a parser defined with the Happy parser generator tool.
-- See http://www.haskell.org/happy/ for details.
--
-- Happy is installed by default along with the Haskell platform.
-- 
-- Convert this file to a Haskell source file with:
--  happy -iParser.info Parser.y
--
---------------------------------------------------------------------
{
module Parser where
import Program
import Token
import Data.Char
}

---------------------------------------------------------------------
-- Name of the parser.
-- In the generated code we get a function  
--   calc :: [Token] -> Exp
%name           calc

-- The type of the input tokens.
%tokentype      { Token }

-- Function to call when there is a parse error.
%error          { parseError }

-- Specify how we will refer to the data contructors that represent
-- each token when defining the grammar.
%token
 let            { TokenLet      }
 in             { TokenIn       }
 int            { TokenInt $$   }
 var            { TokenVar $$   }
 '='            { TokenEq       }
 '+'            { TokenPlus     }
 '-'            { TokenMinus    }
 '*'            { TokenTimes    }
 '/'            { TokenDiv      }
 '('            { TokenBra      }
 ')'            { TokenKet      }

-- Here comes the grammar.
%%
Exp     : let var '=' Exp in Exp        { Let    $2 $4 $6 }
        | Exp1                          { Exp1   $1       }

Exp1    : Exp1 '+' Term                 { Plus   $1 $3    }
        | Exp1 '-' Term                 { Minus  $1 $3    }
        | Term                          { Term   $1       }

Term    : Term '*' Factor               { Times  $1 $3    }
        | Term '/' Factor               { Div    $1 $3    }
        | Factor                        { Factor $1       }

Factor  : int                           { Int    $1       }
        | var                           { Var    $1       }
        | '(' Exp ')'                   { Brack  $2       }

{
-- Call this function when we get a parse error.
parseError :: [Token] -> a
parseError _ = error "Parse error"
}
