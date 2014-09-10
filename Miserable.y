---------------------------------------------------------------------
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
-- Miserable Parser.
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
FUNCTION	{ TokenFunction		}	
VARS		{ TokenVars		$$	}
';'			{ TokenSemicolon	}		
','			{ TokenComma		}	
BEGIN		{ TokenBegin		}	
END 		{ TokenEnd			}
'='			{ TokenEquals		}	
'(' 		{ TokenParenOpen	}		
')' 		{ TokenParenClose	}		
IF 			{ TokenIf			}
THEN 		{ TokenThen			}
ELSE 		{ TokenElse			}
RETURN 		{ TokenReturn		}	
int 		{ TokenNum Int	$$	}	
Id 		 	{ TokenId String 	}		
'+'			{ TokenPlus			}
'-'			{ TokenMinus		}	
'*' 		{ TokenTimes		}	
'/' 		{ TokenDivide		}	
'<' 		{ TokenLT			}
'>'			{ TokenGT			}
'=='		{ TokenEQ			}
-- Grammer for Language below
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
