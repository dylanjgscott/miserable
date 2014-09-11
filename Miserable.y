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
VARS		{ TokenVars			}
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
NUM 		{ TokenNum	$$		}	
ID 	 		{ TokenId 	$$		}		
'+'			{ TokenPlus			}
'-' 		{ TokenMinus		}
'*'			{ TokenTimes		}	
'/'			{ TokenDivide		}	
'<'			{ TokenLT			}
'>'			{ TokenGT			}
'=='		{ TokenEQ 			}

---- Don't need the rest of the individual tokens so grouping them all as OP	
-- OP 			{ TokenPlus	
--			  TokenMinus	
--			  TokenTimes	
--			  TokenDivide
--			  TokenLT		
--			  TokenGT		
--			  TokenEQ }

-- Grammer for Language below
%%
Program 	: Functions							{ $1				}

Functions	: {-empty-}							{ []				} -- Epsilon
			| Function Functions				{ $1 $2				}

Function 	: FUNCTION ID Args Variables Block	{ Function $2 $3 $4 $5	}

Args		: '(' {-empty-} ')'					{ []				} -- Epsilon
			| '(' IdList ')'					{ $2				}

Variables 	: {-empty-}							{ []				} --Epsilon
			| VARS IdList ';'					{ $2				}

IdList 		: ID 								{ IdList $1		} --From happy docs - i think think this is right
			| ID ',' IdList 					{ $1 : $2		}

Block 		: BEGIN Statements END 				{ $2				}

Statements  : {-empty-}							{ []				} -- Epsilon
			| Statement ';' Statements 			{ $1 : $2			}

Statement 	: ID '=' Exp 						{ Assign $1 $3		} 
			| IF ID THEN Block 					{ If $2 $4			}
			| IF ID THEN Block ELSE Block 		{ IfElse $2 $4 $6	}
			| RETURN ID 						{ Return $2			}

Exp 		: NUM 								{ ExpNum $1 		}
			| ID 								{ ExpId $1 			}
			| ID Args 							{ $1  $2			}
			| '(' Exp '+' Exp ')'				{ ExpOp OpAdd $2 $4	}
			| '(' Exp '-' Exp ')'				{ ExpOp OpSub $2 $4	}
			| '(' Exp '*' Exp ')'				{ ExpOp OpMul $2 $4	}
			| '(' Exp '/' Exp ')'				{ ExpOp OpDiv $2 $4	}
			| '(' Exp '<' Exp ')'				{ ExpOp OpLT $2 $4	}
			| '(' Exp '>' Exp ')'				{ ExpOp OpGT $2 $4	}
			| '(' Exp '==' Exp ')'				{ ExpOp OpEq $2 $4	} 

--			| '(' Exp '+' Exp ')'				{ ExpOp $3 $2 $4	}
--			| '(' Exp '-' Exp ')'				{ ExpOp $3 $2 $4	}
--			| '(' Exp '*' Exp ')'				{ ExpOp $3 $2 $4	}
--			| '(' Exp '/' Exp ')'				{ ExpOp $3 $2 $4	}
--			| '(' Exp '<' Exp ')'				{ ExpOp $3 $2 $4	}
--			| '(' Exp '>' Exp ')'				{ ExpOp $3 $2 $4	}
--			| '(' Exp '==' Exp ')'				{ ExpOp $3 $2 $4	}

--			| '(' Exp OP Exp ')'				{ ExpOp $3  $2 $4	}


{
-- Call this function when we get a parse error.
parseError :: [Token] -> a
parseError _ = error "Parse error"
}
