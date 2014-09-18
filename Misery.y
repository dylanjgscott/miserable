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

-- Function to call when there is a syntax error.
%error          { syntaxError }

-- Specify how we will refer to the data contructors that represent
-- each token when defining the grammar.
%token
FUNCTION    { TokenFunction     }   
VARS        { TokenVars         }
';'         { TokenSemicolon    }       
','         { TokenComma        }   
BEGIN       { TokenBegin        }   
END         { TokenEnd          }
'='         { TokenEquals       }   
'('         { TokenParenOpen    }       
')'         { TokenParenClose   }       
IF          { TokenIf           }
THEN        { TokenThen         }
ELSE        { TokenElse         }
RETURN      { TokenReturn       }   
NUM         { TokenNum  $$      }   
ID          { TokenId   $$      }       
'+'         { TokenPlus         }
'-'         { TokenMinus        }
'*'         { TokenTimes        }   
'/'         { TokenDivide       }   
'<'         { TokenLT           }
'>'         { TokenGT           }
'=='        { TokenEQ           }

---- Don't need the rest of the individual tokens so grouping them all as OP    
-- OP           { TokenPlus 
--            TokenMinus    
--            TokenTimes    
--            TokenDivide
--            TokenLT       
--            TokenGT       
--            TokenEQ }

-- Grammer for Language below
%%
Program     : Functions                             { $1                    }

Functions   : {-empty-}                             { []                    } -- Epsilon
            | Function Functions                    { $1 : $2               }

Function    : FUNCTION IdProd Args Variables Block  { Function $2 $3 $4 $5  }

Args        : '(' {-empty-} ')'                     { Args []               } -- Epsilon
            | '(' IdList ')'                        { Args $2               }

Variables   : {-empty-}                             { Vars []               } --Epsilon
            | VARS IdList ';'                       { Vars $2               }

IdList      : IdProd                                { [$1]                  } 
            | IdProd ',' IdList                     { $1 : $3               }

Block       : BEGIN Statements END                  { Block $2              }

Statements  : {-empty-}                             { []                    } -- Epsilon
            | Statement ';' Statements              { $1 : $3               }

Statement   : IdProd '=' Exp                        { Assign $1 $3          } 
            | IF IdProd THEN Block                  { If $2 $4              }
            | IF IdProd THEN Block ELSE Block       { IfElse $2 $4 $6       }
            | RETURN IdProd                         { Return $2             }
       
Exp         : NumProd                               { ExpNum $1             }
            | IdProd                                { ExpId $1              }
            | IdProd Args                           { ExpFun $1 $2          }
            | '(' Exp '+' Exp ')'                   { ExpOp OpAdd $2 $4     }
            | '(' Exp '-' Exp ')'                   { ExpOp OpSub $2 $4     }
            | '(' Exp '*' Exp ')'                   { ExpOp OpMul $2 $4     }
            | '(' Exp '/' Exp ')'                   { ExpOp OpDiv $2 $4     }
            | '(' Exp '<' Exp ')'                   { ExpOp OpLT $2 $4      }
            | '(' Exp '>' Exp ')'                   { ExpOp OpGT $2 $4      }
            | '(' Exp '==' Exp ')'                  { ExpOp OpEq $2 $4      } 

IdProd      : ID                                    { $1                    }

NumProd     : NUM                                   { $1                    }

{
-- Call this function when we get a parse error.
syntaxError :: [Token] -> a
syntaxError _ = error "Syntax Error."
}
