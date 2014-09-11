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
Program     : Functions                         { Program $1                }

Functions   : {-empty-}                         { EmptyFunction     } -- Epsilon
            | Function Functions                { Functions $1 $2               }

Function    : FUNCTION IDProd Args Variables Block  { Function $2 $3 $4 $5  }

Args        : '(' {-empty-} ')'                 { ArgsEmpty             } -- Epsilon
            | '(' IdList ')'                    { Args $2               }

Variables   : {-empty-}                         { VarsEmpty             } --Epsilon
            | VARS IdList ';'                   { Vars $2               }

IdList      : IDProd                                { IdListSingle $1     } 
            | IDProd ',' IdList                     { IdList $1 $3       }

Block       : BEGIN Statements END              { Block $2      }

Statements  : {-empty-}                         { EmptyStatements       } -- Epsilon
            | Statement ';' Statements          { Statements $1 $3           }

Statement   : IDProd '=' Exp                        { Assign $1 $3      } 
            | IF IDProd THEN Block                  { If $2 $4          }
            | IF IDProd THEN Block ELSE Block       { IfElse $2 $4 $6   }
            | RETURN IDProd                         { Return $2         }

Exp         : NUM                               { ExpNum (Num $1)         }
            | IDProd                                { ExpId $1          }
            | IDProd Args                           { ExpFun $1 $2 }
            | '(' Exp '+' Exp ')'               { ExpOp OpAdd $2 $4 }
            | '(' Exp '-' Exp ')'               { ExpOp OpSub $2 $4 }
            | '(' Exp '*' Exp ')'               { ExpOp OpMul $2 $4 }
            | '(' Exp '/' Exp ')'               { ExpOp OpDiv $2 $4 }
            | '(' Exp '<' Exp ')'               { ExpOp OpLT $2 $4  }
            | '(' Exp '>' Exp ')'               { ExpOp OpGT $2 $4  }
            | '(' Exp '==' Exp ')'              { ExpOp OpEq $2 $4  } 

IDProd      : ID                                { Id $1             }

--          | '(' Exp '+' Exp ')'               { ExpOp $3 $2 $4    }
--          | '(' Exp '-' Exp ')'               { ExpOp $3 $2 $4    }
--          | '(' Exp '*' Exp ')'               { ExpOp $3 $2 $4    }
--          | '(' Exp '/' Exp ')'               { ExpOp $3 $2 $4    }
--          | '(' Exp '<' Exp ')'               { ExpOp $3 $2 $4    }
--          | '(' Exp '>' Exp ')'               { ExpOp $3 $2 $4    }
--          | '(' Exp '==' Exp ')'              { ExpOp $3 $2 $4    }

--          | '(' Exp OP Exp ')'                { ExpOp $3  $2 $4   }


{
-- Call this function when we get a parse error.
parseError :: [Token] -> a
parseError _ = error "Parse error"
}
