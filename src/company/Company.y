{
module AsmParser where
import Assembly
import AsmToken
}

---------------------------------------------------------------------
-- Miserable Parser.
-- In the generated code we get a function  
--   calc :: [AsmToken] -> AsmProgram
%name           asmParse

-- The type of the input tokens.
%tokentype      { AsmToken }

-- Function to call when there is a syntax error.
%error          { syntaxError }

-- Specify how we will refer to the data contructors that represent
-- each token when defining the grammar.
%token
NUM     { AsmTokenNum   $$      }
ID      { AsmTokenId    $$      }
'('     { AsmTokenParenOpen     }
')'     { AsmTokenParenClose    }
LC      { AsmTokenLc            }
LD      { AsmTokenLd            }
ST      { AsmTokenSt            }
ADD     { AsmTokenAdd           }
SUB     { AsmTokenSub           }
MUL     { AsmTokenMul           }
DIV     { AsmTokenDiv           }
LT      { AsmTokenLt            }
GT      { AsmTokenGt            }
EQ      { AsmTokenEq            }
BR      { AsmTokenBr            }
RET     { AsmTokenRet           }
CALL    { AsmTokenCall          }

-- Grammar for Language below
%%
Program         : '(' Functions ')'                     { $2                    }

Functions       : {-empty-}                             { []                    }
                | Function Functions                    { $1 : $2               }

Function        : '(' Id Args Blocks ')'                { AsmFunction $2 $3 $4 	}

Args            : '(' IdList ')'                        { $2                    }

IdList          : {-empty-}                             { []                    }
                | Id IdList                             { $1 : $2               }

Blocks          : '(' Num Instructions ')'              { AsmBlock $2 $3 : []   }
                | '(' Num Instructions ')' Blocks       { AsmBlock $2 $3 : $5   }

Instructions    : Instruction                           { $1 : []               }
Instructions    : Instruction Instructions              { $1 : $2               }

Instruction     : '(' LC Reg Num ')'                    { AsmLc $3 $4           }
Instruction     : '(' LD Reg Id ')'                     { AsmLd $3 $4           }
Instruction     : '(' ST Id Reg ')'                     { AsmSt $3 $4           }
Instruction     : '(' ADD Reg Reg Reg ')'               { AsmAdd $3 $4 $5       }
Instruction     : '(' SUB Reg Reg Reg ')'               { AsmSub $3 $4 $5       }
Instruction     : '(' MUL Reg Reg Reg ')'               { AsmMul $3 $4 $5       }
Instruction     : '(' DIV Reg Reg Reg ')'               { AsmDiv $3 $4 $5       }
Instruction     : '(' LT Reg Reg Reg ')'                { AsmLt $3 $4 $5        }
Instruction     : '(' GT Reg Reg Reg ')'                { AsmGt $3 $4 $5        }
Instruction     : '(' EQ Reg Reg Reg ')'                { AsmEq $3 $4 $5        }
Instruction     : '(' BR Reg Num Num ')'                { AsmBr $3 $4 $5        }
Instruction     : '(' RET Reg ')'                       { AsmRet $3             }
Instruction     : '(' CALL Reg Id RegList ')'           { AsmCall $3 $4 $5      }

RegList         : {-empty-}                             { []                    }
RegList         : Reg RegList                           { $1 : $2               }

Num             : NUM                                   { $1                    }
Reg             : ID                                    { if (head $1) == 'r'
                                                            then  read (tail $1)
                                                            else syntaxError []
                                                        }
Id              : ID                                    { $1 	                }

{
-- Call this function when we get a parse error.
syntaxError :: [AsmToken] -> a
syntaxError _ = error "Syntax Error."
}
