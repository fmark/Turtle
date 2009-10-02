-- An example demonstrating how to connect a Happy parser to an Alex lexer.
{
import Tokenize
import PrettyPrint
import AbsSyn
}

%name parseTurtle
%tokentype { Token }

%token  turtle                { TTurtle _           }
        "<="                  { TComparOp _ "<="    }
        ">="                  { TComparOp _ ">="    }
        "=="                  { TComparOp _ "=="    }
        ">"                   { TComparOp _ ">"     }
        "<"                   { TComparOp _ "<"     }
        '='                   { TAssOp _            }
        '+'                   { TArithOp _ '+'      }
        '-'                   { TArithOp _ '-'      }
        '*'                   { TArithOp _ '*'      }
        '/'                   { TArithOp _ '/'      }
        '('                   { TBra _              }
        ')'                   { TKet _              }
        '{'                   { TCBra _             }
        '}'                   { TCKet _             }
        ','                   { TComma _            }
        var                   { TKeyword _ "var"    }
        fun                   { TKeyword _ "fun"    }
        if                    { TKeyword _ "if"     }
        else                  { TKeyword _ "else"   }
        while                 { TKeyword _ "while"  }
        return                { TKeyword _ "return" }
        up                    { TBuiltin _ "up"     }
        down                  { TBuiltin _ "down"   }
        moveto                { TBuiltin _ "moveto" }
        read                  { TBuiltin _ "read"   }
        int                   { TInt _ $$           }
        ident                 { TIdent _ $$         }

%left '+' '-'
%left '*' '/'
%left NEG

%%

Prog     :: { Prog }
Program     : turtle ident VarDecBlock FunDecBlock      { P (TurtleStm $2) (reverse $3) (reverse $4)  }

-- Need to parse lists in reverse order due to an implementation detail of happy
VarDecBlock : {- empty -}                     { []                }
            | VarDecBlock VarDec              { $2 : $1           }

VarDec      : var Assignment                  { VarDecAss $2      }
            | var ident                       { VarDec $2         }

FunDecBlock : {- empty -}                     { []                }
            | FunDecBlock FunDec              { $2 : $1           }

FunDec      : fun ident '(' IdentList ')' VarDecBlock '{' CmpStm '}' { FunDec $2 (reverse $4) (reverse $6) (reverse $8)}

IdentList  : {- empty -}                     { []                }
           | ident                           { [$1]              }
           | IdentList ',' ident             { $3 : $1           }

ExpList    : {- empty -}                     { []                }
           | Exp                             { [$1]              }
           | ExpList ',' Exp                 { $3 : $1           }

Assignment : ident '=' Exp                   { Assignment $1 $3  }

CmpStm     : {- empty -}                     { []                }
           | CmpStm Stm                      { $2 : $1           }

Stm        : Assignment                      { $1                }

Exp        : Exp '+' Exp                     { PlusE $1 $3       }
           | Exp '-' Exp                     { MinusE $1 $3      }
           | Exp '*' Exp                     { TimesE $1 $3      }
           | Exp '/' Exp                     { DivE $1 $3        }
           | ident '(' ExpList ')'           { FunCall $1 (reverse $3)}
           | ident                           { IdentE $1         }
           | int                             { IntE $1           }
           | '(' Exp ')'                     { $2                }
-- negation has highest precedence, not low like subtraction
           | '-' Exp %prec NEG               { NegE $2           }

{


-- Boilerplate code from http://darcs.haskell.org/alex/examples/tiny.y
main :: IO ()
--main = interact (show.runCalc)
main = interact (prettyPrint .runCalc)

runCalc :: String -> Prog
runCalc = parseTurtle . alexScanTokens

happyError :: [Token] -> a
happyError tks = error ("Parse error at " ++ lcn ++ "\n")
   where
   lcn = case tks of
            []   -> "end of file"
            tk:_ -> "line " ++ show l ++ ", column " ++ show c
                  where
                  AlexPn _ l c = token_posn tk
}
