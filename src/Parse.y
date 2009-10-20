{
import Tokenize
import PrettyPrint
import Desugar
import AbsSyn
import Translate
}

%name parseTurtle
%tokentype { Token }

%token  turtle                { TTurtle _           }
        "<="                  { TComparOp _ "<="    }
        ">="                  { TComparOp _ ">="    }
        "=="                  { TComparOp _ "=="    }
        "!="                  { TComparOp _ "!="    }
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

-- Program    :: { ProgPart }
Program     : turtle ident VarDecBlock FunDecBlock CmpStm     { Prog $2 (reverse $3) (reverse $4) $5 }

-- Need to parse lists in reverse order due to an implementation detail of happy
VarDecBlock : {- empty -}                     { []                }
            | VarDecBlock VarDec              { $2 : $1           }

VarDec      : var Assignment                  { VarDec $2      }
            | var ident                       { VarDecDef $2   }

FunDecBlock : {- empty -}                     { []                }
            | FunDecBlock FunDec              { $2 : $1           }

FunDec      : fun ident '(' IdentList ')' VarDecBlock CmpStm { FunDec $2 (reverse $4) (reverse $6) ($7 ++ [(Return (IntE 0))])}

IdentList  : {- empty -}                     { []                }
           | ident                           { [$1]              }
           | IdentList ',' ident             { $3 : $1           }

ExpList    : {- empty -}                     { []                }
           | Exp                             { [$1]              }
           | ExpList ',' Exp                 { $3 : $1           }

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

CmpStm     : '{' StmList '}'                 { reverse $2        }

StmList    : {- empty -}                     { []                }
           | StmList Stm                     { $2 : $1           }

Stm        : up   '(' ')'                    { Up                }
           | up                              { Up                }
           | down '(' ')'                    { Down              }
           | down                            { Down              }
           | moveto '(' Exp ',' Exp ')'      { MoveTo $3 $5      }
           | read '(' ident ')'              { Read $3           }
           | Assignment                      { $1                }
           | CmpStm                          { Compound $1       }
           | return Exp                      { Return $2         }
           | if '(' Comparison ')' CmpStm    { If $3 $5          }
           | if '(' Comparison ')' CmpStm else CmpStm   { IfElse $3 $5 $7 }
           | while '(' Comparison ')' CmpStm { While $3 $5       }
           | ident '(' ExpList ')'           { FunCall $1 (reverse $3)}

Assignment : ident '=' Exp                   { Assignment $1 $3  }

Comparison : Exp "==" Exp                    { Equality $1 $3    }
           | Exp "!=" Exp                    { Inequality $1 $3  }
           | Exp "<"  Exp                    { LessThan $1 $3    }
           | Exp "<=" Exp                    { LessThanEq $1 $3  }
           | Exp ">"  Exp                    { GreaterThan $1 $3 }
           | Exp ">=" Exp                    { GreaterThanEq $1 $3 }

{


-- Boilerplate code from http://darcs.haskell.org/alex/examples/tiny.y
main :: IO ()

--main = interact (prettyPrint . desugar . runCalc) -- just desugar and print source-code
main = interact (prettyPrintI . translate . desugar . runCalc) -- translate and print instructions



runCalc :: String -> ProgPart
runCalc = parseTurtle .alexScanTokens

happyError :: [Token] -> a
happyError tks = error ("Parse error at " ++ lcn ++ "\n")
   where
   lcn = case tks of
            []   -> "end of file"
            tk:_ -> "line " ++ show l ++ ", column " ++ show c
                  where
                  AlexPn _ l c = token_posn tk
}
