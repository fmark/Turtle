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

%expect 1

%%

Program     :: { Program }
Program     : turtle ident VarDecBlock FunDecBlock      { TurtleStm $2      }

VarDecBlock : {- empty -}                     { []                }
            | VarDec VarDecBlock              { $1 : $2           }


VarDec      : var ident '=' Exp               { VarDecAss (Assignment $2 $4) }
            | var ident                       { VarDec $2         }

FunDecBlock : {- empty -}                     { []                }
            | FunDec FunDecBlock              { $1 : $2           }

FunDec      : fun ident '(' FunDecArgs ')' VarDecBlock    { FunDec $2 $4 $6 }

FunDecArgs  : {- empty -}                     { []                }
            | ident                           { [$1]              }
            | ident ',' FunDecArgs            { $1 : $3           }
-- stmts : stmt                   { [$1] }
--      | stmts ';' stmt         { $3 : $1 }

Exp         : Exp '+' Exp                     { PlusE $1 $3       }
            | ident                           { IdentE $1         }
--Exp : let var '=' Exp in Exp   { LetE $2 $4 $6 }
--    | Exp1                     { $1            }
--
--Exp1 : Exp1 '+' Term           { PlusE  $1 $3  }
--     | Exp1 '-' Term           { MinusE $1 $3  }
--     | Term                    { $1            }
--
--Term : Term '*' Factor         { TimesE $1 $3  }
--     | Term '/' Factor         { DivE $1 $3    }
--     | Factor                  { $1            }
--
--Factor : '-' Atom              { NegE $2}
--       | Atom                  { $1}
--
--Atom : int                     { IntE $1       }
--    | var                      { VarE $1       }
--    | '(' Exp ')'              { $2            } 

{


-- Boilerplate code from http://darcs.haskell.org/alex/examples/tiny.y
main :: IO ()
--main = interact (show.runCalc)
main = interact (prettyPrint .runCalc)

runCalc :: String -> Program
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
