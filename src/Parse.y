-- An example demonstrating how to connect a Happy parser to an Alex lexer.
{
import Tokenize
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

%%

Program :: { Program }
Program     : turtle ident VarDecBlock        { TurtleStm $2      }

VarDecBlock : {- empty -}                     { []                }
            | VarDecBlock VarDec              { $2 : $1           }

VarDec      : var ident                       { VarDec $2         }
--            | var Assignment                  { VarDecAss $2      }

--Assignment  : ident '='                       { Assignment $1     }

--Exp :: { Exp }
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
data Program = 
         TurtleStm     String           |
         VarDec        String           |
         VarDecAss     String           |
         Assignment    String      
         deriving Show

-- data Exp =
--   LetE   String Exp Exp |
--   PlusE  Exp Exp        |
--   MinusE Exp Exp        |
--   TimesE Exp Exp        |
--   DivE   Exp Exp        |
--   NegE   Exp            |
--   IntE   Int            |
--   VarE   String
--   deriving Show


-- Boilerplate code from http://darcs.haskell.org/alex/examples/tiny.y
main :: IO ()
main = interact (show.runCalc)

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
