{
import Tokenize
import PrettyPrint
import Desugar
import AbsSyn
import Translate
import System.Console.GetOpt
import System.Environment
import System.IO
import Data.Char (toLower)
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

FunDec      : fun ident '(' IdentList ')' VarDecBlock CmpStm { FunDec $2 (reverse $4) (reverse $6) $7}

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
           | ident '(' ExpList ')'           { FunCallStm $1 (reverse $3)}

Assignment : ident '=' Exp                   { Assignment $1 $3  }

Comparison : Exp "==" Exp                    { Equality $1 $3    }
           | Exp "!=" Exp                    { Inequality $1 $3  }
           | Exp "<"  Exp                    { LessThan $1 $3    }
           | Exp "<=" Exp                    { LessThanEq $1 $3  }
           | Exp ">"  Exp                    { GreaterThan $1 $3 }
           | Exp ">=" Exp                    { GreaterThanEq $1 $3 }

{
--Types and helpers for parsing the command line
data Flag  = Mode ModeT | Output String deriving Show
data ModeT = ParseM | DesugarM | DisasmM | BinaryM deriving (Show, Eq)
options :: [OptDescr Flag]
options =
    [ Option ['o']     ["output"]  (ReqArg Output "FILE")  "output FILE"
    , Option ['m']     ["mode"]    (ReqArg mode   "MODE")  "mode={parse|desugar|disasm|binary}"
    ]
    
mode :: String -> Flag
mode s = case s' of
          "parse"   -> Mode ParseM
          "desugar" -> Mode DesugarM
          "disasm"  -> Mode DisasmM
          "binary"  -> Mode BinaryM
          otherwise -> error $ "Mode must be one of the following values: parse, desugar, disasm, binary"
         where s' = map toLower s
           
header = "Usage: turtle [options] [file]...\nOptions:"

fstMode :: [Flag] -> Maybe ModeT
fstMode (f:fs) = case f of
                   (Mode m)  -> Just m
                   otherwise -> fstMode fs
fstMode []     = Nothing

fstOutp :: [Flag] -> Maybe String
fstOutp (f:fs) = case f of
                   (Output s) -> Just s
                   otherwise  -> fstOutp fs
fstOutp []     = Nothing

--Program entry point
main :: IO ()
main = do
  args <- getArgs
  (flags, nopts, errs) <- return (getOpt Permute options args)
  -- Ugh - is there a better way to have an if with no else here??
  if (length errs) > 0 then error ( concat errs ++ usageInfo header options) else putStr ""

  -- if no input file is specified on cmd line, read from stdin.
  inp <- case (length nopts) of
           0 -> getContents
           1 -> readFile (head nopts)
           otherwise -> error $ "Cannot specify more than one source file to compile.\n" ++ header

  -- read the flags to ascertain what sort of transformation we are doing
  outp <- case fstMode flags of
            Just ParseM   -> return ((prettyPrint . doParse) inp) 
            Just DesugarM -> return ((prettyPrint . desugar . doParse) inp) 
            Just DisasmM  -> return ((prettyPrintI . translate . desugar . doParse) inp)-- translate and print instructions
            otherwise     -> return ((unlines . (map show) . translateToBinary . translate . desugar . doParse) inp) -- translate and print instructions

  -- are we outputting to a file or stdout?
  case fstOutp flags of
    Just s  -> do
              hOut <- openFile s WriteMode
              hPutStr hOut outp
              hClose  hOut
    Nothing -> putStr outp

-- Boilerplate code from http://darcs.haskell.org/alex/examples/tiny.y
doParse :: String -> ProgPart
doParse = parseTurtle .alexScanTokens

happyError :: [Token] -> a
happyError tks = error ("Parse error at " ++ lcn ++ "\n")
   where
   lcn = case tks of
            []   -> "end of file"
            tk:_ -> "line " ++ show l ++ ", column " ++ show c
                  where
                  AlexPn _ l c = token_posn tk
}
