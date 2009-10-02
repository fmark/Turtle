module AbsSyn where

data Prog = P String [ProgPart] [ProgPart] [Statement]

data ProgPart  = 
         VarDec         Statement        |
         FunDec        String [String] [ProgPart] [Statement]
         
         deriving Show

data Statement =
         Assignment    String Exp        |
         Up                              |
         Down                            |
         MoveTo Exp Exp                  |
         Read String                     |       
         If Comparison [Statement]       |
         IfElse Comparison [Statement] [Statement]  |
         While Comparison [Statement]    |
         Return Exp                      |
         FunCallStm String [Exp]         |
         Compound     [Statement]
         deriving Show

data Exp = 
         PlusE   Exp Exp                |
         MinusE  Exp Exp                |
         TimesE  Exp Exp                |
         DivE    Exp Exp                |
         NegE    Exp                    |
         FunCall String [Exp]           |
         IntE    Int                    |
         IdentE  String
         deriving Show

data Comparison =
         Equality      Exp Exp          |
         Inequality    Exp Exp          |
         LessThan      Exp Exp          |
         LessThanEq    Exp Exp          |
         GreaterThan   Exp Exp          |
         GreaterThanEq Exp Exp          
         deriving Show
         