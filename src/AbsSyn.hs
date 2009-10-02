module AbsSyn where

data Prog = P ProgPart [ProgPart] [ProgPart]

data ProgPart  = 
         TurtleStm     String           |
         VarDec        String           |
         VarDecAss     Statement        |
         FunDec        String [String] [ProgPart] [Statement]
         
         deriving Show

data Statement =
         Assignment    String Exp       
         deriving Show

data Exp = 
         PlusE  Exp Exp        |
         MinusE Exp Exp        |
         TimesE Exp Exp        |
         DivE   Exp Exp        |
         NegE   Exp            |
         FunCall String [Exp]  |
         IntE   Int            |
         IdentE String
         deriving Show
