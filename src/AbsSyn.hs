module AbsSyn where

data Program  = 
         TurtleStm     String           |
         VarDec        String VarDecAss 
         deriving Show

data VarDecAss =
         Uninited                       |
         InitVal Exp
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
--         FunCall               |
         IntE   Int            |
         IdentE String
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
