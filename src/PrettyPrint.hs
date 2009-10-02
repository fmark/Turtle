module PrettyPrint (prettyPrint) where

import Data.List (intercalate)
import AbsSyn

prettyPrint :: Prog -> String
prettyPrint (P turt vars funcs main) = "turtle " ++ turt ++ "\n\n" ++ (psPrint vars) ++  "\n" ++ (psPrint funcs) ++ "\n{\n" ++  (ssPrint main) ++ "}\n"

pPrint :: ProgPart -> String
pPrint (VarDec s) = "var " ++ s ++ "\n"
pPrint (VarDecAss a) = "var " ++ (sPrint a)
pPrint (FunDec f args vars stms) = "fun " ++ f ++ "(" 
            ++ (intercalate ", " args) ++ ")\n" 
            ++ (psPrint vars) ++ "{\n" ++ (ssPrint stms) ++ "}\n"

sPrint :: Statement -> String
sPrint (Assignment s e) = s ++ " = " ++ (ePrint e) ++ "\n"
sPrint (Up            ) = "up()\n"
sPrint (Down          ) = "down()\n"
sPrint (MoveTo e1 e2  ) = "moveto(" ++ (ePrint e1) ++ ", " ++ (ePrint e2) ++ ")\n"
sPrint (Read s        ) = "read(" ++ s ++ ")\n"
sPrint (Compound ss   ) = "{\n" ++ (ssPrint ss) ++ "}\n"
sPrint (Return e      ) = "return " ++ (ePrint e) ++ "\n"
sPrint (If c ss       ) = "if (" ++ (cPrint c) ++ ") {\n" ++ (ssPrint ss) ++ "}\n"
sPrint (IfElse c s1s s2s) = "if (" ++ (cPrint c) ++ ") {\n" ++ (ssPrint s1s) ++ "} else {\n"  ++ (ssPrint s2s) ++ "}\n"
sPrint (While c ss    ) = "while (" ++ (cPrint c) ++ ") {\n" ++ (ssPrint ss) ++ "}\n"
sPrint (FunCallStm f es) = f ++ "(" ++ (intercalate ", " (map ePrint es)) ++ ")\n"

cPrint :: Comparison -> String
cPrint (Equality      e1 e2) = (ePrint e1) ++ " == " ++ (ePrint e2)
cPrint (LessThan      e1 e2) = (ePrint e1) ++ " < " ++ (ePrint e2)
cPrint (LessThanEq    e1 e2) = (ePrint e1) ++ " <= " ++ (ePrint e2)
cPrint (GreaterThan   e1 e2) = (ePrint e1) ++ " > " ++ (ePrint e2)
cPrint (GreaterThanEq e1 e2) = (ePrint e1) ++ " >= " ++ (ePrint e2)

ssPrint :: [Statement] -> String
ssPrint (s:ss) = (sPrint s) ++ (ssPrint ss)
ssPrint [] = ""



psPrint :: [ProgPart] -> String
psPrint (p:ps) = (pPrint p) ++ (psPrint ps)
psPrint [] = ""

ePrint :: Exp -> String
ePrint (PlusE e1 e2) = (ePrint e1) ++ " + " ++ (ePrint e2)
ePrint (MinusE e1 e2) = (ePrint e1) ++ " - " ++ (ePrint e2)
ePrint (TimesE e1 e2) = (ePrint e1) ++ " * " ++ (ePrint e2)
ePrint (DivE e1 e2) = (ePrint e1) ++ " / " ++ (ePrint e2)
ePrint (NegE e) = "-" ++ (ePrint e)
ePrint (IdentE i) = i
ePrint (IntE i) = show i
ePrint (FunCall f params) = f ++ "(" 
            ++ (intercalate ", " (map ePrint params)) ++ ")"


