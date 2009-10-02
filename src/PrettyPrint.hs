module PrettyPrint (prettyPrint) where

import Data.List (intercalate)
import AbsSyn

prettyPrint :: Prog -> String
prettyPrint (P turt vars funcs) = (pPrint turt) ++ "\n" ++ (psPrint vars) ++  "\n" ++ (psPrint funcs)

pPrint :: ProgPart -> String
pPrint (TurtleStm s) = "turtle " ++ s ++ " \n"
pPrint (VarDec s) = "var " ++ s ++ "\n"
pPrint (VarDecAss a) = "var " ++ (sPrint a)
pPrint (FunDec f args vars stms) = "fun " ++ f ++ "(" 
            ++ (intercalate ", " args) ++ ")\n" 
            ++ (psPrint vars) ++ "{\n" ++ (ssPrint stms) ++ "}\n"

sPrint :: Statement -> String
sPrint (Assignment s e) = s ++ " = " ++ (prettyPrintExp e) ++ "\n"

ssPrint :: [Statement] -> String
ssPrint (s:ss) = (sPrint s) ++ (ssPrint ss)
ssPrint [] = ""



psPrint :: [ProgPart] -> String
psPrint (p:ps) = (pPrint p) ++ (psPrint ps)
psPrint [] = ""

prettyPrintExp :: Exp -> String
prettyPrintExp (PlusE e1 e2) = (prettyPrintExp e1) ++ " + " ++ (prettyPrintExp e2)
prettyPrintExp (MinusE e1 e2) = (prettyPrintExp e1) ++ " - " ++ (prettyPrintExp e2)
prettyPrintExp (TimesE e1 e2) = (prettyPrintExp e1) ++ " * " ++ (prettyPrintExp e2)
prettyPrintExp (DivE e1 e2) = (prettyPrintExp e1) ++ " / " ++ (prettyPrintExp e2)
prettyPrintExp (NegE e) = "-" ++ (prettyPrintExp e)
prettyPrintExp (IdentE i) = i
prettyPrintExp (IntE i) = show i

