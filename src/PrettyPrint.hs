module PrettyPrint (prettyPrint) where

import Data.List (intercalate)
import AbsSyn

prettyPrint :: Prog -> String
prettyPrint (P turt vars funcs) = (pPrint turt) ++ "\n" ++ (psPrint vars) ++  "\n" ++ (psPrint funcs)

pPrint :: ProgPart -> String
pPrint (TurtleStm s) = "turtle " ++ (show s) ++ " \n"
pPrint (VarDec s) = "var " ++ (show s) ++ "\n"
pPrint (VarDecAss (Assignment s e)) = "var " ++ (show s) ++ " = " ++ (prettyPrintExp e) ++ "\n"
pPrint (FunDec f args vars) = "fun " ++ (show f) ++ "(" ++ (intercalate ", " (map show args)) ++ ")\n" ++ (psPrint vars)

psPrint :: [ProgPart] -> String
psPrint (p:ps) = (pPrint p) ++ (psPrint ps)
psPrint [] = ""

prettyPrintExp :: Exp -> String
prettyPrintExp (PlusE e1 e2) = (show e1) ++ " + " ++ (show e2)
prettyPrintExp (IdentE i) = show i

