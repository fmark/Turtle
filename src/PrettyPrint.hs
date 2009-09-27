module PrettyPrint (prettyPrint) where

import AbsSyn

-- prettyPrint :: Prog -> String
-- prettyPrint (P p1 p2) = (pPrint p1) ++ (pPrint p2)

-- pPrint :: ProgPart -> String
-- pPrint (TurtleStm s) = "turtle " ++ (show s) ++ " \n"
-- pPrint (VarDec s Uninited) = "var " ++ (show s) ++ "\n"
-- pPrint (VarDec s (InitVal v)) = "var " ++ (show s) ++ "= " ++ (prettyPrintExp v) ++ "\n"

prettyPrint :: Program -> String
prettyPrint (TurtleStm s) = "turtle " ++ (show s) ++ " \n"
prettyPrint (VarDec s Uninited) = "var " ++ (show s) ++ "\n"
prettyPrint (VarDec s (InitVal v)) = "var " ++ (show s) ++ "= " ++ (prettyPrintExp v) ++ "\n"


prettyPrintExp :: Exp -> String
prettyPrintExp (PlusE e1 e2) = (show e1) ++ " + " ++ (show e2)
prettyPrintExp (IdentE i) = show i

