module PrettyPrint (prettyPrint) where

import Data.List (intercalate)
import AbsSyn

prettyPrint :: ProgPart -> String
prettyPrint (Prog turt vars funcs main) = "turtle " ++ turt ++ "\n\n" ++ (ppsPrint vars) ++  "\n" ++ (ppsPrint funcs) ++ "\n{\n" ++  (ppsPrint main) ++ "}\n"
prettyPrint (VarDec a) = "var " ++ (prettyPrint a)
prettyPrint (FunDec f args vars stms) = "fun " ++ f ++ "(" 
            ++ (intercalate ", " args) ++ ")\n" 
            ++ (ppsPrint vars) ++ "{\n" ++ (ppsPrint stms) ++ "}\n"
prettyPrint (Assignment s e) = s ++ " = " ++ (prettyPrint e) ++ "\n"
prettyPrint (Up            ) = "up()\n"
prettyPrint (Down          ) = "down()\n"
prettyPrint (MoveTo e1 e2  ) = "moveto(" ++ (prettyPrint e1) ++ ", " ++ (prettyPrint e2) ++ ")\n"
prettyPrint (Read s        ) = "read(" ++ s ++ ")\n"
prettyPrint (Compound ss   ) = "{\n" ++ (ppsPrint ss) ++ "}\n"
prettyPrint (Return e      ) = "return " ++ (prettyPrint e) ++ "\n"
prettyPrint (If c ss       ) = "if (" ++ (prettyPrint c) ++ ") {\n" ++ (ppsPrint ss) ++ "}\n"
prettyPrint (IfElse c s1s s2s) = "if (" ++ (prettyPrint c) ++ ") {\n" ++ (ppsPrint s1s) ++ "} else {\n"  ++ (ppsPrint s2s) ++ "}\n"
prettyPrint (While c ss    ) = "while (" ++ (prettyPrint c) ++ ") {\n" ++ (ppsPrint ss) ++ "}\n"
prettyPrint (Equality      e1 e2) = (prettyPrint e1) ++ " == " ++ (prettyPrint e2)
prettyPrint (Inequality    e1 e2) = (prettyPrint e1) ++ " != " ++ (prettyPrint e2)
prettyPrint (LessThan      e1 e2) = (prettyPrint e1) ++ " < " ++ (prettyPrint e2)
prettyPrint (LessThanEq    e1 e2) = (prettyPrint e1) ++ " <= " ++ (prettyPrint e2)
prettyPrint (GreaterThan   e1 e2) = (prettyPrint e1) ++ " > " ++ (prettyPrint e2)
prettyPrint (GreaterThanEq e1 e2) = (prettyPrint e1) ++ " >= " ++ (prettyPrint e2)
prettyPrint (PlusE e1 e2) = (prettyPrint e1) ++ " + " ++ (prettyPrint e2)
prettyPrint (MinusE e1 e2) = (prettyPrint e1) ++ " - " ++ (prettyPrint e2)
prettyPrint (TimesE e1 e2) = (prettyPrint e1) ++ " * " ++ (prettyPrint e2)
prettyPrint (DivE e1 e2) = (prettyPrint e1) ++ " / " ++ (prettyPrint e2)
prettyPrint (NegE e) = "-" ++ (prettyPrint e)
prettyPrint (IdentE i) = i
prettyPrint (IntE i) = show i
prettyPrint (FunCall f params) = f ++ "(" 
            ++ (intercalate ", " (map prettyPrint params)) ++ ")"


ppsPrint :: [ProgPart] -> String
ppsPrint (s:ss) = (prettyPrint s) ++ (ppsPrint ss)
ppsPrint [] = ""
