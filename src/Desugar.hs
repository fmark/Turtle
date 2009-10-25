module Desugar (desugar) where

-- import Data.List (intercalate)
import AbsSyn
import Debug.Trace

desugar :: ProgPart -> ProgPart
desugar (Prog s vars funcs main) = (Prog s (map desugar vars) (map desugar funcs') (map desugar main))
    where
      -- If the division operator is used, inject a division function 
      -- into the source-code
      funcs' = if hasDiv (Prog s vars funcs main) then
                   funcs ++ [divFunc]
               else
                   funcs

-- convert all ifs to ifelses
desugar (If c ss)          = desugar (IfElse c ss [])
-- remove all the comparison operators we added
desugar (IfElse (Inequality    e1 e2) s1s s2s) = desugar (IfElse (Equality e1 e2) s2s s1s)
desugar (IfElse (GreaterThanEq e1 e2) s1s s2s) = desugar (IfElse (LessThan e1 e2) s2s s1s)
desugar (IfElse (GreaterThan   e1 e2) s1s s2s) = desugar (IfElse (Equality e1 e2) s2s [(IfElse (LessThan e1 e2) s2s s1s)])
desugar (IfElse (LessThanEq    e1 e2) s1s s2s) = desugar (IfElse (Equality e1 e2) s1s [(IfElse (LessThan e1 e2) s1s s2s)])
-- If we have an IfElse that conforms to the core lang, then proceed
desugar (IfElse c s1s s2s) = IfElse (desugar c) (map desugar s1s) (map desugar s2s)

-- need to implement division in terms of plus and mul
desugar (DivE e1 e2)       = desugar (FunCall "!div" [e1, e2])

-- All variable declarations with no explicit assignment should be assigned 0
desugar (VarDecDef s) = desugar (VarDec (Assignment s (IntE 0)))
desugar (VarDec a) = VarDec (desugar a)
-- No desugaring required, but need to recurse
desugar (FunDec f args vardecs body) = (FunDec f args (map desugar vardecs) (map desugar body))
desugar (Assignment s e)   = Assignment s (desugar e)
desugar (MoveTo e1 e2)     = MoveTo (desugar e1) (desugar e2)
desugar (While c ss)       = While (desugar c) (map desugar ss)
desugar (Return e)         = Return (desugar e)
desugar (FunCallStm s es)  = FunCallStm s (map desugar es)
desugar (Compound ss)      = Compound (map desugar ss)
desugar (PlusE e1 e2)      = PlusE  (desugar e1) (desugar e2)
desugar (MinusE e1 e2)     = MinusE (desugar e1) (desugar e2)
desugar (TimesE e1 e2)     = TimesE (desugar e1) (desugar e2)
desugar (NegE e)           = NegE (desugar e)
desugar (FunCall s es)     = FunCall s (map desugar es)
desugar (Equality e1 e2)   = Equality (desugar e1) (desugar e2)
desugar (LessThan e1 e2)   = LessThan (desugar e1) (desugar e2)
desugar (IntE n)           = IntE n
desugar (IdentE v)         = IdentE v
desugar Up                 = Up
desugar Down               = Down
desugar (Read s)           = Read s



hasDiv :: ProgPart -> Bool
hasDiv (Prog s vars funcs main) = or ((map hasDiv vars) ++ (map hasDiv funcs) ++ (map hasDiv main))
hasDiv (IfElse c s1s s2s) = or ((hasDiv c):((map hasDiv s1s) ++ (map hasDiv s2s)))
hasDiv (If c ss)          = or ((hasDiv c):(map hasDiv ss))
hasDiv (DivE e1 e2)       = True
hasDiv (VarDec a)         = hasDiv a
hasDiv (FunDec f args vardecs body) = or ((map hasDiv vardecs)++(map hasDiv body))
hasDiv (Assignment s e)   = hasDiv e
hasDiv (MoveTo e1 e2)     = (hasDiv e1) || (hasDiv e2)
hasDiv (While c ss)       = or ((hasDiv c):(map hasDiv ss))
hasDiv (Return e)         = hasDiv e
hasDiv (FunCallStm s es)  = or (map hasDiv es)
hasDiv (Compound ss)      = or (map hasDiv ss)
hasDiv (PlusE e1 e2)      = (hasDiv e1) || (hasDiv e2)
hasDiv (MinusE e1 e2)     = (hasDiv e1) || (hasDiv e2)
hasDiv (TimesE e1 e2)     = (hasDiv e1) || (hasDiv e2)
hasDiv (NegE e)           = (hasDiv e)
hasDiv (FunCall s es)     = or (map hasDiv es)
hasDiv (Equality e1 e2)   = (hasDiv e1) || (hasDiv e2)
hasDiv (LessThan e1 e2)   = (hasDiv e1) || (hasDiv e2)
hasDiv (Inequality    e1 e2) = (hasDiv e1) || (hasDiv e2)
hasDiv (GreaterThanEq e1 e2) = (hasDiv e1) || (hasDiv e2)
hasDiv (GreaterThan   e1 e2) = (hasDiv e1) || (hasDiv e2)
hasDiv (LessThanEq    e1 e2) = (hasDiv e1) || (hasDiv e2)
hasDiv (IntE n)           = False
hasDiv (IdentE v)         = False
hasDiv Up                 = False
hasDiv Down               = False
hasDiv (Read s)           = False

divFunc :: ProgPart
-- Implements the following function from divtst.t:
-- fun div (dividend, divisor)
--   var quotient = 0
-- {
--   while (divisor < dividend) {
--     quotient = quotient + 1
--     dividend = dividend - divisor
--   }
--   if (dividend + dividend < divisor) {
--     return quotient }
--   else {
--     return quotient + 1 }
-- }

divFunc = (FunDec 
            "!div" 
            ["!dividend", "!divisor"] 
            [(VarDec (Assignment "!quotient" (IntE 0)))]
            [(While (LessThan (IdentE "!divisor") (IdentE "!dividend"))
             [(Assignment 
               "!quotient" 
               (PlusE (IdentE "!quotient") (IntE 1))),
              (Assignment 
               "!dividend" 
               (MinusE (IdentE "!dividend") (IdentE "!divisor")))]),
             (IfElse (LessThan 
                      (PlusE (IdentE "!dividend") (IdentE "!dividend"))
                      (IdentE "!divisor"))
               [(Return (IdentE "!quotient"))]
               [(Return (PlusE (IdentE "!quotient") (IntE 1)))])
            ]
           )
