module Desugar (desugar) where

-- import Data.List (intercalate)
import AbsSyn

desugar :: ProgPart -> ProgPart
desugar (Prog s vars funcs main) = (Prog s (map desugar vars) (map desugar funcs) (map desugar main))
-- All variable declarations with no explicit assignment should be assigned 0
desugar (VarDecDef s) = VarDec (Assignment s (IntE 0))
desugar (VarDec a) = VarDec a
desugar (FunDec f args vardecs body) = (FunDec f args (map desugar vardecs) (map desugar body))
desugar (Assignment s e)   = Assignment s (desugar e)
desugar (MoveTo e1 e2)     = MoveTo (desugar e1) (desugar e2)
-- convert all ifs to ifelses
desugar (If c ss)          = desugar (IfElse c ss [])
-- remove all the comparison operators we added
desugar (IfElse (Inequality    e1 e2) s1s s2s) = desugar (IfElse (Equality e1 e2) s2s s1s)
desugar (IfElse (GreaterThanEq e1 e2) s1s s2s) = desugar (IfElse (LessThan e1 e2) s2s s1s)
desugar (IfElse (GreaterThan   e1 e2) s1s s2s) = desugar (IfElse (Equality e1 e2) s2s [(IfElse (LessThan e1 e2) s2s s1s)])
desugar (IfElse (LessThanEq    e1 e2) s1s s2s) = desugar (IfElse (Equality e1 e2) s1s [(IfElse (LessThan e1 e2) s1s s2s)])
-- default
desugar (IfElse c s1s s2s) = IfElse (desugar c) (map desugar s1s) (map desugar s2s)
desugar (While c ss)       = While (desugar c) (map desugar ss)
desugar (Return e)         = Return (desugar e)
desugar (Compound ss)      = Compound (map desugar ss)
desugar (PlusE e1 e2)      = PlusE  (desugar e1) (desugar e2)
desugar (MinusE e1 e2)     = MinusE (desugar e1) (desugar e2)
desugar (TimesE e1 e2)     = TimesE (desugar e1) (desugar e2)
desugar (NegE e)           = NegE (desugar e)
desugar (FunCall s es)     = FunCall s (map desugar es)
-- need to implement division in terms of plus and mul
desugar (DivE e1 e2)       = error "Division not implemented yet!"
desugar (Equality e1 e2)   = Equality (desugar e1) (desugar e2)
desugar (LessThan e1 e2)   = LessThan (desugar e1) (desugar e2)

desugar e = e -- catchall

