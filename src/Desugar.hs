module Desugar (desugar) where

-- import Data.List (intercalate)
import AbsSyn

-- TODO: figure out how to do this with overloading using type classes.
desugar :: Prog -> Prog
desugar (P s vars funcs main) = (P s (map desugarPP vars) (map desugarPP funcs) (map desugarS main))

desugarPP :: ProgPart -> ProgPart
-- All variable declarations with no explicit assignment should be assigned 0
desugarPP (VarDecDef s) = VarDec (Assignment s (IntE 0))
desugarPP (VarDec a) = VarDec a
desugarPP (FunDec f args vardecs body) = (FunDec f args (map desugarPP vardecs) (map desugarS body))

desugarS :: Statement -> Statement
desugarS (Assignment s e)   = Assignment s (desugarE e)
desugarS (MoveTo e1 e2)     = MoveTo (desugarE e1) (desugarE e2)
-- convert all ifs to ifelses
desugarS (If c ss)          = desugarS (IfElse c ss [])
-- remove all the comparison operators we added
desugarS (IfElse (Inequality    e1 e2) s1s s2s) = desugarS (IfElse (Equality e1 e2) s2s s1s)
desugarS (IfElse (GreaterThanEq e1 e2) s1s s2s) = desugarS (IfElse (LessThan e1 e2) s2s s1s)
desugarS (IfElse (GreaterThan   e1 e2) s1s s2s) = desugarS (IfElse (Equality e1 e2) s2s [(IfElse (LessThan e1 e2) s2s s1s)])
desugarS (IfElse (LessThanEq    e1 e2) s1s s2s) = desugarS (IfElse (Equality e1 e2) s1s [(IfElse (LessThan e1 e2) s1s s2s)])
-- default
desugarS (IfElse c s1s s2s) = IfElse (desugarC c) (map desugarS s1s) (map desugarS s2s)
desugarS (While c ss)       = While (desugarC c) (map desugarS ss)
desugarS (Return e)         = Return (desugarE e)
desugarS (FunCallStm s es)  = FunCallStm s (map desugarE es)
desugarS (Compound ss)      = Compound (map desugarS ss)
desugarS s = s

desugarE :: Exp -> Exp
desugarE (PlusE e1 e2)      = PlusE  (desugarE e1) (desugarE e2)
desugarE (MinusE e1 e2)     = MinusE (desugarE e1) (desugarE e2)
desugarE (TimesE e1 e2)     = TimesE (desugarE e1) (desugarE e2)
desugarE (NegE e)           = NegE (desugarE e)
desugarE (FunCall s es)     = FunCall s (map desugarE es)
-- need to implement division in terms of plus and mul
desugarE (DivE e1 e2)       = error "Division not implemented yet!"
desugarE e = e

desugarC :: Comparison -> Comparison
desugarC (Equality e1 e2)   = Equality (desugarE e1) (desugarE e2)
desugarC (LessThan e1 e2)   = LessThan (desugarE e1) (desugarE e2)

