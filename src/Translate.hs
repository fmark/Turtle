module Translate (translate) where

import Data.List (intersect)
import AbsSyn

hasDups :: Eq a => [a] -> Bool
-- Inefficient, maybe replace with something smarter
-- hasDups a = if length a == length (nub a) then False else True
hasDups l                   = hasDups' l []  
  where
    hasDups' [] _           = False
    hasDups' (x:xs) ls
        | x `elem` ls   = True
        | otherwise     = hasDups' xs (x:ls)

-- Will error if hasDups == False
firstDup :: Eq a => [a] -> a
firstDup l                   = firstDup' l []  
  where
    firstDup' (x:xs) ls
        | x `elem` ls   = x
        | otherwise     = firstDup' xs (x:ls)


hasKey :: Eq a => a -> [(a, b)] -> Bool
hasKey a abs = elem a (map fst abs)

findVals :: Eq a => a -> [(a, b)] -> [b]
findVals a abs = (map snd (filter (\ab -> a == (fst ab)) abs))

-- ensures exactly one val is returned.  Use findVals if another
-- number of vals could be valid
getVal :: Eq a => a -> [(a, b)] -> b
getVal a abs = head (findVals a abs)

pushF :: (String, (Int, Int)) -> [(String, (Int, Int))] -> [(String, (Int, Int))]
pushF f ftab = if hasKey (fst f) ftab then 
                   error $ "pushing duplicate function \"" ++ (fst f) ++ "\" onto table."
               else f:ftab

pushV :: (String, Int) -> [(String, Int)] -> [(String, Int)]
pushV v vtab = if hasKey (fst v) vtab then 
                   error $ "pushing duplicate variable \"" ++ (fst v) ++ "\" onto table."
               else v:vtab

pushVs :: [(String, Int)] -> [(String, Int)] -> [(String, Int)]
pushVs vs vtab = if length dups > 0 then 
                     error $ "pushing duplicate variables \"" ++ show dups ++ "\" onto table."
                 else (reverse vs) ++ vtab -- push in reverse order
         where dups = (intersect (map fst vs) (map fst vtab))

translate :: Prog -> Prog
translate p = fst3 (translateP p [] [])
         where fst3 (a, b, c) = a

-- First param is input, 2nd Param is function table, 3rd param is var table
translateP :: Prog -> [(String, (Int, Int))] -> [(String, Int)] ->  (Prog, [(String, (Int, Int))], [(String, Int)])
translateP (P s vars funcs main) ftab vtab = ((P s vars' funcs' main'), ftab''', vtab''')
                where 
                  (vars', ftab', vtab')     = translatePPs vars ftab vtab
                  (funcs', ftab'', vtab'')  = translatePPs funcs ftab' vtab'
--                  (main', ftab''', vtab''') = translateSs main ftab'' vtab''
                  (main', ftab''', vtab''') = (main, ftab'', vtab'')
                  

translatePPs :: [ProgPart] -> [(String, (Int, Int))] -> [(String, Int)] ->  ([ProgPart], [(String, (Int, Int))], [(String, Int)])
translatePPs (pp:pps) ftab vtab = ((pp':pps'), ftab'', vtab'') 
                where 
                  (pp', ftab', vtab')    = translatePP pp ftab vtab
                  (pps', ftab'', vtab'') = translatePPs pps ftab' vtab'
translatePPs [] ftab vtab = ([], ftab, vtab)

translatePP :: ProgPart -> [(String, (Int, Int))] -> [(String, Int)] ->  (ProgPart, [(String, (Int, Int))], [(String, Int)])
translatePP (VarDec (Assignment s e)) ftab vtab = if hasKey s vtab then 
                                                      error $ "Identifier \"" ++ s ++ "\" declared more than once."
                                                  else ((VarDec (Assignment s e)), ftab, ((s, -1):vtab))
translatePP (FunDec f args vars body) ftab vtab = if hasKey f ftab then
                                                      error $ "Function \"" ++ f ++ "\" declared more than once."
                                                  else if hasDups args then
                                                      error $ "Function \"" ++ f ++ "\" has duplicate argument named \"" ++ (firstDup args) ++ "\"."
                                                  else if length argsvartabi > 0 then -- parameters already a variable in scope
                                                      error $ "Function \"" ++ f ++ "\" has parameters" ++ (show argsvartabi) ++ 
                                                                " that conflict with already declared variables in scope."
                                                  else if length argsvarsi > 0 then --same variable name in declarations and parameter list
                                                      error $ "Function \"" ++ f ++ "\" redeclares parameters " ++ (show argsvarsi) ++ "."
                                                  else if length varsvartabi > 0 then -- redeclares variables
                                                      error $ "Function \"" ++ f ++ "\" redeclares variables " ++ (show varsvartabi) ++ "."
                                                  else ((FunDec f args vars body), ftab', vtab'' ) -- do nothing 
        where
          argsvartabi = intersect args (map fst vtab)
          argsvarsi = intersect args (varStrList vars)
          varStrList vs = map (\(VarDec (Assignment s e)) -> s) vs 
          varsvartabi = intersect (varStrList vars) (map fst vtab)
          ftab' = pushF (f, (-1, -1)) ftab
          vtab' = pushVs (map (\v -> (v, -1)) args) vtab
          vtab'' = pushVs (map (\(VarDec (Assignment s e))-> (s, -1)) vars) vtab'

translatePP pp ftab vtab = (pp, ftab, vtab)

-- translateSs :: [Statement] -> [(String, (Int, Int))] -> [(String, Int)] ->  ([Statement], [(String, (Int, Int))], [(String, Int)])
-- translateSs (s:ss) ftab vtab = (s', ftab', vtab'):(translateSs ss ftab' vtab')
--                 where (s', ftab', vtab') = translateS s ftab vtab
-- translateSs [] ftab vtab = []

-- translateS :: Statement -> [(String, (Int, Int))] -> [(String, Int)] ->  (Statement, [(String, (Int, Int))], [(String, Int)])
-- translateS s ftab vtab = (s, ftab, vtab)



-- translatePP :: ProgPart -> ProgPart
-- All variable declarations with no explicit assignment should be assigned 0
-- translatePP (VarDecDef s) = VarDec (Assignment s (IntE 0))
-- translatePP (VarDec a) = VarDec a
--translatePP (FunDec f args vardecs body) = (FunDec f args (map translatePP vardecs) (map translateS body))
-- translatePP (FunDec f args vardecs body) = (FunDec f args (map translatePP vardecs) body)


translateE :: Exp -> Exp
translateE (PlusE e1 e2)      = PlusE  (translateE e1) (translateE e2)
translateE (MinusE e1 e2)     = MinusE (translateE e1) (translateE e2)
translateE (TimesE e1 e2)     = TimesE (translateE e1) (translateE e2)
translateE (NegE e)           = NegE (translateE e)
translateE (FunCall s es)     = FunCall s (map translateE es)
-- need to implement division in terms of plus and mul
translateE (DivE e1 e2)       = error "Division not implemented yet!"
translateE e = e

translateC :: Comparison -> Comparison
translateC (Equality e1 e2)   = Equality (translateE e1) (translateE e2)
translateC (LessThan e1 e2)   = LessThan (translateE e1) (translateE e2)

