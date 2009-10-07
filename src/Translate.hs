module Translate (translate) where

import Data.List (intersect)
import Debug.Trace
import qualified Data.Sequence as S
import AbsSyn


-- hasDups :: Eq a => [a] -> Bool
-- -- Inefficient, maybe replace with something smarter
-- -- hasDups a = if length a == length (nub a) then False else True
-- hasDups l                   = hasDups' l []  
--   where
--     hasDups' [] _           = False
--     hasDups' (x:xs) ls
--         | x `elem` ls   = True
--         | otherwise     = hasDups' xs (x:ls)

-- -- Will error if hasDups == False
-- firstDup :: Eq a => [a] -> a
-- firstDup l                   = firstDup' l []  
--   where
--     firstDup' (x:xs) ls
--         | x `elem` ls   = x
--         | otherwise     = firstDup' xs (x:ls)


-- hasKey :: Eq a => a -> [(a, b)] -> Bool
-- hasKey a abs = elem a (map fst abs)

-- findVals :: Eq a => a -> [(a, b)] -> [b]
-- findVals a abs = (map snd (filter (\ab -> a == (fst ab)) abs))

-- -- ensures exactly one val is returned.  Use findVals if another
-- -- number of vals could be valid
-- getVal :: Eq a => a -> [(a, b)] -> b
-- getVal a abs = head (findVals a abs)

data Symbol = F String Int Int
            | G String Int
            | L String Int
            | P String Int
              deriving Show

data Instruction = HaltI
                 | UpI
                 | DownI
                 | MoveI
                 | AddI
                 | SubI
                 | NegI
                 | MulI
                 | TestI
                 | RtsI
                 | LoadGP Int
                 | LoadFP Int
                 | StoreGP Int
                 | StoreFP Int
                 | ReadGP Int
                 | ReadFP Int
                 | JsrI Int
                 | JumpI Int
                 | JeqI Int
                 | JltI Int
                 | LoadiI Int
                 | PopI Int
                   deriving Show
                 

idDeclared :: String -> [Symbol] -> Bool
idDeclared sym (s:ss) = if sym == (name s) then True else idDeclared sym ss
         where
           name (F s _ _) = s
           name (G s _) = s
           name (L s _) = s
           name (P s _) = s
idDeclared _ [] = False

lookupId :: String -> [Symbol] -> Maybe Symbol
lookupId sym (s:ss) = if sym == (name s) then Just s else lookupId sym ss
         where
           name (F s _ _) = s
           name (G s _) = s
           name (L s _) = s
           name (P s _) = s
lookupId _ [] = Nothing

ap :: [a] -> a -> [a]
ap as a = as ++ [a]

translate :: ProgPart -> ProgPart
translate p = fst4 (trace ((show is) ++ "\n--\n") (pp, ftab, vtab, is))
         where 
           (pp, ftab, vtab, is) = translate' p [] [] []
           fst4 (a, _, _, _) = a

-- First param is input, 2nd Param is function table, 3rd param is var table
translatePPs :: [ProgPart] -> [Symbol] -> [Symbol] -> [Instruction] ->  ([ProgPart], [Symbol], [Symbol], [Instruction])
translatePPs (pp:pps) ftab vtab is = ((pp':pps'), ftab'', vtab'', is'') 
                where 
                  (pp', ftab', vtab', is')    = translate' pp ftab vtab is
                  (pps', ftab'', vtab'', is'') = translatePPs pps ftab' vtab' is'
translatePPs [] ftab vtab is = ([], ftab, vtab, is)

-- First param is input, 2nd Param is function table, 3rd param is var table, 4th param is the instructions generated
translate' :: ProgPart -> [Symbol] -> [Symbol] -> [Instruction] -> (ProgPart, [Symbol], [Symbol], [Instruction])

translate' (Prog s vars funcs main) ftab vtab is = ((Prog s vars' funcs' main'), ftab''', vtab''', is''')
    where 
      (vars', ftab', vtab', is')     = translateGVarDecs vars 1 ftab vtab is
      (funcs', ftab'', vtab'', is'')  = translateFunDecs funcs 1 ftab' vtab' is'
      (main', ftab''', vtab''', is''') = translatePPs main ftab'' vtab'' is''
-- statements
-- translate' (Assignment s e) ftab vtab is = 
translate' (Up)                  ftab vtab is = (Up, ftab, vtab, (ap is UpI))
translate' (Down)                ftab vtab is = (Down, ftab, vtab, (ap is DownI))
translate' (MoveTo e1 e2)        ftab vtab is = ((MoveTo e1' e2'), ftab, vtab, (ap is'' MoveI))
    where 
      (e1', _, _, is')  = translate' e1 ftab vtab is
      (e2', _, _, is'') = translate' e2 ftab vtab is'
translate' (Read s)              ftab vtab is = case lookupId s vtab of
                                                  Just (G _ i) -> ret (ReadGP i)
                                                  Just (L _ i) -> ret (ReadFP i)
                                                  Just (P _ i) -> ret (ReadFP i)
                                                  Nothing      -> error $ "Cannot read to undeclared variable \"" ++ s ++ "\"."
                                                  otherwise    -> error $ "Unhandled case"
    where ret i = ((Read s), ftab, vtab, (ap is i))
-- expressions
translate' (IntE i)              ftab vtab is = ((IntE i), ftab, vtab, (ap is (LoadiI i)))
translate' (IdentE s)            ftab vtab is = case lookupId s vtab of
                                                  Just (G _ i) -> ret (LoadGP i)
                                                  Just (L _ i) -> ret (LoadFP i)
                                                  Just (P _ i) -> ret (LoadFP i)
                                                  Nothing      -> error $ "Use of undeclared variable \"" ++ s ++ "\"."
                                                  otherwise    -> error $ "Unhandled case"
    where ret i = ((IdentE s), ftab, vtab, (ap is i))

translate' (PlusE e1 e2)         ftab vtab is = translateBinaryOp (PlusE e1 e2) ftab vtab is
translate' (MinusE e1 e2)        ftab vtab is = translateBinaryOp (MinusE e1 e2) ftab vtab is
translate' (TimesE e1 e2)        ftab vtab is = translateBinaryOp (TimesE e1 e2) ftab vtab is
translate' (NegE e1)             ftab vtab is = ((NegE e1'), ftab', vtab', (ap is' NegI))
    where (e1', ftab', vtab', is') = translate' e1 ftab vtab is
-- PlusE  (translate' e1 ftab vtab ) (translate' e2)
-- translate' (MinusE e1 e2)     ftab vtab is = MinusE (translate' e1) (translate' e2)
-- translate' (TimesE e1 e2)     ftab vtab is = TimesE (translate' e1) (translate' e2)
-- translate' (NegE e)           ftab vtab is = NegE (translate' e)
-- translate' (FunCall s es)     = FunCall s (map translate' es)

-- translate' (If c ss)             ftab vtab is = 
-- translate' (IfElse c ss1 ss2)    ftab vtab is = 
-- translate' (While c ss)          ftab vtab is = 
-- translate' (Return e)            ftab vtab is = 
-- translate' (FunCallStm f params) ftab vtab is = 
translate' (Compound ss)         ftab vtab is = ((Compound ss'), ftab', vtab', is')
    where (ss', ftab', vtab', is') = translatePPs ss ftab vtab is
translate' pp ftab vtab is = (pp, ftab, vtab, is)


translateBinaryOp exp ftab vtab is = (exp, ftab, vtab, (ap is'' inst))
    where
      (e1', e2', inst) = case exp of 
                   (PlusE e1 e2)  -> (e1, e2, AddI)
                   (MinusE e1 e2) -> (e1, e2, SubI)
                   (TimesE e1 e2) -> (e1, e2, MulI)
      (e1'', _, _, is')  = translate' e1' ftab vtab is
      (e2'', _, _, is'') = translate' e2' ftab vtab is'
      

translateFunDecs :: [ProgPart] -> Int -> [Symbol] -> [Symbol] ->  [Instruction] -> ([ProgPart], [Symbol], [Symbol], [Instruction])
translateFunDecs (pp:pps) i ftab vtab is = ((pp':pps'), ftab'', vtab'', is'') 
                where 
                  (pp', i', ftab', vtab', is') = translateFD pp i ftab vtab is
                  (pps', ftab'', vtab'', is'')  = translateFunDecs pps i' ftab' vtab' is'
                  translateFD :: ProgPart -> Int -> [Symbol] -> [Symbol] -> [Instruction] ->  (ProgPart, Int, [Symbol], [Symbol], [Instruction])
                  translateFD (FunDec f args vars body) i ftab vtab is = 
                              if idDeclared f ftab then 
                                       error $ "Function \"" ++ f ++ "\" declared more than once."
                              else ((FunDec f args' vars' body'), (i + 1), ftab', vtab, is'''')
                                   where
                                     ftab' = (F f i (length args)):ftab
                                     (args', ftab'', vtab'', is'')     = translatePVarDecs args (-(length args)) ftab' vtab is
                                     (vars', ftab''', vtab''', is''')   = translateLVarDecs vars 1 ftab' vtab'' is''
                                     (body', ftab'''', vtab'''', is'''') = translatePPs body ftab' vtab''' is'''
translateFunDecs [] _ ftab vtab is = ([], ftab, vtab, is)

translateGVarDecs :: [ProgPart] -> Int -> [Symbol] -> [Symbol] ->  [Instruction] -> ([ProgPart], [Symbol], [Symbol], [Instruction])
translateGVarDecs (pp:pps) i ftab vtab is = ((pp':pps'), ftab'', vtab'', is) 
                where 
                  (pp', i', ftab', vtab', is')    = translateGVD pp i ftab vtab is
                  (pps', ftab'', vtab'', is'') = translateGVarDecs pps i' ftab' vtab' is'
                  translateGVD :: ProgPart -> Int -> [Symbol] -> [Symbol] ->  [Instruction] -> (ProgPart, Int, [Symbol], [Symbol], [Instruction])
                  translateGVD (VarDec (Assignment s e)) i ftab vtab is = if idDeclared s vtab then 
                                                                           error $ "Identifier \"" ++ s ++ "\" declared more than once."
                                                                       else ((VarDec (Assignment s e)), (i + 1), ftab, ((G s i):vtab), is)
translateGVarDecs [] _ ftab vtab is = ([], ftab, vtab, is)

translateLVarDecs :: [ProgPart] -> Int -> [Symbol] -> [Symbol] ->  [Instruction] -> ([ProgPart], [Symbol], [Symbol], [Instruction])
translateLVarDecs (pp:pps) i ftab vtab is = ((pp':pps'), ftab'', vtab'', is) 
                where 
                  (pp', i', ftab', vtab', is') = translateLVD pp i ftab vtab is
                  (pps', ftab'', vtab'', is'') = translateLVarDecs pps i' ftab' vtab' is'
                  translateLVD :: ProgPart -> Int -> [Symbol] -> [Symbol] -> [Instruction] -> (ProgPart, Int, [Symbol], [Symbol], [Instruction])
                  translateLVD (VarDec (Assignment s e)) i ftab vtab is = if idDeclared s vtab then 
                                                                           error $ "Identifier \"" ++ s ++ "\" declared more than once."
                                                                       else ((VarDec (Assignment s e)), (i + 1), ftab, ((L s i):vtab), is)
translateLVarDecs [] _ ftab vtab is = ([], ftab, vtab, is)

translatePVarDecs :: [String] -> Int -> [Symbol] -> [Symbol] ->  [Instruction] -> ([String], [Symbol], [Symbol], [Instruction])
translatePVarDecs (pp:pps) i ftab vtab is = ((pp':pps'), ftab'', vtab'', is) 
                where 
                  (pp', i', ftab', vtab', is') = translatePVD pp i ftab vtab is
                  (pps', ftab'', vtab'', is'') = translatePVarDecs pps i' ftab' vtab' is'
                  translatePVD :: String -> Int -> [Symbol] -> [Symbol] ->  [Instruction] -> (String, Int, [Symbol], [Symbol], [Instruction])
                  translatePVD s i ftab vtab is = if idDeclared s vtab then 
                                                   error $ "Identifier \"" ++ s ++ "\" declared more than once."
                                               else (s, (i + 1), ftab, ((P s i):vtab), is)
translatePVarDecs [] _ ftab vtab is = ([], ftab, vtab, is)


-- translateSs :: [Statement] -> [(String, (Int, Int))] -> [(String, Int)] ->  ([Statement], [(String, (Int, Int))], [(String, Int)])
-- translateSs (s:ss) ftab vtab = (s', ftab', vtab'):(translateSs ss ftab' vtab')
--                 where (s', ftab', vtab') = translateS s ftab vtab
-- translateSs [] ftab vtab = []

-- translateS :: Statement -> [(String, (Int, Int))] -> [(String, Int)] ->  (Statement, [(String, (Int, Int))], [(String, Int)])
-- translateS s ftab vtab = (s, ftab, vtab)



-- translate' :: ProgPart -> ProgPart
-- All variable declarations with no explicit assignment should be assigned 0
-- translate' (VarDecDef s) = VarDec (Assignment s (IntE 0))
-- translate' (VarDec a) = VarDec a
--translate' (FunDec f args vardecs body) = (FunDec f args (map translate' vardecs) (map translateS body))
-- translate' (FunDec f args vardecs body) = (FunDec f args (map translate' vardecs) body)


-- translate' (PlusE e1 e2)      = PlusE  (translate' e1) (translate' e2)
-- translate' (MinusE e1 e2)     = MinusE (translate' e1) (translate' e2)
-- translate' (TimesE e1 e2)     = TimesE (translate' e1) (translate' e2)
-- translate' (NegE e)           = NegE (translate' e)
-- translate' (FunCall s es)     = FunCall s (map translate' es)
-- -- need to implement division in terms of plus and mul
-- translate' (DivE e1 e2)       = error "Division not implemented yet!"
-- translate' e = e

-- translate' (Equality e1 e2)   = Equality (translate' e1) (translate' e2)
-- translate' (LessThan e1 e2)   = LessThan (translate' e1) (translate' e2)

