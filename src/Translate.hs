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
                 | JsrI
                 | JumpI
                 | JeqI 
                 | JltI 
                 | LoadiI
                 | PopI 
                 | WordI Int
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

-- use own append fn to make it easier if we wish to use lists instead of sequences later
ap :: S.Seq a -> a -> S.Seq a
ap as a = as S.|> a

backpatch :: S.Seq Instruction -> [Int] -> Instruction -> (S.Seq Instruction, [Int])
backpatch is (idx:idxs) i = ((S.update idx i is), idxs)
backpatch is [] i = error "backpatch called against empty index stack"

translate :: ProgPart -> ProgPart
translate p = fst5 (trace ((show is) ++ "\n--\n") (pp, ftab, vtab, is, idxs))
         where 
           (pp, ftab, vtab, is, idxs) = translate' p [] [] S.empty []
           fst5 (a, _, _, _, _) = a

-- First param is input, 2nd Param is function table, 3rd param is var table
translatePPs :: [ProgPart] -> [Symbol] -> [Symbol] -> S.Seq Instruction -> [Int] -> ([ProgPart], [Symbol], [Symbol], S.Seq Instruction, [Int])
translatePPs (pp:pps) ftab vtab is idxs = ((pp':pps'), ftab'', vtab'', is'', idxs'') 
                where 
                  (pp', ftab', vtab', is', idxs')      = translate' pp ftab vtab is idxs
                  (pps', ftab'', vtab'', is'', idxs'') = translatePPs pps ftab' vtab' is' idxs'
translatePPs [] ftab vtab is idxs = ([], ftab, vtab, is, idxs)

-- First param is input, 2nd Param is function table, 3rd param is var table, 4th param is the instructions generated, 5th param is backpatch idx stack
translate' :: ProgPart -> [Symbol] -> [Symbol] -> S.Seq Instruction -> [Int] -> (ProgPart, [Symbol], [Symbol], S.Seq Instruction, [Int])

translate' (Prog s vars funcs main) ftab vtab is idxs = ((Prog s vars' funcs' main'), ftab'''', vtab'''', is''''', idxs''''')
    where 
      (vars', ftab', vtab', is', idxs')             = translateGVarDecs vars 1 ftab vtab is idxs
      is''                                          = ap (ap is' JumpI) (WordI 0)
      idxs''                                        = ((S.length is'') - 1):idxs'
      (funcs', ftab''', vtab''', is''', idxs''')    = translateFunDecs funcs 1 ftab' vtab' is'' idxs''
      (main', ftab'''', vtab'''', is'''', idxs'''') = translatePPs main ftab''' vtab''' is''' idxs'''
      (is''''', idxs''''') = backpatch is'''' idxs'''' (WordI (S.length is'''))
-- statements
-- translate' (Assignment s e) ftab vtab is = 
translate' (Up)                  ftab vtab is idxs  = (Up, ftab, vtab, (ap is UpI), idxs)
translate' (Down)                ftab vtab is idxs = (Down, ftab, vtab, (ap is DownI), idxs)
translate' (MoveTo e1 e2)        ftab vtab is idxs = ((MoveTo e1' e2'), ftab, vtab, (ap is'' MoveI), idxs)
    where 
      (e1', _, _, is', _)  = translate' e1 ftab vtab is  idxs
      (e2', _, _, is'', _) = translate' e2 ftab vtab is' idxs
translate' (Read s)              ftab vtab is idxs = case lookupId s vtab of
                                                  Just (G _ i) -> ret (ReadGP i)
                                                  Just (L _ i) -> ret (ReadFP i)
                                                  Just (P _ i) -> ret (ReadFP i)
                                                  Nothing      -> error $ "Cannot read to undeclared variable \"" ++ s ++ "\"."
                                                  otherwise    -> error $ "Unhandled case"
    where ret i = ((Read s), ftab, vtab, (ap is i), idxs)
-- expressions
translate' (IntE i)              ftab vtab is idxs = ((IntE i), ftab, vtab, (ap (ap is LoadiI) (WordI i)), idxs)
translate' (IdentE s)            ftab vtab is idxs = case lookupId s vtab of
                                                  Just (G _ i) -> ret (LoadGP i)
                                                  Just (L _ i) -> ret (LoadFP i)
                                                  Just (P _ i) -> ret (LoadFP i)
                                                  Nothing      -> error $ "Use of undeclared variable \"" ++ s ++ "\"."
                                                  otherwise    -> error $ "Unhandled case"
    where ret i = ((IdentE s), ftab, vtab, (ap is i), idxs)

translate' (PlusE e1 e2)         ftab vtab is idxs = translateBinaryOp (PlusE e1 e2) ftab vtab is idxs
translate' (MinusE e1 e2)        ftab vtab is idxs = translateBinaryOp (MinusE e1 e2) ftab vtab is idxs
translate' (TimesE e1 e2)        ftab vtab is idxs = translateBinaryOp (TimesE e1 e2) ftab vtab is idxs
translate' (NegE e1)             ftab vtab is idxs = ((NegE e1'), ftab', vtab', (ap is' NegI), idxs')
    where (e1', ftab', vtab', is', idxs') = translate' e1 ftab vtab is idxs
translate' (FunCall s es)        ftab vtab is idxs = (fc, ftab', vtab', is''', idxs')
    where
      is' = (ap (ap is LoadiI) (WordI 0))  -- reserve a spot on the stack for the function call result
      (es', ftab', vtab', is'', idxs')  = translatePPs es ftab vtab is' idxs
      (fc, is''') = case lookupId s ftab' of
             Just (F _ i _) -> ((FunCall s es'), (ap (ap is'' JsrI) (WordI i)))
             Nothing        -> error $ "Calling undeclared function \"" ++ s ++ "\" called."
             otherwise      -> error $ "Unhandled case in function call."
-- translate' (If c ss)             ftab vtab is = 
-- translate' (IfElse c ss1 ss2)    ftab vtab is = 
-- translate' (While c ss)          ftab vtab is = 
-- translate' (Return e)            ftab vtab is = 
-- translate' (FunCallStm f params) ftab vtab is = 
translate' (Compound ss)         ftab vtab is idxs = ((Compound ss'), ftab', vtab', is', idxs')
    where (ss', ftab', vtab', is', idxs') = translatePPs ss ftab vtab is idxs
translate' pp ftab vtab is idxs = (pp, ftab, vtab, is, idxs)

translateBinaryOp exp ftab vtab is idxs = (exp, ftab, vtab, (ap is'' inst), idxs)
    where
      (e1', e2', inst) = case exp of 
                   (PlusE e1 e2)  -> (e1, e2, AddI)
                   (MinusE e1 e2) -> (e1, e2, SubI)
                   (TimesE e1 e2) -> (e1, e2, MulI)
      (e1'', _, _, is', _)  = translate' e1' ftab vtab is idxs
      (e2'', _, _, is'', _) = translate' e2' ftab vtab is' idxs

translateFunDecs :: [ProgPart] -> Int -> [Symbol] -> [Symbol] ->  S.Seq Instruction -> [Int] -> ([ProgPart], [Symbol], [Symbol], S.Seq Instruction, [Int])
translateFunDecs (pp:pps) i ftab vtab is idxs = ((pp':pps'), ftab'', vtab'', is'', idxs'') 
                where 
                  (pp', i', ftab', vtab', is', idxs') = translateFD pp i ftab vtab is idxs
                  (pps', ftab'', vtab'', is'', idxs'')  = translateFunDecs pps i' ftab' vtab' is' idxs'
                  translateFD :: ProgPart -> Int -> [Symbol] -> [Symbol] -> S.Seq Instruction -> [Int] -> 
                                 (ProgPart, Int, [Symbol], [Symbol], S.Seq Instruction, [Int])
                  translateFD (FunDec f args vars body) i ftab vtab is idxs = 
                              if idDeclared f ftab then 
                                       error $ "Function \"" ++ f ++ "\" declared more than once."
                              else ((FunDec f args' vars' body'), (i + 1), ftab', vtab, is'''', idxs)
                                   where
                                     ftab' = (F f i (length args)):ftab
                                     (args', ftab'', vtab'', is'', idxs'')         = translatePVarDecs args (-(length args)) ftab' vtab is idxs
                                     (vars', ftab''', vtab''', is''', idxs''')     = translateLVarDecs vars 1 ftab' vtab'' is'' idxs''
                                     (body', ftab'''', vtab'''', is'''', idxs'''') = translatePPs body ftab' vtab''' is''' idxs'''
translateFunDecs [] _ ftab vtab is idxs = ([], ftab, vtab, is, idxs)

translateGVarDecs :: [ProgPart] -> Int -> [Symbol] -> [Symbol] ->  S.Seq Instruction -> [Int] ->
                     ([ProgPart], [Symbol], [Symbol], S.Seq Instruction, [Int])
translateGVarDecs (pp:pps) i ftab vtab is idxs = ((pp':pps'), ftab'', vtab'', is'', idxs'') 
                where 
                  (pp', i', ftab', vtab', is', idxs')  = translateGVD pp i ftab vtab is idxs
                  (pps', ftab'', vtab'', is'', idxs'') = translateGVarDecs pps i' ftab' vtab' is' idxs'
                  translateGVD :: ProgPart -> Int -> [Symbol] -> [Symbol] ->  S.Seq Instruction -> [Int] ->
                                  (ProgPart, Int, [Symbol], [Symbol], S.Seq Instruction, [Int])
                  translateGVD (VarDec (Assignment s e)) i ftab vtab is idxs = 
                      if idDeclared s vtab then 
                          error $ "Global variable \"" ++ s ++ "\" declared more than once."
                      else ((VarDec (Assignment s e')), (i + 1), ftab''', ((G s i):vtab'''), is''', idxs''')
                          where (e', ftab''', vtab''', is''', idxs''') = (translate' e ftab vtab is idxs)

translateGVarDecs [] _ ftab vtab is idxs = ([], ftab, vtab, is, idxs)

translateLVarDecs :: [ProgPart] -> Int -> [Symbol] -> [Symbol] ->  S.Seq Instruction -> [Int] -> 
                     ([ProgPart], [Symbol], [Symbol], S.Seq Instruction, [Int])
translateLVarDecs (pp:pps) i ftab vtab is idxs = ((pp':pps'), ftab'', vtab'', is, idxs)
                where 
                  (pp', i', ftab', vtab', is', idxs') = translateLVD pp i ftab vtab is idxs
                  (pps', ftab'', vtab'', is'', idxs'') = translateLVarDecs pps i' ftab' vtab' is' idxs'
                  translateLVD :: ProgPart -> Int -> [Symbol] -> [Symbol] -> S.Seq Instruction -> [Int] -> 
                                  (ProgPart, Int, [Symbol], [Symbol], S.Seq Instruction, [Int])
                  translateLVD (VarDec (Assignment s e)) i ftab vtab is idxs = 
                      if idDeclared s vtab then 
                          error $ "Identifier \"" ++ s ++ "\" declared more than once."
                      else ((VarDec (Assignment s e)), (i + 1), ftab, ((L s i):vtab), is, idxs)
translateLVarDecs [] _ ftab vtab is idxs = ([], ftab, vtab, is, idxs)

translatePVarDecs :: [String] -> Int -> [Symbol] -> [Symbol] ->  S.Seq Instruction -> [Int] -> 
                     ([String], [Symbol], [Symbol], S.Seq Instruction, [Int])
translatePVarDecs (pp:pps) i ftab vtab is idxs = ((pp':pps'), ftab'', vtab'', is, idxs)
                where 
                  (pp', i', ftab', vtab', is', idxs')  = translatePVD pp i ftab vtab is idxs
                  (pps', ftab'', vtab'', is'', idxs'') = translatePVarDecs pps i' ftab' vtab' is' idxs'
                  translatePVD :: String -> Int -> [Symbol] -> [Symbol] ->  S.Seq Instruction -> [Int] -> 
                                  (String, Int, [Symbol], [Symbol], S.Seq Instruction, [Int])
                  translatePVD s i ftab vtab is idxs = 
                      if idDeclared s vtab then 
                          error $ "Identifier \"" ++ s ++ "\" declared more than once."
                      else (s, (i + 1), ftab, ((P s i):vtab), is, idxs)
translatePVarDecs [] _ ftab vtab is idxs = ([], ftab, vtab, is, idxs)