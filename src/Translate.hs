-- Transforms a desugarred AST into PDPlot instructions
-- translate performs two passes.  The first does the bulk
-- of the translation including backpatching, while the second
-- translates function calls into a Jsr instruction.  The second
-- pass is required to allow functions to be called before 
-- they are declared.
module Translate (translateToBinary, translate, prettyPrintI) where

import Data.List (intersect)
import Data.Foldable (toList)
import qualified Data.Sequence as S
import AbsSyn

-- The symbols representing functions, global variables, 
-- local variables, and parameters respectively in symbol
-- tables
data Symbol = F String Int Int
            | G String Int
            | L String Int
            | P String Int
              deriving Show

-- A PDPlot instruction.  Two word instructions
-- are separated into separate words, so for example,
-- JumpI should be followed by WordI
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
-- JsrFirstPass is transformed in the second pass into JsrI
-- The first param is the function name and the second is
-- the number of parameters used in the function call
                 | JsrFirstPass String Int
                 | JumpI
                 | JeqI 
                 | JltI 
                 | LoadiI
                 | PopI 
                 | WordI Int
                   deriving (Show, Eq)
                 
-- Is an identifier declared in a symbol table?
idDeclared :: String -> [Symbol] -> Bool
idDeclared sym (s:ss) = if sym == (name s) then True else idDeclared sym ss
         where
           name (F s _ _) = s
           name (G s _) = s
           name (L s _) = s
           name (P s _) = s
idDeclared _ [] = False

-- Fetch the symbol matching an identifier from a symbol table
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

-- Appends an item to end of sequence, but only if the last item in the sequence
-- is not a duplicate of it.  Useful for removing possibly redundant instructions
-- such as RtsI
apNoDup :: (Eq a) => S.Seq a -> a -> S.Seq a
apNoDup as a = if (l > 0) && ((S.index as (l -1)) == a) then as else ap as a
               where l = S.length as

-- Appends [PopI, (WordI i)] to end of the instruction sequence.  However, if the 
-- last two instructions are already pop and word, then it simply adds the 
-- two word values together.  This coalesces multiple consecutive pops
-- together into a single pop.
-- Horrible horrible implementation.  I need to get me some haskell!
apPopMerge :: S.Seq Instruction -> Int -> S.Seq Instruction
apPopMerge is i = if (l >= 2) && i1 == PopI && i2 == (WordI n) then
                          S.update (l - 1) (WordI (n + i)) is
                      else 
                          ap (ap is PopI) (WordI i)
    where l = S.length is
          (is2, i2) = case S.viewr is  of (xx S.:> x) -> (xx, x)
          (is1, i1) = case S.viewr is2 of xx S.:> x -> (xx, x)
          n         = case i2 of WordI n   -> n 
                                 otherwise -> 0

-- Replaces the instruction in the instruction sequence at the location
-- on top of the index stack with the supplied instruction
backpatch :: S.Seq Instruction -> [Int] -> Instruction -> (S.Seq Instruction, [Int])
backpatch is (idx:idxs) i = ((S.update idx i is), idxs)
backpatch is [] i = error "backpatch called against empty index stack"

-- Transforms an instruction type to a 16 bit int as per the 
-- PDPlot specification
translateToBinary :: [Instruction] -> [Int]
translateToBinary = map iToB
    where iToB inst = case inst of
                     HaltI       -> 0x0000
                     UpI         -> 0x0A00
                     DownI       -> 0x0C00
                     MoveI       -> 0x0E00
                     AddI        -> 0x1000
                     SubI        -> 0x1200
                     NegI        -> 0x2200
                     MulI        -> 0x1400
                     TestI       -> 0x1600
                     RtsI        -> 0x2800
                     (LoadGP i)  -> 0x0600 + twosComp i
                     (LoadFP i)  -> 0x0700 + twosComp i
                     (StoreGP i) -> 0x0400 + twosComp i
                     (StoreFP i) -> 0x0500 + twosComp i
                     (ReadGP i)  -> 0x0200 + twosComp i
                     (ReadFP i)  -> 0x0300 + twosComp i
                     JsrI        -> 0x6800
                     JumpI       -> 0x7000
                     JeqI        -> 0x7200
                     JltI        -> 0x7400
                     LoadiI      -> 0x5600
                     PopI        -> 0x5E00
                     (WordI i)   -> i
                     otherwise   -> error $ "Unhandled instruction " ++ (show inst)
              where
                twosComp i = if i > 0x7F then 
                            error $ "Too many variables declared.  Cannot continue." 
                        else if i < 0 then
                                 0x100 + i
                             else
                                 i

-- Takes a Program and translates it into an instruction sequence
translate :: ProgPart -> [Instruction]
translate p = toList (frth  (pp, ftab, vtab, is', idxs))
         where 
           -- First pass:
           (pp, ftab, vtab, is, idxs) = translate' p [] [] S.empty []
           -- Second pass:
           is' = linkFunctionCalls is ftab
           -- extract instruction sequence
           frth (_, _, _, a, _) = a

-- A second pass that translates all JsrFirstPass calls into actual
-- function calls, or throws an error if the function is never declared
-- or the wrong number of params is used.
linkFunctionCalls :: S.Seq Instruction -> [Symbol] -> S.Seq Instruction
linkFunctionCalls is ftab = case S.viewl is of
                              (JsrFirstPass s i S.:< xx) -> case lookupId s ftab of 
                                                              Just (F _ pos args) -> if args == i then 
                                                                                        JsrI S.<| (linkFunctionCalls (S.update 0 (WordI pos) xx) ftab) else 
                                                                                        error $ "Function \"" ++ s ++ "\" expects " ++ show args ++ 
                                                                                                  " parameters but was called with " ++ show i ++
                                                                                                  " parameters."
                                                              otherwised -> error $ "Undeclared function \"" ++ s ++ "\" called."
                              (x S.:< xx)                -> x S.<| (linkFunctionCalls xx ftab)
                              otherwise                -> S.empty
                                            
-- Call translate' on each item in a list of ProgParts.  Should be replaced with a fold
translatePPs :: [ProgPart] -> [Symbol] -> [Symbol] -> S.Seq Instruction -> [Int] -> ([ProgPart], [Symbol], [Symbol], S.Seq Instruction, [Int])
translatePPs (pp:pps) ftab vtab is idxs = ((pp':pps'), ftab'', vtab'', is'', idxs'') 
                where 
                  (pp', ftab', vtab', is', idxs')      = translate' pp ftab vtab is idxs
                  (pps', ftab'', vtab'', is'', idxs'') = translatePPs pps ftab' vtab' is' idxs'
translatePPs [] ftab vtab is idxs = ([], ftab, vtab, is, idxs)

-- Translates a program into an instruction sequence, leaving function calls unindexed
-- First param is an input program
-- 2nd Param is a function symbol table 
-- 3rd param is a variable symbol table
-- 4th param is the sequence instructions generated so far
-- 5th param is a stack of addresses in the instruction sequence that need backpatching

-- This quintuple is essentially the program state, and is passed in and out of every
-- call to translate'.  
translate' :: ProgPart -> [Symbol] -> [Symbol] -> S.Seq Instruction -> [Int] -> (ProgPart, [Symbol], [Symbol], S.Seq Instruction, [Int])

translate' (Prog s vars funcs main) ftab vtab is idxs = ((Prog s vars' funcs' main'), ftab'''', vtab'''', is'''''', idxs''''')
    where 
      (vars', ftab', vtab', is', idxs')             = translateGVarDecs vars 1 ftab vtab is idxs -- global variables
      is''                                          = ap (ap is' JumpI) (WordI 0) -- jump to program body
      idxs''                                        = ((S.length is'') - 1):idxs' -- need to backpatch jump
      (funcs', ftab''', vtab''', is''', idxs''')    = translateFunDecs funcs ftab' vtab' is'' idxs'' -- function declarations
      (main', ftab'''', vtab'''', is'''', idxs'''') = translatePPs main ftab''' vtab''' is''' idxs''' -- program body
      (is''''', idxs''''') = backpatch is'''' idxs'''' (WordI (S.length is''')) -- can now backpatch jump to program body
      is'''''' = ap is''''' HaltI --final halt instruction

-- Simple translation of commands that map 1:1 to a PDPlot instruction
translate' (Up)                  ftab vtab is idxs  = (Up, ftab, vtab, (ap is UpI), idxs)
translate' (Down)                ftab vtab is idxs = (Down, ftab, vtab, (ap is DownI), idxs)
translate' (MoveTo e1 e2)        ftab vtab is idxs = ((MoveTo e1' e2'), ftab, vtab, (ap is'' MoveI), idxs)
    where 
      (e1', _, _, is', _)  = translate' e1 ftab vtab is  idxs
      (e2', _, _, is'', _) = translate' e2 ftab vtab is' idxs
-- Can only read to a variable that has been declared
translate' (Read s)              ftab vtab is idxs = case lookupId s vtab of
                                                  Just (G _ i) -> ret (ReadGP i)
                                                  Just (L _ i) -> ret (ReadFP i)
                                                  Just (P _ i) -> ret (ReadFP i)
                                                  otherwise    -> error $ "Cannot read to undeclared variable \"" ++ s ++ "\"."
    where ret i = ((Read s), ftab, vtab, (ap is i), idxs)
-- Simple expressions
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
-- Function calls don't lookup symbol table until the second pass, but we still generate the code
-- to push params onto the stack, reserve return location, etc.
translate' (FunCall s es)        ftab vtab is idxs = (fc, ftab', vtab', is'''', idxs')
    where
      is' = (ap (ap is LoadiI) (WordI 0))  -- reserve a spot on the stack for the function call result
      (es', ftab', vtab', is'', idxs')  = translatePPs es ftab vtab is' idxs
      (fc, is''') = ((FunCall s es'), (ap (ap is'' (JsrFirstPass s (length es'))) (WordI 0)))
      is'''' = if (length es) > 0 then
                   apPopMerge is''' (length es)
               else
                   is'''
-- The desugar phase should have guarenteed we have only if-elses, not standalone ifs
translate' (IfElse c ss1 ss2)    ftab vtab is idxs = ((IfElse c' ss1' ss2'), ftab''', vtab''', is'''''', idxs'''''')
    where
      (c', ftab', vtab', is', idxs')               = translate' c ftab vtab is idxs
      (ss1', ftab'', vtab'', is'', idxs'')         = translatePPs ss1 ftab' vtab' is' idxs'
-- add jump to after else
      (is''', idxs''')                             = (ap (ap is'' JumpI) (WordI 0), ((S.length is'') + 1):idxs'')
      (ss2', ftab''', vtab''', is'''', idxs'''')   = translatePPs ss2 ftab'' vtab'' is''' idxs'''
      (is''''', idxs''''')                         = backpatch is'''' idxs'''' (WordI (S.length is''''))
      (is'''''', idxs'''''')                       = backpatch is''''' idxs''''' (WordI (S.length is'''))

translate' (While c ss)          ftab vtab is idxs = ((While c' ss'), ftab'', vtab'', is'''', idxs''')
    where
      (c', ftab', vtab', is', idxs')               = translate' c ftab vtab is idxs
      (ss', ftab'', vtab'', is'', idxs'')          = translatePPs ss ftab' vtab' is' idxs'
      is'''                                        = ap (ap is'' JumpI) (WordI (S.length is))
      (is'''', idxs''')                            = backpatch is''' idxs'' (WordI (S.length is'''))

-- A FunCallStm is a function call used for side-effects only, i.e. not as part of an expression
-- It is processed separately because that way the return value can be cleaned off the stack
translate' (FunCallStm f params) ftab vtab is idxs = ((FunCallStm f' params'), ftab', vtab', is'', idxs')
    where
      ((FunCall f' params'), ftab', vtab', is', idxs') = translate' (FunCall f params) ftab vtab is idxs
      is''                                             = apPopMerge is' 1 --cleanup stack
translate' (Compound ss)         ftab vtab is idxs = ((Compound ss'), ftab', vtab', is', idxs')
    where (ss', ftab', vtab', is', idxs') = translatePPs ss ftab vtab is idxs
translate' (Assignment s e)      ftab vtab is idxs = ((Assignment s e'), ftab', vtab', is'', idxs')
    where
      (e', ftab', vtab', is', idxs') = translate' e ftab vtab is idxs
      is''                           =  case lookupId s vtab' of
                                                  Just (G _ i) -> ap is' (StoreGP i)
                                                  Just (L _ i) -> ap is' (StoreFP i)
                                                  Just (P _ i) -> ap is' (StoreFP i)
                                                  otherwise    -> error $ "Storing to undeclared variable \"" ++ s ++ "\"."
-- We use "!ret" in the symbol table as a convient place to store the location
-- of the return value of a function.  "!ret" should be pushed onto the variable
-- symbol table before the first instruction of an function body is translated
translate' (Return e)            ftab vtab is idxs = ((Return e'), ftab', vtab', is''', idxs')
    where
      (e', ftab', vtab', is', idxs') = translate' e ftab vtab is idxs
      is''                           = case lookupId "!ret" vtab' of
                                                  Just (P _ i) -> ap is' (StoreFP i)
                                                  otherwise    -> error $ "Return statement can only be used within a function."
      is'''                          = ap is'' RtsI

-- Desugar phase should have guarenteed we have only Equality and LessThan comparators.
translate' (LessThan e1 e2)      ftab vtab is idxs = translateComparator (LessThan e1 e2) ftab vtab is idxs
translate' (Equality e1 e2)      ftab vtab is idxs = translateComparator (Equality e1 e2) ftab vtab is idxs

-- Leaves a hanging index that needs to be backpatched
translateComparator :: ProgPart -> [Symbol] -> [Symbol] -> S.Seq Instruction -> [Int] -> 
                       (ProgPart, [Symbol], [Symbol], S.Seq Instruction, [Int])
translateComparator pp ftab vtab is idxs = (pp', ftab'', vtab'', is'''', idxs''')
    where
      (ctor, e1, e2, jmpinst) = case pp of 
                         (Equality ea eb) -> (Equality, ea, eb, JeqI)
                         (LessThan ea eb) -> (LessThan, ea, eb, JltI)
                         otherwise        -> error $ "Unhandled comparator."
      (e1', ftab', vtab', is', idxs')     = translate' e1 ftab vtab is idxs
      (e2', ftab'', vtab'', is'', idxs'') = translate' e2 ftab' vtab' is' idxs'
      is''' = is'' S.>< (S.fromList [SubI, TestI, PopI, (WordI 1), jmpinst])
      is'''' = is''' S.>< (S.fromList [(WordI ((S.length is''') + 3)), JumpI, (WordI 0)])
      idxs''' = ((S.length is'''') - 1):idxs''
      pp' = ctor e1' e2'

translateBinaryOp exp ftab vtab is idxs = (exp, ftab, vtab, (ap is'' inst), idxs)
    where
      (e1', e2', inst) = case exp of 
                   (PlusE e1 e2)  -> (e1, e2, AddI)
                   (MinusE e1 e2) -> (e1, e2, SubI)
                   (TimesE e1 e2) -> (e1, e2, MulI)
      (e1'', _, _, is', _)  = translate' e1' ftab vtab is idxs
      (e2'', _, _, is'', _) = translate' e2' ftab vtab is' idxs

translateFunDecs :: [ProgPart] -> [Symbol] -> [Symbol] ->  S.Seq Instruction -> [Int] -> ([ProgPart], [Symbol], [Symbol], S.Seq Instruction, [Int])
translateFunDecs (pp:pps) ftab vtab is idxs = ((pp':pps'), ftab'', vtab'', is'', idxs'') 
                where 
                  (pp', ftab', vtab', is', idxs') = translateFD pp ftab vtab is idxs
                  (pps', ftab'', vtab'', is'', idxs'')  = translateFunDecs pps ftab' vtab' is' idxs'
                  translateFD :: ProgPart -> [Symbol] -> [Symbol] -> S.Seq Instruction -> [Int] -> 
                                 (ProgPart, [Symbol], [Symbol], S.Seq Instruction, [Int])
                  translateFD (FunDec f args vars body) ftab vtab is idxs = 
                              if idDeclared f ftab then 
                                       error $ "Function \"" ++ f ++ "\" declared more than once."
                              else ((FunDec f args' vars' body'), ftab', vtab, is''''', idxs'''')
                                   where
                                     ftab' = (F f (S.length is) (length args)):ftab
                                     (args', ftab'', vtab'', is'', idxs'')         = translatePVarDecs args (-(length args)-1) ftab' vtab is idxs
                                     (vars', ftab''', vtab''', is''', idxs''')     = translateLVarDecs vars 1 ftab' vtab'' is'' idxs''
                                     -- push a pseduo-variable onto the lookup table for return statement
                                     vtab''''                                      = (P "!ret" ((-(length args)) - 2)):vtab'''
                                     (body', ftab'''', _, is'''', idxs'''')        = translatePPs body ftab' vtab'''' is''' idxs'''
                                     is'''''                                       = apNoDup is'''' RtsI
translateFunDecs [] ftab vtab is idxs = ([], ftab, vtab, is, idxs)

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
translateLVarDecs (pp:pps) i ftab vtab is idxs = ((pp':pps'), ftab'', vtab'', is'', idxs'')
                where 
                  (pp', i', ftab', vtab', is', idxs') = translateLVD pp i ftab vtab is idxs
                  (pps', ftab'', vtab'', is'', idxs'') = translateLVarDecs pps i' ftab' vtab' is' idxs'
                  translateLVD :: ProgPart -> Int -> [Symbol] -> [Symbol] -> S.Seq Instruction -> [Int] -> 
                                  (ProgPart, Int, [Symbol], [Symbol], S.Seq Instruction, [Int])
                  translateLVD (VarDec (Assignment s e)) i ftab vtab is idxs = 
                      case lookupId s vtab of
                        Just (P _ i) -> error $ "Local variable \"" ++ s ++ "\" is already declared as a parameter in this function."
                        Just (L _ i) -> error $ "Local variable \"" ++ s ++ "\" is already declared in this function."
                        otherwise    -> ((VarDec (Assignment s e''')), (i + 1), ftab''', ((L s i):vtab'''), is''', idxs''')
                            where 
                              (e''', ftab''', vtab''', is''', idxs''') = translate' e ftab vtab is idxs
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
                      case lookupId s vtab of
                        Just (P _ i) -> error $ "Parameter \"" ++ s ++ "\" is already declared in this function."
                        Just (L _ i) -> error $ "Parameter \"" ++ s ++ "\" is already declared as a local variable in this function."
                        otherwise    -> (s, (i + 1), ftab, ((P s i):vtab), is, idxs)
translatePVarDecs [] _ ftab vtab is idxs = ([], ftab, vtab, is, idxs)

prettyPrintI  :: [Instruction] -> String
prettyPrintI  is       = prettyPrintI' is 0
prettyPrintI' (i:is) n = (show n) ++ "  " ++ (show i) ++ "\n" ++ (prettyPrintI' is (n + 1))
prettyPrintI' [] _     = ""
