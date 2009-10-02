{
-- Use of line numbers taken from
-- http://darcs.haskell.org/alex/examples/Tokens_posn.x
module Tokenize (Token(..), AlexPosn(..), alexScanTokens, token_posn) where

import Data.Char (isSpace)
}

%wrapper "posn"

$digit = 0-9                        -- digits
$alpha = [a-zA-Z]                   -- alphabetic characters

tokens :-

  $white+                           ; -- Whitespace
  "//".*                            ; -- Comments
  turtle                            { \p s -> TTurtle p }
  -- Comparison operators.  May contain whitespace if two char symbols
  \< $white* \= | \= $white* \= 
                | \> $white* \= 
                | \> 
                | \<                { \p s -> TComparOp p (filter (not . isSpace) s) }
  \=                                { \p s -> TAssOp p }
  [\+\-\*\/]                        { \p s -> TArithOp p (head s) }
  \(                                { \p s -> TBra p }
  \)                                { \p s -> TKet p }
  \{                                { \p s -> TCBra p }
  \}                                { \p s -> TCKet p }
  \,                                { \p s -> TComma p }
  "var" | "fun" | "if" | "else" 
        | "while" | "return"        { \p s -> TKeyword p s }
  "up" | "down" | "moveto" | "read" { \p s -> TBuiltin p s }
  $digit+                           { \p s -> TInt p (read s) }
  $alpha [$alpha $digit \_ \']*     { \p s -> TIdent p s }

{
-- Each action has type :: String -> Token
-- The token type:
data Token = TTurtle AlexPosn
           | TComparOp AlexPosn String
           | TAssOp AlexPosn
           | TArithOp AlexPosn Char
           | TBra AlexPosn
           | TKet AlexPosn
           | TCBra AlexPosn
           | TCKet AlexPosn
           | TComma AlexPosn
           | TKeyword AlexPosn String
           | TBuiltin AlexPosn String
           | TIdent AlexPosn String
           | TInt AlexPosn Int
       deriving (Eq,Show)

token_posn (TTurtle p) = p
token_posn (TComparOp p _) = p
token_posn (TAssOp p) = p
token_posn (TArithOp p _) = p
token_posn (TBra p) = p
token_posn (TKet p) = p
token_posn (TCBra p) = p
token_posn (TCKet p) = p
token_posn (TComma p) = p
token_posn (TKeyword p _) = p
token_posn (TBuiltin p _) = p
token_posn (TIdent p _) = p
token_posn (TInt p _) = p

}