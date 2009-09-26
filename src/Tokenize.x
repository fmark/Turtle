{
module Main (main) where

import Data.Char (isSpace)
}

%wrapper "basic"

$digit = 0-9                        -- digits
$alpha = [a-zA-Z]                   -- alphabetic characters

tokens :-

  $white+                           ; -- Whitespace
  "//".*                            ; -- Comments
  turtle                            { \s -> TTurtle }
  -- Comparison operators.  May contain whitespace if two char symbols
  \< $white* \= | \= $white* \= 
                | \> $white* \= 
                | \> 
                | \<                { \s -> TComparOp (filter (not . isSpace) s) }
  \=                                { \s -> TAssOp }
  [\+\-\*\\]                        { \s -> TArithOp (head s) }
  \(                                { \s -> TBra }
  \)                                { \s -> TKet }
  \{                                { \s -> TCBra }
  \}                                { \s -> TCKet }
  \,                                { \s -> TComma }
  $digit+                           { \s -> TInt (read s) }
  $alpha [$alpha $digit \_ \']*     { \s -> TIdent s }

{
-- Each action has type :: String -> Token
-- The token type:
data Token = TTurtle
           | TComparOp String
           | TAssOp
           | TArithOp Char
           | TBra  | TKet
           | TCBra | TCKet
           | TComma
           | TIdent String
           | TInt Int
       deriving (Eq,Show)

main = do
  s <- getContents
  print (alexScanTokens s)
}