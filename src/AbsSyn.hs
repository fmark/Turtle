-- Abstract syntax tree for turtle language
module AbsSyn where

data ProgPart  = 
-- core language
         Prog String [ProgPart] [ProgPart] [ProgPart]              |
         VarDec        ProgPart                                    |
         FunDec        String [String] [ProgPart] [ProgPart]       |
         Assignment    String ProgPart                             |
         Up                                                        |
         Down                                                      |
         MoveTo ProgPart ProgPart                                  |
         Read String                                               | 
         IfElse ProgPart [ProgPart] [ProgPart]                     |
         While ProgPart [ProgPart]                                 |
         Return ProgPart                                           |
         FunCallStm String [ProgPart]                              |
         Compound     [ProgPart]                                   |
         PlusE   ProgPart ProgPart                                 |
         MinusE  ProgPart ProgPart                                 |
         TimesE  ProgPart ProgPart                                 |
         NegE    ProgPart                                          |
         FunCall String [ProgPart]                                 |
         IntE    Int                                               |
         IdentE  String                                            |
         Equality      ProgPart ProgPart                           |
         LessThan      ProgPart ProgPart                           |
-- syntactic sugar                                                                     
         VarDecDef     String                                      |
         If ProgPart [ProgPart]                                    |
         DivE    ProgPart ProgPart                                 |
         Inequality    ProgPart ProgPart                           |
         LessThanEq    ProgPart ProgPart                           |
         GreaterThan   ProgPart ProgPart                           |
         GreaterThanEq ProgPart ProgPart                          
         deriving Show
