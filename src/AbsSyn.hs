module AbsSyn where

data ProgPart  = 
         Prog String [ProgPart] [ProgPart] [ProgPart] |
         VarDec        ProgPart          |
         VarDecDef     String            |
         FunDec        String [String] [ProgPart] [ProgPart]       |
         Assignment    String ProgPart        |
         Up                              |
         Down                            |
         MoveTo ProgPart ProgPart                  |
         Read String                     |       
         If ProgPart [ProgPart]       |
         IfElse ProgPart [ProgPart] [ProgPart]  |
         While ProgPart [ProgPart]    |
         Return ProgPart                      |
         FunCallStm String [ProgPart]         |
         Compound     [ProgPart]        |
         PlusE   ProgPart ProgPart                |
         MinusE  ProgPart ProgPart                |
         TimesE  ProgPart ProgPart                |
         DivE    ProgPart ProgPart                |
         NegE    ProgPart                    |
         FunCall String [ProgPart]           |
         IntE    Int                    |
         IdentE  String                 |
         Equality      ProgPart ProgPart          |
         Inequality    ProgPart ProgPart          |
         LessThan      ProgPart ProgPart          |
         LessThanEq    ProgPart ProgPart          |
         GreaterThan   ProgPart ProgPart          |
         GreaterThanEq ProgPart ProgPart          |
         Nil -- null Ctor
