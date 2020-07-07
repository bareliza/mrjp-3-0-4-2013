module Kod4 where

import AbsLatte

data Kod4 =
   KAss Op String String String
 | Mov String String
 | MovRelPtr String String Int
 | RelPtrMov String Int String 
 | Call String [String]
 | CallRelPtr String Int [String]

instance Show Kod4 where
  show instr = case instr of
   KAss op v1 v2 v3 -> v1++" := "++v2++" "++(show1 op)++" "++v3++"\n"
   Mov v1 v2 -> v1++" := "++v2++"\n"
   RelPtrMov v1  offset v2 -> "["++v1++"+"++(show offset)++"] := "++v2++"\n"
   MovRelPtr v1 v2 offset -> v1++" := ["++v2++"+"++(show offset)++"] \n"
   Call label args -> label++"( "++(show args)++" )\n"
   CallRelPtr v1 offset args -> 
     "Call ["++v1++"+"++(show offset)++"] ("++(show args)++" )\n"
     
data Op =
   Plus
 | Minus deriving (Show)
   
show1 :: Op -> String
show1 a = "qqq"
{-  
  case a of
  Plus -> "+"
  Minus -> "-"
  Times -> "*"
  Div -> "/"
  Mod -> "%"
  LTH -> "<"
  LE  -> "<="
  GTH -> ">"
  GE  -> ">="
  EQU -> "=="
  NE  -> "!="
-}