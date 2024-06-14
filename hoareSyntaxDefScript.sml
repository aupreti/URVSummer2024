open HolKernel Parse boolLib bossLib;
open stringTheory;

val _ = new_theory "hoareSyntaxDef";

val _ = type_abbrev("var"  , ``:string``);
    
val _ = Datatype‘
         aexp =
         | ANum num
         | APlus aexp aexp
         | AMinus  aexp aexp
         | AMult aexp aexp
        ’;

val _ = Datatype‘
         bexp =
         | BTrue
         | BFalse
         | BEq aexp aexp 
         | BNeq aexp aexp
         | BLe aexp aexp 
         | BGt aexp aexp 
         | BNot bexp
         | BAnd bexp bexp   
        ’;


val _ = Datatype‘
         com = 
         | CSkip
         | CAsgn var aexp
         | CSeq  com com 
         | CIf bexp com com
         | CWhile bexp com
        ’;
        
val _ = export_theory();
