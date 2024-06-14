open HolKernel Parse boolLib bossLib;
open stringTheory hoareSyntaxDefTheory;

val _ = new_theory "hoareSemanticsDef";
val _ = type_abbrev("env"  , ``:string -> num``);

Inductive eval_aexpr:
  (eval_aexpr (ANum n) n) ∧
  (eval_aexpr ae1 n1 ∧ eval_aexpr ae2 n2 ⇒ eval_aexpr (APlus ae1 ae2) (n1 + n2)) ∧
  (eval_aexpr ae1 n1 ∧ eval_aexpr ae2 n2 ⇒ eval_aexpr (AMinus ae1 ae2) (n1 - n2)) ∧
  (eval_aexpr ae1 n1 ∧ eval_aexpr ae2 n2 ⇒ eval_aexpr (AMult ae1 ae2) (n1 * n2))
End


Inductive eval_bexpr:
  (eval_bexpr BTrue T) ∧
  (eval_bexpr BFalse F) ∧
  (eval_aexpr ae1 n1 ∧ eval_aexpr ae2 n2 ⇒ eval_bexpr (BEq ae1 ae2) (n1 = n2)) ∧
  (eval_aexpr ae1 n1 ∧ eval_aexpr ae2 n2 ⇒ eval_bexpr (BNeq ae1 ae2) (n1 ≠ n2)) ∧
  (eval_aexpr ae1 n1 ∧ eval_aexpr ae2 n2 ⇒ eval_bexpr (BLe ae1 ae2) (n1 < n2)) ∧
  (eval_aexpr ae1 n1 ∧ eval_aexpr ae2 n2 ⇒ eval_bexpr (BGt ae1 ae2) (n1 > n2)) ∧
  (eval_bexpr be1 T ⇒ eval_bexpr (BNot be1) F) ∧
  (eval_bexpr be1 F ⇒ eval_bexpr (BNot be1) T)  
End

Inductive eval_com:
  (eval_aexpr ae n ⇒ eval_com env (CAsgn v ae) (env⦇v ↦ n⦈))
End

(*val _ = Datatype‘
         com = 
         | CSkip
         | CAsgn var aexp
         | CSeq  com com 
         | CIf bexp com com
         | CWhile bexp com
        ’;

*)



                
val _ = export_theory();
