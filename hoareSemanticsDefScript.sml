open HolKernel Parse boolLib bossLib;
open stringTheory hoareSyntaxDefTheory;

val _ = new_theory "hoareSemanticsDef";

Inductive eval_aexpr:
  (eval_aexpr env (ANum n) n) ∧
  (eval_aexpr env (AId x) (env x)) ∧
  (eval_aexpr env ae1 n1 ∧ eval_aexpr env ae2 n2 ⇒ eval_aexpr env (APlus ae1 ae2) (n1 + n2)) ∧
  (eval_aexpr env ae1 n1 ∧ eval_aexpr env ae2 n2 ⇒ eval_aexpr env (AMinus ae1 ae2) (n1 - n2)) ∧
  (eval_aexpr env ae1 n1 ∧ eval_aexpr env ae2 n2 ⇒ eval_aexpr env (AMult ae1 ae2) (n1 * n2))
End


Inductive eval_bexpr:
  (eval_bexpr env BTrue T) ∧
  (eval_bexpr env BFalse F) ∧
  (eval_aexpr env ae1 n1 ∧ eval_aexpr env ae2 n2 ⇒ eval_bexpr env (BEq ae1 ae2) (n1 = n2)) ∧
  (eval_aexpr env ae1 n1 ∧ eval_aexpr env ae2 n2 ⇒ eval_bexpr env (BNeq ae1 ae2) (n1 ≠ n2)) ∧
  (eval_aexpr env ae1 n1 ∧ eval_aexpr env ae2 n2 ⇒ eval_bexpr env (BLe ae1 ae2) (n1 < n2)) ∧
  (eval_aexpr env ae1 n1 ∧ eval_aexpr env ae2 n2 ⇒ eval_bexpr env (BGt ae1 ae2) (n1 > n2)) ∧
  (eval_bexpr env be1 T ⇒ eval_bexpr env (BNot be1) F) ∧
  (eval_bexpr env be1 F ⇒ eval_bexpr env (BNot be1) T)  
End

Inductive eval_com:
  (eval_aexpr env ae n ⇒ eval_com env (CAsgn v ae) (env⦇v ↦ n⦈))∧
  (eval_com env1 CSkip env1) ∧
  (eval_com env1 c1 env2 ∧ eval_com env2 c2 env3 ⇒ eval_com env1 (CSeq c1 c2) env3) ∧
  (eval_bexpr env1 be T ∧ eval_com env1 c1 env2 ⇒ eval_com env1 (CIf be c1 c2) env2) ∧
  (eval_bexpr env1 be F ∧ eval_com env1 c2 env2 ⇒ eval_com env1 (CIf be c1 c2) env2) ∧
  (eval_bexpr env1 be T ∧ eval_com env1 c env2 ∧ eval_com env2 (CWhile be c) env3 ⇒ eval_com env1 (CWhile be c) env3) ∧
  (eval_bexpr env be F ⇒ eval_com env (CWhile be c) env)
End

val _ = export_theory();
