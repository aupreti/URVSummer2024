open HolKernel Parse boolLib bossLib;
open stringTheory hoareSyntaxDefTheory;

val _ = new_theory "hoareSemanticsDef";

Inductive eval_expr:
  (eval_expr (ANum n) n) ∧
  (eval_expr ae1 n1 ∧ eval_expr ae2 n2 ⇒ eval_expr (APlus ae1 ae2) (n1 + n2))
End

val _ = export_theory();
