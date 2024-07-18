open HolKernel Parse boolLib bossLib;
open stringTheory hoareSyntaxDefTheory hoareSemanticsDefTheory hoareProofDefTheory;

val _ = new_theory "factorialDef";


set_mapped_fixity {tok = ";;" , fixity =  Infix(RIGHT, 200) , term_name = "CSeq"};
set_mapped_fixity {tok = "::==" , fixity =  Infix(RIGHT, 199) , term_name = "CAsgn"};


Definition factorial_prog_def:
  factorial_prog : com =
  CSeq
  (CSeq
   (CAsgn "Z" (AId "X"))
   (CAsgn "Y" (ANum 1)))
  (CWhile (BNot  (BEq (AId "Z") (ANum 0)))
   (CSeq
    (CAsgn "Y" (AMult (AId "X") (AId "Z")))
    (CAsgn "Z" (AMinus (AId "Z") (ANum 1)))))
End

Definition fact_spec_def:
  (fact_spec 0  = 1) ∧
  (fact_spec (SUC n0)  =  (SUC n0) * (fact_spec n0))
End

Definition precond_factorial_def:
  precond_factorial n : assert =
  (λenv. env "Z" = 0 ∧ env "Y" = 0 ∧ env "X" = n  )
End
        
Definition post_factorial_def:
  post_factorial n : assert =
   (λenv. env "Y" = fact_spec n)
End

Theorem factorial_correct:
  ∀n. Hoare (precond_factorial n) factorial_prog (post_factorial n)
Proof
QED

(*
Definition fact_in_coq : com :=
  <{ Z := X;
     Y := 1;
     while Z ≠ 0 do
       Y := Y × Z;
       Z := Z - 1
              end }>.
*)

val _ = export_theory();
