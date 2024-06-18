open HolKernel Parse boolLib bossLib;
open stringTheory hoareSyntaxDefTheory;

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
