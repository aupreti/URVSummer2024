open HolKernel Parse boolLib bossLib;
open stringTheory hoareSyntaxDefTheory hoareSemanticsDefTheory;
open combinTheory hoareProofDefTheory;

val _ = new_theory "factorialDef";

set_mapped_fixity {tok = ";;" , fixity =  Infix(RIGHT, 200) , term_name = "CSeq"};
set_mapped_fixity {tok = "::==" , fixity =  Infix(RIGHT, 199) , term_name = "CAsgn"};


Definition factorial_prog_def:
  factorial_prog : com =
  CSeq
  (CAsgn "Y" (ANum 1))
  (CWhile (BNot  (BEq (AId "X") (ANum 0)))
   (CSeq
    (CAsgn "Y" (AMult (AId "Y") (AId "X")))
    (CAsgn "X" (AMinus (AId "X") (ANum 1)))))
End

Definition fact_spec_def:
  (fact_spec 0  = 1) ∧
  (fact_spec (SUC n0)  =  (SUC n0) * (fact_spec n0))
End

Definition precond_factorial_def:
  precond_factorial n : assert =
  (λenv. env "X" = n)
End
        
Definition post_factorial_def:
  post_factorial n : assert =
   (λenv. env "Y" = fact_spec n)
End
 
Theorem factorial_correct:
  ∀n. Hoare (precond_factorial n) factorial_prog (post_factorial n)
Proof
  simp[factorial_prog_def, precond_factorial_def, post_factorial_def, Once Hoare_cases]
  >> strip_tac >> DISJ2_TAC >> qexists_tac ‘(λenv. 1 * (fact_spec (env "X")) = fact_spec n)’
  >> simp[] >> simp[Once Hoare_cases, assert_subst_def, FUN_EQ_THM]
  >> qexists_tac ‘(λenv. env "Y" * fact_spec (env "X") = fact_spec n /\ env "X" = 0)’ >> rw[]
                                                                                                
  >-(DISJ1_TAC >> simp[Once Hoare_cases, assert_subst_def, FUN_EQ_THM]
     >> qexists_tac ‘(λenv. env "Y" * fact_spec (env "X") = fact_spec n)’
     >> simp[] >> rw[]
     (* Assignment  Y ::= ANum 1 *)
     >-(DISJ1_TAC >> simp[Once eval_aexpr_cases, APPLY_UPDATE_THM])
     (* while loop *)
     >-(simp[Once Hoare_cases]
       (* TODO *)
       )

    )
        
  (* Consequence rule at the end *)
  >-(gs[fact_spec_def])
  
QED

(*
Example factorial_dec (m:nat) : dcom := (
    {{ fun st => st X = m }} ->>
    {{ fun st => 1 * real_fact (st X) = real_fact m }}
  Y ::= ANum 1
    {{ fun st => st Y * real_fact (st X) = real_fact m }};;
  WHILE BNot (BEq (AId X) (ANum 0))
  DO   {{ fun st => st Y * real_fact (st X) = real_fact m /\ st X <> 0 }} ->>
       {{ fun st => st Y * st X * real_fact (st X - 1) = real_fact m }}
     Y ::= AMult (AId Y) (AId X)
       {{ fun st => st Y * real_fact (st X - 1) = real_fact m }};;
     X ::= AMinus (AId X) (ANum 1)
       {{ fun st => st Y * real_fact (st X) = real_fact m }}
  END
    {{ fun st => st Y * real_fact (st X) = real_fact m /\ st X = 0 }} ->>
    {{ fun st => st Y = real_fact m }}
) % dcom.
*)

val _ = export_theory();
