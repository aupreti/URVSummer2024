open HolKernel Parse boolLib bossLib;
open stringTheory hoareSyntaxDefTheory  hoareSemanticsDefTheory;

val _ = new_theory "hoareProofDef";

val _ = type_abbrev("var"  , ``:string``);
val _ = type_abbrev("env"  , ``:string -> num``);
val _ = type_abbrev("assert"  , ``:env -> bool``);

Definition assert_subst_def:
  assert_subst (P: assert) (x: var) (ae: aexp) : assert =
  (λenv.
     ∀a. eval_aexpr env ae a ⇒ P env⦇x ↦ a⦈)
End

Inductive Hoare:
  (Hoare P CSkip P) ∧
  (Hoare (assert_subst Q v ae) (CAsgn v ae) Q) ∧
  (Hoare P c1 Q ∧ Hoare Q c2 R ⇒ Hoare P (CSeq c1 c2) R) ∧
  (Hoare (λenv. P env ∧ eval_bexpr env b T) c1 Q ∧
   Hoare (λenv. P env ∧ eval_bexpr env b F) c2 Q ⇒ Hoare P (CIf b c1 c2) Q) ∧  
  (Hoare (λenv. P env ∧ eval_bexpr env b T) c P ⇒ Hoare P (CWhile b c) (λenv. P env ∧ (eval_bexpr env b F))) ∧ 
  (∀ P1 Q1. (∀ env. P0 env ⇒ P1 env) ∧ Hoare P1 c Q1 ∧ (∀ env. Q1 env ⇒ Q0 env) ⇒ Hoare P0 c Q0)
End

Definition is_valid_def:
  is_valid (P: assert) (c: com) (Q: assert) =
  (∀ env0 env1. P env0 ∧ eval_com env0 c env1 ⇒ Q env1)
End

Theorem hoare_consequence_pre:
  ∀P0 P c Q. Hoare P0 c Q ∧ (∀env. P env ⇒ P0 env) ⇒ Hoare P c Q
Proof
  rw[] >> simp[Once Hoare_cases] >> rw[] >> NTAC 5 disj2_tac
  >> qexists_tac ‘P0’ >> qexists_tac ‘Q’ >> simp[]
QED
        
Theorem hoare_consequence_post:
  ∀P c Q Q0. Hoare P c Q0 ∧(∀env. Q0 env ⇒ Q env) ⇒ Hoare P c Q
Proof
  rw[] >> simp[Once Hoare_cases] >> rw[] >> NTAC 5 disj2_tac
  >> qexists_tac ‘P’ >> qexists_tac ‘Q0’ >> simp[]  
QED

Theorem hoare_sound :
  ∀ P c Q. Hoare P c Q ⇒ is_valid P c Q
Proof
  Induct_on ‘Hoare’ >> rw[]
  (* Skip *)
  >-(simp[is_valid_def, Once eval_com_cases])
  (* Assignment *)
  >-(simp [is_valid_def, assert_subst_def, Once eval_com_cases]
     >> rw[] >> first_x_assum drule >> simp[])
  (* Seq *)
  >-(fs[is_valid_def] >> simp[Once eval_com_cases] >> rw[] >> gs[]
     >> qpat_x_assum ‘∀env0 env1. P env0 ∧ eval_com env0 c1 env1 ⇒ Q env1’ drule_all
     >> rw[] >> first_x_assum irule >> qexists_tac ‘env2’ >> gs[])
  (* If *)
  >-(fs[is_valid_def] >> simp[Once eval_com_cases] >> rw[]
     >-(last_x_assum (qspecl_then [‘env0’, ‘env1’] assume_tac) >> fs[])
     >-(first_x_assum (qspecl_then [‘env0’, ‘env1’] assume_tac) >> fs[]))
  (* While *)
  >-(fs[is_valid_def] >> Induct_on ‘eval_com’ >> rw[]
     >> first_x_assum drule_all >> gs[])
  (* Consequence *)
  (*  >-(fs[is_valid_def] >> metis_tac[]) works, but the following is more step-by-step approach. *)
  >-(fs[is_valid_def] >>  drule_all hoare_consequence_pre >> drule_all hoare_consequence_post  >> rw[]
     >> first_x_assum irule >> first_x_assum irule >> qexists_tac ‘env0’ >> gs[])
QED

(*
Inductive derivable : Assertion → com → Assertion → Type :=
  | H_Skip : ∀ P,
      derivable P <{skip}> P
  | H_Asgn : ∀ Q V a,
      derivable (Q [V ⊢> a]) <{V := a}> Q
  | H_Seq : ∀ P c Q d R,
      derivable Q d R → derivable P c Q → derivable P <{c;d}> R
  | H_If : ∀ P Q b c1 c2,
    derivable (fun st ⇒ P st ∧ bassertion b st) c1 Q →
    derivable (fun st ⇒ P st ∧ ~(bassertion b st)) c2 Q →
    derivable P <{if b then c1 else c2 end}> Q
  | H_While : ∀ P b c,
    derivable (fun st ⇒ P st ∧ bassertion b st) c P →
    derivable P <{while b do c end}> (fun st ⇒ P st ∧ ¬ (bassertion b st))
  | H_Consequence : ∀ (P Q P' Q' : Assertion) c,
    derivable P' c Q' →
    (∀ st, P st → P' st) →
    (∀ st, Q' st → Q st) →
    derivable P c Q.
End
*)

val _ = export_theory();
