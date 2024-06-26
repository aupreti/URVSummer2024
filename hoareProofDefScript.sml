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
  (Hoare P c1 Q ∧ Hoare Q c2 R ⇒ Hoare P (Cseq c1 c2) R) ∧
  (Hoare (λenv. P env ∧ eval_expr env b T) c1 Q ∧
   Hoare (λenv. P env ∧ eval_expr env b F) c2 Q ⇒ Hoare P (CIf b c1 c2) Q) ∧  
  (Hoare (λenv. P env ∧ eval_bexpr env b T) c P ⇒ Hoare P (CWhile b c) (λenv. P env ∧ (eval_bexpr env b F))) ∧ 
  (∀ P1 Q1. (∀ env. P0 env ⇒ P1 env) ∧ Hoare P1 c Q1 ∧ (∀ env. Q1 env ⇒ Q0 env) ⇒ Hoare P0 c Q0)
End

Definition is_valid_def:
  is_valid (P: assert) (c: com) (Q: assert) =
  (∀ env0 env1. P env0 ∧ eval_com env0 c env1 ⇒ Q env1)
End


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
  >-(cheat)
  (* If *)
  >-(cheat)
  (* While *)
  >-(cheat)
  (* Consequence *)
  >-(cheat)
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
