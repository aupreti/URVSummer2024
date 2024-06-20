open HolKernel Parse boolLib bossLib;
open stringTheory hoareSyntaxDefTheory;

val _ = new_theory "hoareSemanticsDef";

val _ = type_abbrev("var"  , ``:string``);
val _ = type_abbrev("env"  , ``:string -> num``);
val _ = type_abbrev("assert"  , ``:env -> bool``);

Definition assert_subst_def:
  assert_subst (P: assert) (x: var) (ae: aexp) : assert =
  (λenv.
     ∃a. eval_aexpr env ae a ⇒ P env⦇x ↦ a⦈)
End

Inductive Hoare:
  (Hoare P CSkip P) ∧
  (Hoare (assert_subst Q v ae) (CAsgn v ae) Q) ∧
  (Hoare P c1 Q ∧ Hoare Q c2 R ⇒ Hoare P (Cseq c1 c2) R) ∧
  (Hoare (λenv. P env ∧ eval_bexpr env b) c P ⇒ Hoare P (CWhile b c) (λenv. P env ∧ ¬(eval_bexpr env b)))
   
End

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
