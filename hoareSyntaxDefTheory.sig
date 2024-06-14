signature hoareSyntaxDefTheory =
sig
  type thm = Thm.thm
  
  (*  Definitions  *)
    val aexp_TY_DEF : thm
    val aexp_case_def : thm
    val aexp_size_def : thm
    val bexp_TY_DEF : thm
    val bexp_case_def : thm
    val bexp_size_def : thm
    val com_TY_DEF : thm
    val com_case_def : thm
    val com_size_def : thm
  
  (*  Theorems  *)
    val aexp_11 : thm
    val aexp_Axiom : thm
    val aexp_case_cong : thm
    val aexp_case_eq : thm
    val aexp_distinct : thm
    val aexp_induction : thm
    val aexp_nchotomy : thm
    val bexp_11 : thm
    val bexp_Axiom : thm
    val bexp_case_cong : thm
    val bexp_case_eq : thm
    val bexp_distinct : thm
    val bexp_induction : thm
    val bexp_nchotomy : thm
    val com_11 : thm
    val com_Axiom : thm
    val com_case_cong : thm
    val com_case_eq : thm
    val com_distinct : thm
    val com_induction : thm
    val com_nchotomy : thm
    val datatype_aexp : thm
    val datatype_bexp : thm
    val datatype_com : thm
  
  val hoareSyntaxDef_grammars : type_grammar.grammar * term_grammar.grammar
(*
   [string] Parent theory of "hoareSyntaxDef"
   
   [aexp_TY_DEF]  Definition
      
      ⊢ ∃rep.
          TYPE_DEFINITION
            (λa0'.
                 ∀ $var$('aexp').
                   (∀a0'.
                      (∃a. a0' =
                           (λa. ind_type$CONSTR 0 a (λn. ind_type$BOTTOM))
                             a) ∨
                      (∃a0 a1.
                         a0' =
                         (λa0 a1.
                              ind_type$CONSTR (SUC 0) ARB
                                (ind_type$FCONS a0
                                   (ind_type$FCONS a1 (λn. ind_type$BOTTOM))))
                           a0 a1 ∧ $var$('aexp') a0 ∧ $var$('aexp') a1) ∨
                      (∃a0 a1.
                         a0' =
                         (λa0 a1.
                              ind_type$CONSTR (SUC (SUC 0)) ARB
                                (ind_type$FCONS a0
                                   (ind_type$FCONS a1 (λn. ind_type$BOTTOM))))
                           a0 a1 ∧ $var$('aexp') a0 ∧ $var$('aexp') a1) ∨
                      (∃a0 a1.
                         a0' =
                         (λa0 a1.
                              ind_type$CONSTR (SUC (SUC (SUC 0))) ARB
                                (ind_type$FCONS a0
                                   (ind_type$FCONS a1 (λn. ind_type$BOTTOM))))
                           a0 a1 ∧ $var$('aexp') a0 ∧ $var$('aexp') a1) ⇒
                      $var$('aexp') a0') ⇒
                   $var$('aexp') a0') rep
   
   [aexp_case_def]  Definition
      
      ⊢ (∀a f f1 f2 f3. aexp_CASE (ANum a) f f1 f2 f3 = f a) ∧
        (∀a0 a1 f f1 f2 f3. aexp_CASE (APlus a0 a1) f f1 f2 f3 = f1 a0 a1) ∧
        (∀a0 a1 f f1 f2 f3. aexp_CASE (AMinus a0 a1) f f1 f2 f3 = f2 a0 a1) ∧
        ∀a0 a1 f f1 f2 f3. aexp_CASE (AMult a0 a1) f f1 f2 f3 = f3 a0 a1
   
   [aexp_size_def]  Definition
      
      ⊢ (∀a. aexp_size (ANum a) = 1 + a) ∧
        (∀a0 a1.
           aexp_size (APlus a0 a1) = 1 + (aexp_size a0 + aexp_size a1)) ∧
        (∀a0 a1.
           aexp_size (AMinus a0 a1) = 1 + (aexp_size a0 + aexp_size a1)) ∧
        ∀a0 a1. aexp_size (AMult a0 a1) = 1 + (aexp_size a0 + aexp_size a1)
   
   [bexp_TY_DEF]  Definition
      
      ⊢ ∃rep.
          TYPE_DEFINITION
            (λa0'.
                 ∀ $var$('bexp').
                   (∀a0'.
                      a0' =
                      ind_type$CONSTR 0 (ARB,ARB) (λn. ind_type$BOTTOM) ∨
                      a0' =
                      ind_type$CONSTR (SUC 0) (ARB,ARB)
                        (λn. ind_type$BOTTOM) ∨
                      (∃a0 a1.
                         a0' =
                         (λa0 a1.
                              ind_type$CONSTR (SUC (SUC 0)) (a0,a1)
                                (λn. ind_type$BOTTOM)) a0 a1) ∨
                      (∃a0 a1.
                         a0' =
                         (λa0 a1.
                              ind_type$CONSTR (SUC (SUC (SUC 0))) (a0,a1)
                                (λn. ind_type$BOTTOM)) a0 a1) ∨
                      (∃a0 a1.
                         a0' =
                         (λa0 a1.
                              ind_type$CONSTR (SUC (SUC (SUC (SUC 0))))
                                (a0,a1) (λn. ind_type$BOTTOM)) a0 a1) ∨
                      (∃a0 a1.
                         a0' =
                         (λa0 a1.
                              ind_type$CONSTR
                                (SUC (SUC (SUC (SUC (SUC 0))))) (a0,a1)
                                (λn. ind_type$BOTTOM)) a0 a1) ∨
                      (∃a. a0' =
                           (λa.
                                ind_type$CONSTR
                                  (SUC (SUC (SUC (SUC (SUC (SUC 0))))))
                                  (ARB,ARB)
                                  (ind_type$FCONS a (λn. ind_type$BOTTOM)))
                             a ∧ $var$('bexp') a) ∨
                      (∃a0 a1.
                         a0' =
                         (λa0 a1.
                              ind_type$CONSTR
                                (SUC (SUC (SUC (SUC (SUC (SUC (SUC 0)))))))
                                (ARB,ARB)
                                (ind_type$FCONS a0
                                   (ind_type$FCONS a1 (λn. ind_type$BOTTOM))))
                           a0 a1 ∧ $var$('bexp') a0 ∧ $var$('bexp') a1) ⇒
                      $var$('bexp') a0') ⇒
                   $var$('bexp') a0') rep
   
   [bexp_case_def]  Definition
      
      ⊢ (∀v v1 f f1 f2 f3 f4 f5. bexp_CASE BTrue v v1 f f1 f2 f3 f4 f5 = v) ∧
        (∀v v1 f f1 f2 f3 f4 f5.
           bexp_CASE BFalse v v1 f f1 f2 f3 f4 f5 = v1) ∧
        (∀a0 a1 v v1 f f1 f2 f3 f4 f5.
           bexp_CASE (BEq a0 a1) v v1 f f1 f2 f3 f4 f5 = f a0 a1) ∧
        (∀a0 a1 v v1 f f1 f2 f3 f4 f5.
           bexp_CASE (BNeq a0 a1) v v1 f f1 f2 f3 f4 f5 = f1 a0 a1) ∧
        (∀a0 a1 v v1 f f1 f2 f3 f4 f5.
           bexp_CASE (BLe a0 a1) v v1 f f1 f2 f3 f4 f5 = f2 a0 a1) ∧
        (∀a0 a1 v v1 f f1 f2 f3 f4 f5.
           bexp_CASE (BGt a0 a1) v v1 f f1 f2 f3 f4 f5 = f3 a0 a1) ∧
        (∀a v v1 f f1 f2 f3 f4 f5.
           bexp_CASE (BNot a) v v1 f f1 f2 f3 f4 f5 = f4 a) ∧
        ∀a0 a1 v v1 f f1 f2 f3 f4 f5.
          bexp_CASE (BAnd a0 a1) v v1 f f1 f2 f3 f4 f5 = f5 a0 a1
   
   [bexp_size_def]  Definition
      
      ⊢ bexp_size BTrue = 0 ∧ bexp_size BFalse = 0 ∧
        (∀a0 a1. bexp_size (BEq a0 a1) = 1 + (aexp_size a0 + aexp_size a1)) ∧
        (∀a0 a1. bexp_size (BNeq a0 a1) = 1 + (aexp_size a0 + aexp_size a1)) ∧
        (∀a0 a1. bexp_size (BLe a0 a1) = 1 + (aexp_size a0 + aexp_size a1)) ∧
        (∀a0 a1. bexp_size (BGt a0 a1) = 1 + (aexp_size a0 + aexp_size a1)) ∧
        (∀a. bexp_size (BNot a) = 1 + bexp_size a) ∧
        ∀a0 a1. bexp_size (BAnd a0 a1) = 1 + (bexp_size a0 + bexp_size a1)
   
   [com_TY_DEF]  Definition
      
      ⊢ ∃rep.
          TYPE_DEFINITION
            (λa0'.
                 ∀ $var$('com').
                   (∀a0'.
                      a0' =
                      ind_type$CONSTR 0 (ARB,ARB,ARB) (λn. ind_type$BOTTOM) ∨
                      (∃a0 a1.
                         a0' =
                         (λa0 a1.
                              ind_type$CONSTR (SUC 0) (a0,a1,ARB)
                                (λn. ind_type$BOTTOM)) a0 a1) ∨
                      (∃a0 a1.
                         a0' =
                         (λa0 a1.
                              ind_type$CONSTR (SUC (SUC 0)) (ARB,ARB,ARB)
                                (ind_type$FCONS a0
                                   (ind_type$FCONS a1 (λn. ind_type$BOTTOM))))
                           a0 a1 ∧ $var$('com') a0 ∧ $var$('com') a1) ∨
                      (∃a0 a1 a2.
                         a0' =
                         (λa0 a1 a2.
                              ind_type$CONSTR (SUC (SUC (SUC 0)))
                                (ARB,ARB,a0)
                                (ind_type$FCONS a1
                                   (ind_type$FCONS a2 (λn. ind_type$BOTTOM))))
                           a0 a1 a2 ∧ $var$('com') a1 ∧ $var$('com') a2) ∨
                      (∃a0 a1.
                         a0' =
                         (λa0 a1.
                              ind_type$CONSTR (SUC (SUC (SUC (SUC 0))))
                                (ARB,ARB,a0)
                                (ind_type$FCONS a1 (λn. ind_type$BOTTOM)))
                           a0 a1 ∧ $var$('com') a1) ⇒
                      $var$('com') a0') ⇒
                   $var$('com') a0') rep
   
   [com_case_def]  Definition
      
      ⊢ (∀v f f1 f2 f3. com_CASE CSkip v f f1 f2 f3 = v) ∧
        (∀a0 a1 v f f1 f2 f3. com_CASE (CAsgn a0 a1) v f f1 f2 f3 = f a0 a1) ∧
        (∀a0 a1 v f f1 f2 f3. com_CASE (CSeq a0 a1) v f f1 f2 f3 = f1 a0 a1) ∧
        (∀a0 a1 a2 v f f1 f2 f3.
           com_CASE (CIf a0 a1 a2) v f f1 f2 f3 = f2 a0 a1 a2) ∧
        ∀a0 a1 v f f1 f2 f3.
          com_CASE (CWhile a0 a1) v f f1 f2 f3 = f3 a0 a1
   
   [com_size_def]  Definition
      
      ⊢ com_size CSkip = 0 ∧
        (∀a0 a1.
           com_size (CAsgn a0 a1) =
           1 + (list_size char_size a0 + aexp_size a1)) ∧
        (∀a0 a1. com_size (CSeq a0 a1) = 1 + (com_size a0 + com_size a1)) ∧
        (∀a0 a1 a2.
           com_size (CIf a0 a1 a2) =
           1 + (bexp_size a0 + (com_size a1 + com_size a2))) ∧
        ∀a0 a1. com_size (CWhile a0 a1) = 1 + (bexp_size a0 + com_size a1)
   
   [aexp_11]  Theorem
      
      ⊢ (∀a a'. ANum a = ANum a' ⇔ a = a') ∧
        (∀a0 a1 a0' a1'. APlus a0 a1 = APlus a0' a1' ⇔ a0 = a0' ∧ a1 = a1') ∧
        (∀a0 a1 a0' a1'.
           AMinus a0 a1 = AMinus a0' a1' ⇔ a0 = a0' ∧ a1 = a1') ∧
        ∀a0 a1 a0' a1'. AMult a0 a1 = AMult a0' a1' ⇔ a0 = a0' ∧ a1 = a1'
   
   [aexp_Axiom]  Theorem
      
      ⊢ ∀f0 f1 f2 f3. ∃fn.
          (∀a. fn (ANum a) = f0 a) ∧
          (∀a0 a1. fn (APlus a0 a1) = f1 a0 a1 (fn a0) (fn a1)) ∧
          (∀a0 a1. fn (AMinus a0 a1) = f2 a0 a1 (fn a0) (fn a1)) ∧
          ∀a0 a1. fn (AMult a0 a1) = f3 a0 a1 (fn a0) (fn a1)
   
   [aexp_case_cong]  Theorem
      
      ⊢ ∀M M' f f1 f2 f3.
          M = M' ∧ (∀a. M' = ANum a ⇒ f a = f' a) ∧
          (∀a0 a1. M' = APlus a0 a1 ⇒ f1 a0 a1 = f1' a0 a1) ∧
          (∀a0 a1. M' = AMinus a0 a1 ⇒ f2 a0 a1 = f2' a0 a1) ∧
          (∀a0 a1. M' = AMult a0 a1 ⇒ f3 a0 a1 = f3' a0 a1) ⇒
          aexp_CASE M f f1 f2 f3 = aexp_CASE M' f' f1' f2' f3'
   
   [aexp_case_eq]  Theorem
      
      ⊢ aexp_CASE x f f1 f2 f3 = v ⇔
        (∃n. x = ANum n ∧ f n = v) ∨
        (∃a a0. x = APlus a a0 ∧ f1 a a0 = v) ∨
        (∃a a0. x = AMinus a a0 ∧ f2 a a0 = v) ∨
        ∃a a0. x = AMult a a0 ∧ f3 a a0 = v
   
   [aexp_distinct]  Theorem
      
      ⊢ (∀a1 a0 a. ANum a ≠ APlus a0 a1) ∧
        (∀a1 a0 a. ANum a ≠ AMinus a0 a1) ∧
        (∀a1 a0 a. ANum a ≠ AMult a0 a1) ∧
        (∀a1' a1 a0' a0. APlus a0 a1 ≠ AMinus a0' a1') ∧
        (∀a1' a1 a0' a0. APlus a0 a1 ≠ AMult a0' a1') ∧
        ∀a1' a1 a0' a0. AMinus a0 a1 ≠ AMult a0' a1'
   
   [aexp_induction]  Theorem
      
      ⊢ ∀P. (∀n. P (ANum n)) ∧ (∀a a0. P a ∧ P a0 ⇒ P (APlus a a0)) ∧
            (∀a a0. P a ∧ P a0 ⇒ P (AMinus a a0)) ∧
            (∀a a0. P a ∧ P a0 ⇒ P (AMult a a0)) ⇒
            ∀a. P a
   
   [aexp_nchotomy]  Theorem
      
      ⊢ ∀aa.
          (∃n. aa = ANum n) ∨ (∃a a0. aa = APlus a a0) ∨
          (∃a a0. aa = AMinus a a0) ∨ ∃a a0. aa = AMult a a0
   
   [bexp_11]  Theorem
      
      ⊢ (∀a0 a1 a0' a1'. BEq a0 a1 = BEq a0' a1' ⇔ a0 = a0' ∧ a1 = a1') ∧
        (∀a0 a1 a0' a1'. BNeq a0 a1 = BNeq a0' a1' ⇔ a0 = a0' ∧ a1 = a1') ∧
        (∀a0 a1 a0' a1'. BLe a0 a1 = BLe a0' a1' ⇔ a0 = a0' ∧ a1 = a1') ∧
        (∀a0 a1 a0' a1'. BGt a0 a1 = BGt a0' a1' ⇔ a0 = a0' ∧ a1 = a1') ∧
        (∀a a'. BNot a = BNot a' ⇔ a = a') ∧
        ∀a0 a1 a0' a1'. BAnd a0 a1 = BAnd a0' a1' ⇔ a0 = a0' ∧ a1 = a1'
   
   [bexp_Axiom]  Theorem
      
      ⊢ ∀f0 f1 f2 f3 f4 f5 f6 f7. ∃fn.
          fn BTrue = f0 ∧ fn BFalse = f1 ∧
          (∀a0 a1. fn (BEq a0 a1) = f2 a0 a1) ∧
          (∀a0 a1. fn (BNeq a0 a1) = f3 a0 a1) ∧
          (∀a0 a1. fn (BLe a0 a1) = f4 a0 a1) ∧
          (∀a0 a1. fn (BGt a0 a1) = f5 a0 a1) ∧
          (∀a. fn (BNot a) = f6 a (fn a)) ∧
          ∀a0 a1. fn (BAnd a0 a1) = f7 a0 a1 (fn a0) (fn a1)
   
   [bexp_case_cong]  Theorem
      
      ⊢ ∀M M' v v1 f f1 f2 f3 f4 f5.
          M = M' ∧ (M' = BTrue ⇒ v = v') ∧ (M' = BFalse ⇒ v1 = v1') ∧
          (∀a0 a1. M' = BEq a0 a1 ⇒ f a0 a1 = f' a0 a1) ∧
          (∀a0 a1. M' = BNeq a0 a1 ⇒ f1 a0 a1 = f1' a0 a1) ∧
          (∀a0 a1. M' = BLe a0 a1 ⇒ f2 a0 a1 = f2' a0 a1) ∧
          (∀a0 a1. M' = BGt a0 a1 ⇒ f3 a0 a1 = f3' a0 a1) ∧
          (∀a. M' = BNot a ⇒ f4 a = f4' a) ∧
          (∀a0 a1. M' = BAnd a0 a1 ⇒ f5 a0 a1 = f5' a0 a1) ⇒
          bexp_CASE M v v1 f f1 f2 f3 f4 f5 =
          bexp_CASE M' v' v1' f' f1' f2' f3' f4' f5'
   
   [bexp_case_eq]  Theorem
      
      ⊢ bexp_CASE x v v1 f f1 f2 f3 f4 f5 = v' ⇔
        x = BTrue ∧ v = v' ∨ x = BFalse ∧ v1 = v' ∨
        (∃a a0. x = BEq a a0 ∧ f a a0 = v') ∨
        (∃a a0. x = BNeq a a0 ∧ f1 a a0 = v') ∨
        (∃a a0. x = BLe a a0 ∧ f2 a a0 = v') ∨
        (∃a a0. x = BGt a a0 ∧ f3 a a0 = v') ∨
        (∃b. x = BNot b ∧ f4 b = v') ∨ ∃b b0. x = BAnd b b0 ∧ f5 b b0 = v'
   
   [bexp_distinct]  Theorem
      
      ⊢ BTrue ≠ BFalse ∧ (∀a1 a0. BTrue ≠ BEq a0 a1) ∧
        (∀a1 a0. BTrue ≠ BNeq a0 a1) ∧ (∀a1 a0. BTrue ≠ BLe a0 a1) ∧
        (∀a1 a0. BTrue ≠ BGt a0 a1) ∧ (∀a. BTrue ≠ BNot a) ∧
        (∀a1 a0. BTrue ≠ BAnd a0 a1) ∧ (∀a1 a0. BFalse ≠ BEq a0 a1) ∧
        (∀a1 a0. BFalse ≠ BNeq a0 a1) ∧ (∀a1 a0. BFalse ≠ BLe a0 a1) ∧
        (∀a1 a0. BFalse ≠ BGt a0 a1) ∧ (∀a. BFalse ≠ BNot a) ∧
        (∀a1 a0. BFalse ≠ BAnd a0 a1) ∧
        (∀a1' a1 a0' a0. BEq a0 a1 ≠ BNeq a0' a1') ∧
        (∀a1' a1 a0' a0. BEq a0 a1 ≠ BLe a0' a1') ∧
        (∀a1' a1 a0' a0. BEq a0 a1 ≠ BGt a0' a1') ∧
        (∀a1 a0 a. BEq a0 a1 ≠ BNot a) ∧
        (∀a1' a1 a0' a0. BEq a0 a1 ≠ BAnd a0' a1') ∧
        (∀a1' a1 a0' a0. BNeq a0 a1 ≠ BLe a0' a1') ∧
        (∀a1' a1 a0' a0. BNeq a0 a1 ≠ BGt a0' a1') ∧
        (∀a1 a0 a. BNeq a0 a1 ≠ BNot a) ∧
        (∀a1' a1 a0' a0. BNeq a0 a1 ≠ BAnd a0' a1') ∧
        (∀a1' a1 a0' a0. BLe a0 a1 ≠ BGt a0' a1') ∧
        (∀a1 a0 a. BLe a0 a1 ≠ BNot a) ∧
        (∀a1' a1 a0' a0. BLe a0 a1 ≠ BAnd a0' a1') ∧
        (∀a1 a0 a. BGt a0 a1 ≠ BNot a) ∧
        (∀a1' a1 a0' a0. BGt a0 a1 ≠ BAnd a0' a1') ∧
        ∀a1 a0 a. BNot a ≠ BAnd a0 a1
   
   [bexp_induction]  Theorem
      
      ⊢ ∀P. P BTrue ∧ P BFalse ∧ (∀a a0. P (BEq a a0)) ∧
            (∀a a0. P (BNeq a a0)) ∧ (∀a a0. P (BLe a a0)) ∧
            (∀a a0. P (BGt a a0)) ∧ (∀b. P b ⇒ P (BNot b)) ∧
            (∀b b0. P b ∧ P b0 ⇒ P (BAnd b b0)) ⇒
            ∀b. P b
   
   [bexp_nchotomy]  Theorem
      
      ⊢ ∀bb.
          bb = BTrue ∨ bb = BFalse ∨ (∃a a0. bb = BEq a a0) ∨
          (∃a a0. bb = BNeq a a0) ∨ (∃a a0. bb = BLe a a0) ∨
          (∃a a0. bb = BGt a a0) ∨ (∃b. bb = BNot b) ∨
          ∃b b0. bb = BAnd b b0
   
   [com_11]  Theorem
      
      ⊢ (∀a0 a1 a0' a1'. CAsgn a0 a1 = CAsgn a0' a1' ⇔ a0 = a0' ∧ a1 = a1') ∧
        (∀a0 a1 a0' a1'. CSeq a0 a1 = CSeq a0' a1' ⇔ a0 = a0' ∧ a1 = a1') ∧
        (∀a0 a1 a2 a0' a1' a2'.
           CIf a0 a1 a2 = CIf a0' a1' a2' ⇔ a0 = a0' ∧ a1 = a1' ∧ a2 = a2') ∧
        ∀a0 a1 a0' a1'. CWhile a0 a1 = CWhile a0' a1' ⇔ a0 = a0' ∧ a1 = a1'
   
   [com_Axiom]  Theorem
      
      ⊢ ∀f0 f1 f2 f3 f4. ∃fn.
          fn CSkip = f0 ∧ (∀a0 a1. fn (CAsgn a0 a1) = f1 a0 a1) ∧
          (∀a0 a1. fn (CSeq a0 a1) = f2 a0 a1 (fn a0) (fn a1)) ∧
          (∀a0 a1 a2. fn (CIf a0 a1 a2) = f3 a0 a1 a2 (fn a1) (fn a2)) ∧
          ∀a0 a1. fn (CWhile a0 a1) = f4 a0 a1 (fn a1)
   
   [com_case_cong]  Theorem
      
      ⊢ ∀M M' v f f1 f2 f3.
          M = M' ∧ (M' = CSkip ⇒ v = v') ∧
          (∀a0 a1. M' = CAsgn a0 a1 ⇒ f a0 a1 = f' a0 a1) ∧
          (∀a0 a1. M' = CSeq a0 a1 ⇒ f1 a0 a1 = f1' a0 a1) ∧
          (∀a0 a1 a2. M' = CIf a0 a1 a2 ⇒ f2 a0 a1 a2 = f2' a0 a1 a2) ∧
          (∀a0 a1. M' = CWhile a0 a1 ⇒ f3 a0 a1 = f3' a0 a1) ⇒
          com_CASE M v f f1 f2 f3 = com_CASE M' v' f' f1' f2' f3'
   
   [com_case_eq]  Theorem
      
      ⊢ com_CASE x v f f1 f2 f3 = v' ⇔
        x = CSkip ∧ v = v' ∨ (∃s a. x = CAsgn s a ∧ f s a = v') ∨
        (∃c c0. x = CSeq c c0 ∧ f1 c c0 = v') ∨
        (∃b c c0. x = CIf b c c0 ∧ f2 b c c0 = v') ∨
        ∃b c. x = CWhile b c ∧ f3 b c = v'
   
   [com_distinct]  Theorem
      
      ⊢ (∀a1 a0. CSkip ≠ CAsgn a0 a1) ∧ (∀a1 a0. CSkip ≠ CSeq a0 a1) ∧
        (∀a2 a1 a0. CSkip ≠ CIf a0 a1 a2) ∧
        (∀a1 a0. CSkip ≠ CWhile a0 a1) ∧
        (∀a1' a1 a0' a0. CAsgn a0 a1 ≠ CSeq a0' a1') ∧
        (∀a2 a1' a1 a0' a0. CAsgn a0 a1 ≠ CIf a0' a1' a2) ∧
        (∀a1' a1 a0' a0. CAsgn a0 a1 ≠ CWhile a0' a1') ∧
        (∀a2 a1' a1 a0' a0. CSeq a0 a1 ≠ CIf a0' a1' a2) ∧
        (∀a1' a1 a0' a0. CSeq a0 a1 ≠ CWhile a0' a1') ∧
        ∀a2 a1' a1 a0' a0. CIf a0 a1 a2 ≠ CWhile a0' a1'
   
   [com_induction]  Theorem
      
      ⊢ ∀P. P CSkip ∧ (∀s a. P (CAsgn s a)) ∧
            (∀c c0. P c ∧ P c0 ⇒ P (CSeq c c0)) ∧
            (∀c c0. P c ∧ P c0 ⇒ ∀b. P (CIf b c c0)) ∧
            (∀c. P c ⇒ ∀b. P (CWhile b c)) ⇒
            ∀c. P c
   
   [com_nchotomy]  Theorem
      
      ⊢ ∀cc.
          cc = CSkip ∨ (∃s a. cc = CAsgn s a) ∨ (∃c c0. cc = CSeq c c0) ∨
          (∃b c c0. cc = CIf b c c0) ∨ ∃b c. cc = CWhile b c
   
   [datatype_aexp]  Theorem
      
      ⊢ DATATYPE (aexp ANum APlus AMinus AMult)
   
   [datatype_bexp]  Theorem
      
      ⊢ DATATYPE (bexp BTrue BFalse BEq BNeq BLe BGt BNot BAnd)
   
   [datatype_com]  Theorem
      
      ⊢ DATATYPE (com CSkip CAsgn CSeq CIf CWhile)
   
   
*)
end
