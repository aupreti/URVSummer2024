structure hoareSyntaxDefTheory :> hoareSyntaxDefTheory =
struct
  
  val _ = if !Globals.print_thy_loads
    then TextIO.print "Loading hoareSyntaxDefTheory ... "
    else ()
  
  open Type Term Thm
  local open stringTheory in end;
  
  structure TDB = struct
    val thydata = 
      TheoryReader.load_thydata "hoareSyntaxDef"
        (holpathdb.subst_pathvars "/Users/angelaupreti/Desktop/Research/URVSummer2024/hoareSyntaxDefTheory.dat")
    fun find s = Redblackmap.find (thydata,s)
  end
  
  fun op aexp_TY_DEF _ = () val op aexp_TY_DEF = TDB.find "aexp_TY_DEF"
  fun op aexp_case_def _ = ()
  val op aexp_case_def = TDB.find "aexp_case_def"
  fun op aexp_size_def _ = ()
  val op aexp_size_def = TDB.find "aexp_size_def"
  fun op bexp_TY_DEF _ = () val op bexp_TY_DEF = TDB.find "bexp_TY_DEF"
  fun op bexp_case_def _ = ()
  val op bexp_case_def = TDB.find "bexp_case_def"
  fun op bexp_size_def _ = ()
  val op bexp_size_def = TDB.find "bexp_size_def"
  fun op com_TY_DEF _ = () val op com_TY_DEF = TDB.find "com_TY_DEF"
  fun op com_case_def _ = () val op com_case_def = TDB.find "com_case_def"
  fun op com_size_def _ = () val op com_size_def = TDB.find "com_size_def"
  fun op datatype_aexp _ = ()
  val op datatype_aexp = TDB.find "datatype_aexp"
  fun op aexp_11 _ = () val op aexp_11 = TDB.find "aexp_11"
  fun op aexp_distinct _ = ()
  val op aexp_distinct = TDB.find "aexp_distinct"
  fun op aexp_nchotomy _ = ()
  val op aexp_nchotomy = TDB.find "aexp_nchotomy"
  fun op aexp_Axiom _ = () val op aexp_Axiom = TDB.find "aexp_Axiom"
  fun op aexp_induction _ = ()
  val op aexp_induction = TDB.find "aexp_induction"
  fun op aexp_case_cong _ = ()
  val op aexp_case_cong = TDB.find "aexp_case_cong"
  fun op aexp_case_eq _ = () val op aexp_case_eq = TDB.find "aexp_case_eq"
  fun op datatype_bexp _ = ()
  val op datatype_bexp = TDB.find "datatype_bexp"
  fun op bexp_11 _ = () val op bexp_11 = TDB.find "bexp_11"
  fun op bexp_distinct _ = ()
  val op bexp_distinct = TDB.find "bexp_distinct"
  fun op bexp_nchotomy _ = ()
  val op bexp_nchotomy = TDB.find "bexp_nchotomy"
  fun op bexp_Axiom _ = () val op bexp_Axiom = TDB.find "bexp_Axiom"
  fun op bexp_induction _ = ()
  val op bexp_induction = TDB.find "bexp_induction"
  fun op bexp_case_cong _ = ()
  val op bexp_case_cong = TDB.find "bexp_case_cong"
  fun op bexp_case_eq _ = () val op bexp_case_eq = TDB.find "bexp_case_eq"
  fun op datatype_com _ = () val op datatype_com = TDB.find "datatype_com"
  fun op com_11 _ = () val op com_11 = TDB.find "com_11"
  fun op com_distinct _ = () val op com_distinct = TDB.find "com_distinct"
  fun op com_nchotomy _ = () val op com_nchotomy = TDB.find "com_nchotomy"
  fun op com_Axiom _ = () val op com_Axiom = TDB.find "com_Axiom"
  fun op com_induction _ = ()
  val op com_induction = TDB.find "com_induction"
  fun op com_case_cong _ = ()
  val op com_case_cong = TDB.find "com_case_cong"
  fun op com_case_eq _ = () val op com_case_eq = TDB.find "com_case_eq"
  
  
val _ = if !Globals.print_thy_loads then TextIO.print "done\n" else ()
val _ = Theory.load_complete "hoareSyntaxDef"

val hoareSyntaxDef_grammars = valOf (Parse.grammarDB {thyname = "hoareSyntaxDef"})
end
