signature WFCPO_REGISTRY = 
sig

datatype constraint = 
	 Liskov of 
	 { 
	  (** subconstraint *)
	  weaken_pre          : bool,
	  (** subconstraint *)
	  strengthen_post     : bool,
	  (** subconstraint *)
	  conjugate_inv       : bool 
	 }
       | DataModel of 
	 {
	  (** subconstraint *)
	  class_model_con    : bool,
	  (** subconstraint *)
	  strong_model_con   : bool
	 }
       | Refine of 
	 {
	  (** argument *)
	   abs_rel : string list list 
	 } 

    val generate_po                 : constraint -> Rep.Model -> Rep_OclTerm.OclTerm list
    val analyze_model               : constraint list -> Rep.Model -> Rep_OclTerm.OclTerm list
end;
structure WFCPO_Registry : WFCPO_REGISTRY = 
struct 


datatype constraint = 
	 Liskov of 
	 { 
	  weaken_pre          : bool,
	  strengthen_post    : bool,
	  conjugate_inv      : bool 
	 }
       | DataModel of 
	 {
	  class_model_con    : bool,
	  strong_model_con   : bool
	 }
       | Refine of 
	 {
	  abs_rel : string list list 
	 } 


fun generate_po (Liskov{weaken_pre,strengthen_post,conjugate_inv}) model = 
    let
	val wp = if (weaken_pre)
		 then Liskov_Constraint.weaken_precondition model
		 else []
	val sp = if (strengthen_post)
		 then Liskov_Constraint.strengthen_postcondition model
		 else []
	val ci = if (conjugate_inv)
		 then Liskov_Constraint.conjugate_invariants model
		 else []
    in
	wp@sp@ci
    end
  | generate_po (DataModel{class_model_con,strong_model_con}) model =
    let
	val cmc = if (class_model_con)
		  then Data_Model_Consistency_Constraint.class_model_consistency model
		  else []
	val smc = if (strong_model_con)
		  then Data_Model_Consistency_Constraint.strong_model_consistency model
		  else []
    in
	cmc@smc
    end
  | generate_po (Refine {abs_rel}) model = []

fun analyze_model [] model = []
  | analyze_model [x:constraint] model = generate_po x model
  | analyze_model (con_list) model = List.concat (List.map (fn a => generate_po a model) con_list)

end
