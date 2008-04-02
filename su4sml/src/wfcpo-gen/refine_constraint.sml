signature WFCPOG_REFINE_CONSTRAINT =
sig
    type RFM_args 

    structure RFM_Data :
	      sig
		  type T = RFM_args
		  val get : WFCPOG.wfpo -> T
		  val put : T -> WFCPOG.wfpo -> WFCPOG.wfpo
		  val map : (T -> T) -> WFCPOG.wfpo -> WFCPOG.wfpo
	      end	 

    val check_syntax         : WFCPOG.wfpo -> Rep.Model -> bool

    val generate_pos            : WFCPOG.wfpo -> Rep.Model -> Rep_OclTerm.OclTerm list 

    exception WFCPOG_RefineError of string
    exception ClassGroupError of Rep_Core.Classifier list * string
    exception OpGroupError of Rep_Core.operation list * string
    exception WFCPO_SyntaxError_ClassConsistency of (Rep_OclType.Path * Rep_Core.Classifier list)
    exception WFCPO_SyntaxError_OpConsistency of (Rep_Core.Classifier * Rep_Core.operation list)
    exception WFCPO_SyntaxError_TypeConsistency of (Rep_Core.Classifier * Rep_Core.Classifier * Rep_Core.operation * Rep_Core.operation)

end
structure WFCPOG_Refine_Constraint : WFCPOG_REFINE_CONSTRAINT =
struct

(* su4sml *)
open Rep_Help_Functions
open Rep_Logger
open Rep_Core
open Rep_OclTerm
open Rep_OclType
open Rep2String
open ModelImport
open HolOcl_Namespace

(* wfcpo-gen *)
open WFCPOG_Library


exception WFCPO_SyntaxError_ClassConsistency of (Path * Classifier list)
exception WFCPO_SyntaxError_OpConsistency of (Classifier * operation list)
exception WFCPO_SyntaxError_TypeConsistency of (Classifier * Classifier * operation * operation)
exception ClassGroupError of Rep_Core.Classifier list * string
exception OpGroupError of Rep_Core.operation list * string
exception WFCPOG_RefineError of string

type RFM_args = {
     key : int,
     rfm_tuples : (Rep_OclType.Path * Rep_OclType.Path) list
}



structure RFM_Data = WfpoDataFun
		     (struct
		      type T = RFM_args;
		      val empty = ({key=10,rfm_tuples=[([]:Path,[]:Path)]});
		      fun copy T = T;
		      fun extend T = T;
		      end);

fun rm x [] = [] 
  | rm x [b] = if (x = b) then [] else [b] 
  | rm x (h::tail) = if (x = h) then (rm x tail) else (h::(rm x tail))
						      
fun group_cl [] [] = []
  | group_cl [] toC = []
  | group_cl FromC [] = raise ClassGroupError (FromC,("Some classes of the abstract package are public where in the concrete not.\n")) 
  | group_cl (h1::t1) list =
    let
	val _ = trace zero ("Class: " ^ string_of_path (name_of h1) ^ "\n")
	val x = List.filter (fn a => ((List.last (name_of a)) = (List.last (name_of h1)))) list
	(* val _ = trace high ("fromClass = " ^ (string_of_path (name_of h1)) ^ ", toClass = " ^ (string_of_path (name_of (hd(x)))) ^ "\n") *)
    in
	if (List.length(x) = 0)
	then raise ClassGroupError ([h1],("Some classes of the abstract package are public where in the concrete not.\n"))
	else (h1,hd(x))::(group_cl t1 (rm (hd(x)) list))
    end

fun group_op [] [] = []
  | group_op fromOps [] = raise OpGroupError (fromOps,("Some operations of the abstract class are public where int the concrete not.\n"))
  | group_op [] toOps = []
  | group_op ((h1:operation)::t1) list =
    let
	val x = hd(List.filter (fn a => ((name_of_op a) = (name_of_op h1))) list)
    in
	(h1,x)::(group_op t1 (rm x list))
    end

(* RETURN: (Classifier * Classifer) list *)
fun map_public_classes fromPath toPath (model as (clist,alist)) = 
    let
	val _ = trace zero ("MAP_PUBLIC_CLASSES ...\n")
	val abs_c = List.filter (is_visible_cl) (List.filter (fn a => if (package_of a = fromPath) then true else false) (clist))
	val _ = printList abs_c
	val _ = trace zero ("map_public_classes 2 \n")
	val _ = trace zero ("Package " ^ string_of_path (fromPath) ^ " contains " ^ Int.toString (List.length(abs_c)) ^ " classes.\n")
	val conc_c = List.filter (is_visible_cl) (List.filter (fn a => if (package_of a = toPath) then true else false) (clist))
	val _ = printList conc_c
	val _ = trace zero ("Package " ^ string_of_path (toPath) ^ " contains " ^ Int.toString (List.length(conc_c)) ^ " classes.\n")
	val _ = trace zero ("map_public_classes 3 \n")
    in
	group_cl abs_c conc_c 
	handle ClassGroupError (clist,s) => 
	       let
		   val s1 = ("SYNTAX ERROR: Class consistency \n\n")
		   val s2 = ("The following public classes are not included in the refined class:\n\n") 
		   val s3 = (String.concat (List.map (fn a => (" * " ^ (string_of_path (name_of a)) ^ "\n")) clist))
		   val _ = trace exce (s1^s2^s3)
	       in 
		   raise WFCPOG_RefineError ("Please adjust model...\n")
	       end     
    end

fun map_public_ops [] = [[]]
  | map_public_ops ((f,t)::tail) = 
    let
	val _ = trace function_calls ("Refine_Constraint.map_public_ops\n")
	val f_ops = List.filter (is_visible_op) (operations_of f)
	val t_ops = List.filter (is_visible_op) (operations_of t)
	val _ = trace zero ("Number of operations of f_class(" ^ (string_of_path (name_of f)) ^ ") = " ^ Int.toString (List.length(f_ops)) ^ "\n")
	val _ = trace zero ("Number of operations of t_class(" ^ (string_of_path (name_of t)) ^ ") = " ^ Int.toString (List.length(t_ops)) ^ "\n")
	val res = 
	    [(List.map (fn (a,b) => (f,t,a,b)) (group_op f_ops t_ops
						handle OpGroupError (oplist,s) => 
						       let
							   val s1 = ("SYNTAX ERROR: Operation consistency \n\n")
							   val s2 = ("FromClass = " ^ (string_of_path (name_of f)) ^ "\n")
							   val s3 = ("ToClass = " ^ (string_of_path (name_of t)) ^ "\n")
							   val s4 = ("The following public operations are not included in the refined classes:\n\n") 
							   val s5 = (String.concat (List.map (fn a => (" * " ^ (operation2string a) ^ "\n")) oplist))
							   val _ = trace exce (s1^s2^s3^s4^s5)
						       in 
							   raise WFCPOG_RefineError ("Please adjust model...\n")
						       end     
	    ))]
	    @(map_public_ops tail)
	val _ = trace function_ends ("Refine_Constraint.map_public_op\n")
    in
	res 
    end
    
    
    
fun map_types [] fP tP model = []
  | map_types ((h1:Classifier,h2:Classifier,h3:operation,h4:operation)::tail) fP tP model =
    let
	val _ = trace zero ("MAP_TYPES ... \n")
	val _ = trace zero ("map_types: f_cl = " ^ string_of_path (name_of h1) ^ "\n")
	val _ = trace zero ("map_types: f_cl = " ^ string_of_path (name_of h2) ^ "\n")
	val _ = trace zero ("map_types: f_op = " ^ name_of_op h3 ^ "\n")
	val _ = trace zero ("map_types: t_op = " ^ name_of_op h4 ^ "\n")
	(* classifier of return type *)
	val ret_fC = class_of_type (#result (h3)) model
	val _ = trace zero ("map_types 2 \n")
	(* name of classifier of return type *)
	val ret_namefC = name_of ret_fC
	val _ = trace zero ("map_types_3: " ^ string_of_path (ret_namefC) ^ "\n")
	val _ = trace zero ("map_types_4: " ^ string_of_path fP ^ "\n")
	(* relative path of return type *)
	val new_path = substitute_package fP tP ret_namefC
	val _ = trace zero ("map_types_5: name of return type: " ^ string_of_path (ret_namefC) ^ "\n")
	val c1 = class_of (new_path) (model)
		 handle _ => 
			let
			    val _ = trace exce  ("\n\n#####################################################################\n")
			    val _ = trace exce ("#####################################################################\n\n")
			    val _ = trace exce ("SYNTAX ERROR: Map types \n\n")

			    val _ = trace exce ("The return type of the operation " ^ (operation2string h3) ^ " is inconsistent.\n")
			    val _ = trace exce ("The refining package has no corresponding class.\n")
			    val _ = trace exce ("Existing FromClass = " ^ (string_of_path (name_of h1)) ^ "\n")
			    val _ = trace exce ("Inexisting ToClass = " ^ (string_of_path (name_of h2)) ^ "\n")
			in 
			    raise WFCPOG_RefineError ("Please adjust model...\n")
			end
	(* name of the arguments *)
	val _ = trace zero ("map_types_6: " ^ string_of_path (name_of c1) ^ "\n")
	val arg_class_name1 = List.map (fn (a,b) => (name_of (class_of_type b model))) (arguments_of_op (h3))
	val _ = trace zero ("map_types_7: \n")
	val c2 = List.map (fn a => 
			      let
				  val rel_path = substitute_package fP tP a
			      in
				  class_of (rel_path) (model)
				  handle _ => 
					 let
					     val _ = trace exce  ("\n\n#####################################################################\n")
					     val _ = trace exce ("#####################################################################\n\n")
					     val _ = trace exce ("SYNTAX ERROR: Map types \n\n")
						     
					     val _ = trace exce ("One of the arguments type of the operation " ^ (operation2string h3) ^ " is inconsistent.\n")
					     val _ = trace exce ("The refining package has no corresponding class.\n")
					     val _ = trace exce ("Existing FromClass = " ^ (string_of_path (name_of h1)) ^ "\n")
					     val _ = trace exce ("Inexisting ToClass = " ^ (string_of_path (name_of h2)) ^ "\n")
					 in 
					     raise WFCPOG_RefineError ("Please adjust model...\n")
					 end
			      end
			  ) arg_class_name1
	val _ = trace zero ("map_types_8: \n")
    in
	(true)::(map_types tail fP tP model)
    end

fun check_syntax_help (model:Rep.Model as (clist,alist)) fromPath toPath = 
    let
	val _ = trace zero ("CHECK SYNTAX ... \n")
	(* check public classes of the two packages *)
	val x = map_public_classes fromPath toPath model
        val _ = trace zero ("check syntax 2 \n")
	(* check public methods of the public classes *)
	val y = List.concat (map_public_ops x)
	val _ = trace zero ("check syntax 3 \n")
	(* check types of the public operations of public classes *)
	val z = map_types y fromPath toPath model
	val _ = trace zero ("check syntax 4 \n")
    in 
	List.all (fn a => a) z
    end

fun check_syntax' abs_path conc_path model = 
    let
	val _ = trace function_calls ("WFCPOG_Refine_Constraint.check_syntax'\n")
	val model_packages = all_packages_of_model model
	val res = 
	    if (member abs_path model_packages)
	    then check_syntax_help model abs_path conc_path
	    else raise WFCPOG_RefineError ("This specific constraint is not applicable for this model.\n")
	val _ = trace function_calls ("WFCPOG_Refine_Constraint.check_syntax'\n")
    in
	res
    end

fun check_syntax wfpo (model:Rep.Model as (clist,alist)) = 
    let
	val data = RFM_Data.get wfpo
	val packages = (#rfm_tuples data)
	val abstract_packages = List.map (fn (a,b) => a) packages
	val model_packages = all_packages_of_model model
    in
	if (List.all (fn a => member a model_packages) abstract_packages)
	then List.all (fn a => a) (List.map (fn a => check_syntax_help model (#1 a) (#2 a)) packages)
	else raise WFCPOG_RefineError ("This specific constraint is not applicable for this model.\n")
    end



(* TODO: *)
fun get_holocl_operation oper class model = 
    let
	val _ = trace function_calls ("WFCPOG_Refine_Constraint.get_holocl_operation\n") 
	(** use Rep_Encoder to get operation as HOL-OCL-Term **)
	(* val term = Rep_Encoder. .... *)
	val hol_name = mk_def_of (name_of class)@[(name_of_op oper)]
	val args = List.map (fn (a,b) => (Variable(a,b),b)) (arguments_of_op oper)
	val typ = type_of class
	val res = Predicate(Variable("x",typ),typ,hol_name,args)
	val _ = trace function_ends ("WFCPOG_Refine_Constraint.get_holocl_operation\n") 
    in
	res
    end

fun get_holocl_abstraction_relation abs_package conc_package model = 
    let
	val _ = trace function_calls ("WFCPOG_Refine_Constraint.get_holocl_abstraction_relation\n")
	val res = Predicate(Variable("x",DummyT),DummyT,["AbstractionRelation"],[])
	val _ = trace function_ends ("WFCPOG_Refine_Constraint.get_holocl_abstraction_relation\n")
    in
	res
    end

fun get_holocl_invariant class model = 
    let
	val _ = trace function_calls ("WFCPOG_Refine_Constraint.get_holocl_invariant\n")
	val hol_name = mk_def_of (name_of class)
	val typ = type_of class
	val res = Predicate(Variable("x",typ),typ,hol_name,[])
	val _ = trace function_ends ("WFCPOG_Refine_Constraint.get_holocl_invariant\n")
    in
	res
    end

fun M_valid (hol_operation:OclTerm) ((Tuple(tuple)):OclTerm) = 
    let
	val _ = trace function_calls ("WFCPOG_Refine_Constraint.M_valid\n")
	(* evaluation to true *)
	val t = OperationCall(hol_operation,Boolean,["holOclLib","Boolean","OclLocalValid"],[(Tuple(tuple),DummyT)],Boolean)
	
        (* evaluation to exception *)
	val undef = OperationCall(hol_operation,DummyT,["oclLib","OclAny","oclIsUndefined"],[],Boolean)
	val e = OperationCall(undef,Boolean,["holOclLib","Boolean","OclLocalValid"],[(Tuple(tuple),DummyT)],Boolean)
	val res = disjugate_terms [t,e]
	val _ = trace function_ends ("WFCPOG_Refine_Constraint.M_valid\n")
    in
	res
    end

(* valid pre states (in paper (55)) *)
fun pre_state_valid hol_operation ((Tuple(tuple)):OclTerm) V =
    let
	val _ = trace function_calls ("WFCPOG_Refine_Constraint.valid_pre_states\n")
	(* (string * term * type) *)
	val (sigma_name,sigma_term,sigma_type) = List.hd tuple
        (* (string * term * type) *)
	val (sigma'_name,sigma'_term,sigma'_type) = List.last tuple
	(* sigma is_element_of V *)
(*	val s_E_V = Predicate(V,DummyT, *)
	val m_valid = M_valid hol_operation (Tuple(tuple))
	val exists = Iterator("holOclLib.exists",[(sigma'_name,sigma'_type)],V,Boolean,m_valid,Boolean,Boolean)
	val res = Iterator("holOclLib.forAll",[(sigma_name,sigma_type)],V,Boolean,exists,Boolean,Boolean)
	val _ = trace function_ends ("WFCPOG_Refine_Constraint.valid_pre_states\n")
    in
	res
    end

(* in paper (56) *)
fun generate_po1 abs_tuple conc_tuple S T V R = Variable("dummy_term",DummyT)
(*    let
	val _ = trace function_calls ("WFCPOG_Refine_Constraint.generate_po1\n")
	val pre_S = pre_state_valid S abs_tuple
	val pre_T = pre_state_valid T conc_tuple
		
	val m_valid_T = M_valid T conc_tuple
	val m_valid_S = M_valid S abs_tuple
	val 


	val _ = trace function_ends ("WFCPOG_Refine_Constraint.generate_po1\n")
    in
	res
    end
*)
(* in paper (57) *)
fun generate_po2 abs_tuple conc_tuple S T V R = Variable("dummy_term",DummyT)
(*    let
	val x = ...
    in
	
    end
*)
fun refine_operation abs_oper conc_oper abs_class conc_class V R model = 
    let
	val _ = trace function_calls ("WFCPOG_Refine_Constraint.refine_classifier\n")
	val S = get_holocl_operation abs_oper abs_class model
	val T = get_holocl_operation conc_oper conc_class model
	val abs_tuple = Tuple[("sigma_a",Variable("sigma_a",DummyT),DummyT),("sigma_a'",Variable("sigma_a'",DummyT),DummyT)]
	val conc_tuple = Tuple[("sigma_c",Variable("sigma_c",DummyT),DummyT),("sigma_c'",Variable("sigma_c'",DummyT),DummyT)]
	val po1 = generate_po1 abs_tuple conc_tuple S T V R
	val po2 = generate_po2 abs_tuple conc_tuple S T V R
	val res = conjugate_terms [po1,po2]
	val _ = trace function_ends ("WFCPOG_Refine_Constraint.refine_classifier\n")	
    in
	res
    end

fun refine_classifier abs_class conc_class R model = 
    let
	val _ = trace function_calls ("WFCPOG_Refine_Constraint.refine_classifier\n")
        (* all valid states of class *)
	val V = get_holocl_invariant abs_class model
	val abs_ops = List.filter (is_visible_op) (all_operations_of abs_class model)
	val conc_ops = List.filter (is_visible_op) (all_operations_of conc_class model)
	val gops = group_op abs_ops conc_ops
    in
	List.map (fn (a,b) => refine_operation a b abs_class conc_class V R model) (gops)
    end

fun refine_package abs_path conc_path (model as (clist,alist)) =
    let
	val _ = trace function_calls ("WFCPOG_Refine_Constraint.refine_package\n")
	val R = get_holocl_abstraction_relation abs_path conc_path model
	val _ = trace function_ends ("WFCPOG_Refine_Constraint.refine_package\n")
	val syntax_check = check_syntax' abs_path conc_path model
	val abs_classes = List.filter (fn a => (package_of a = abs_path) andalso (is_visible_cl a)) (clist)
	val conc_classes = List.filter (fn a => (package_of a = conc_path) andalso (is_visible_cl a)) (clist)
	val cl_grouped = group_cl abs_classes conc_classes
	val res = 
	    if syntax_check = true 
	    then (List.concat (List.map (fn (a,b) => refine_classifier a b R model) cl_grouped))
	    else raise WFCPOG_RefineError ("Something went wrong.\n")
	val _ = trace function_ends ("WFCPOG_Refine_Constraint.refine_package\n")
    in
	res
    end

fun generate_pos wfpo (model as (clist,alist)) =
    let
	val _ = trace function_calls ("WFCPOG_Refine_Constraint.generate_pos\n")
	val _ = trace wgen ("remove OclLib ...\n")
	val classes = removeOclLibrary clist
	val _ = trace wgen ("oclLib removed ...\n")
	val _ = trace wgen ("Extract args ...\n")
	val rfm_args = RFM_Data.get wfpo
	val to_refine_packages = (#rfm_tuples rfm_args)
	val _ = trace wgen("Args extracted ...\n")
	val res = List.concat (List.map (fn (a,b) => refine_package a b model) to_refine_packages)
	val _ = trace function_ends ("WFCPOG_Refine_Constraint.generate_pos\n")
    in	
	res
    end
end;
