signature REFINE_CONSTRAINT =
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

    val refine_po            : WFCPOG.wfpo -> Rep.Model -> Rep_OclTerm.OclTerm list 

    exception WFCPO_RefineError of string
    exception ClassGroupError of Rep_Core.Classifier list * string
    exception OpGroupError of Rep_Core.operation list * string
    exception WFCPO_SyntaxError_ClassConsistency of (Rep_OclType.Path * Rep_Core.Classifier list)
    exception WFCPO_SyntaxError_OpConsistency of (Rep_Core.Classifier * Rep_Core.operation list)
    exception WFCPO_SyntaxError_TypeConsistency of (Rep_Core.Classifier * Rep_Core.Classifier * Rep_Core.operation * Rep_Core.operation)

end
structure Refine_Constraint : REFINE_CONSTRAINT =
struct

(* su4sml *)
open Rep_Core
open Rep_OclTerm
open Rep_OclType
open Rep2String

(* ocl-parser *)
open Ext_Library

(* wfcpo-gen *)
open WFCPOG_Library


exception WFCPO_SyntaxError_ClassConsistency of (Path * Classifier list)
exception WFCPO_SyntaxError_OpConsistency of (Classifier * operation list)
exception WFCPO_SyntaxError_TypeConsistency of (Classifier * Classifier * operation * operation)
exception ClassGroupError of Rep_Core.Classifier list * string
exception OpGroupError of Rep_Core.operation list * string
exception WFCPO_RefineError of string

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
		   val _ = trace exce ("\n\n#####################################################################\n")
		   val _ = trace exce ("#####################################################################\n\n")
		   val _ = trace exce ("SYNTAX ERROR: Class consistency \n\n")
		   val _ = trace exce ("The following public classes are not included in the refined class:\n\n") 
		   val _ = trace exce (String.concat (List.map (fn a => (" * " ^ (string_of_path (name_of a)) ^ "\n")) clist))
	       in 
		   raise WFCPO_RefineError ("Please adjust model...\n")
	       end     
    end

fun map_public_ops [] = [[]]
  | map_public_ops ((f,t)::tail) = 
    let
	val _ = trace zero ("MAP_PUBLIC_OPS ... \n")
	val f_ops = List.filter (is_visible_op) (operations_of f)
	val t_ops = List.filter (is_visible_op) (operations_of t)
	val _ = trace zero ("Number of operations of f_class(" ^ (string_of_path (name_of f)) ^ ") = " ^ Int.toString (List.length(f_ops)) ^ "\n")
	val _ = trace zero ("Number of operations of t_class(" ^ (string_of_path (name_of t)) ^ ") = " ^ Int.toString (List.length(t_ops)) ^ "\n")
    in
	[(List.map (fn (a,b) => (f,t,a,b)) (group_op f_ops t_ops
					    handle OpGroupError (oplist,s) => 
	       let
		   val _ = trace exce  ("\n\n#####################################################################\n")
		   val _ = trace exce ("#####################################################################\n\n")
		   val _ = trace exce ("SYNTAX ERROR: Operation consistency \n\n")
		   val _ = trace exce ("FromClass = " ^ (string_of_path (name_of f)) ^ "\n")
		   val _ = trace exce ("ToClass = " ^ (string_of_path (name_of t)) ^ "\n")
		   val _ = trace exce ("The following public operations are not included in the refined classes:\n\n") 
		   val _ = trace exce (String.concat (List.map (fn a => (" * " ^ (operation2string a) ^ "\n")) oplist))
	       in 
		   raise WFCPO_RefineError ("Please adjust model...\n")
	       end     
	 ))]
	@(map_public_ops tail)
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
	val c1 = class_of (new_path) (#1 model)
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
			    raise WFCPO_RefineError ("Please adjust model...\n")
			end
	(* name of the arguments *)
	val _ = trace zero ("map_types_6: " ^ string_of_path (name_of c1) ^ "\n")
	val arg_class_name1 = List.map (fn (a,b) => (name_of (class_of_type b model))) (arguments_of_op (h3))
	val _ = trace zero ("map_types_7: \n")
	val c2 = List.map (fn a => 
			      let
				  val rel_path = substitute_package fP tP a
			      in
				  class_of (rel_path) (#1 model)
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
					     raise WFCPO_RefineError ("Please adjust model...\n")
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

fun check_syntax wfpo (model:Rep.Model as (clist,alist)) = 
    let
	val data = RFM_Data.get wfpo
	val packages = (#rfm_tuples data)
    in
	List.all (fn a => a) (List.map (fn a => check_syntax_help model (#1 a) (#2 a)) packages)
    end



(* page 13 *)
fun operation_term oper class = 
    let
	val pres = List.map (fn (a,b) => b) (precondition_of_op oper)
	val posts = List.map (fn (a,b) => b) (postcondition_of_op oper)
	val con_pres = conjugate_terms pres
	val con_posts = conjugate_terms posts
	val args = List.map (fn (a,b) => Variable(a,b)) (arguments_of_op oper)
	val args_term = conjugate_terms args
	val result = Variable("result",result_of_op oper)
	val pre_term = conjugate_terms [con_pres,Variable("self",type_of class),args_term]
	val post_term = conjugate_terms [con_posts,Variable("self",type_of class),args_term,result]
	val pre_local = OperationCall(pre_term,Boolean,["holOclLib","Boolean","OclLocalValid"],[(Variable("tau",DummyT),DummyT)],Boolean)
	val post_local = OperationCall(post_term,Boolean,["holOclLib","Boolean","OclLocalValid"],[(Variable("tau",DummyT),DummyT)],Boolean)
	val final = conjugate_terms [pre_local,post_local]
    in
	OperationCall(result,result_of_op oper,["holOclLib","Quantor","existence"],[(final,Boolean)],Boolean)
    end
(*
fun lamdba_operation name class model = 
    let
	(* go up hierarchie where operation is implemented *)
	(* (operation of subclass, classifier of super class) *)
	val cl = go_up_hierarchy class (class_has_local_op name model) model
	val oper = get_operation name cl
    in
	operation_term oper cl
    end
*)
(* evaluation to true or exception *)
(*
fun eval_to_true_or_excep name class model  = 
    let
	val S = lambda_operation name class model
	val tau = Variable("tau",DummyT)
	val delta = Variable("delta",DummyT)

    in
	OperationCall(term,DummyT,["holOclLib","Boolean","OclLocalValid"],[],Boolean)
    end
*)
(* valid pre states *)
(* fun valid_pre_states name class model = 
    let
	val 
    in
	
    end
*)
(* abs make trans, conc can also *)
(*
fun abs_to_conc_trans op_abs op_conc model = 
*)
(* conc make trans, abs terminate in R(conc) *)
(*
fun conc_to_abs_trans op_abs op_conc model = 
*)
(* forward simulation *)
(* fun fw_sim op_abs op_conc model = 
    let
	val po1 = abs_to_conc_trans op_abs op_conc model
	val po2 = conc_to_abs_trans op_abs op_conc model
    in
	conjugate_terms [po1,po2]
    end
*)
(*
fun refine_operation fc fop_name tc top_name model = 
    let
	
	val prereq1 = eval_to_true_or_excep fop_name fc model
	val prereq2 = valid_pre_states fop_name fc model
	val po1 = 
	val po2 = 
    in
	
    end

fun refine_classifier fc tc model = 
    let
	val fops_name = List.filter (is_visible_op) (all_operations_of fc)
	val tops_name = List.filter (is_visible_op) (all_operations_of tc)
	val gops = group_op fops tops
    in
	List.map (fn (a,b) => refine_operation fc (name_of_op a) tc (name_of_op b)) (gops)
    end

*)
fun refine_po wfpo (model as (clist,alist)) = []
(*
    let
	val fc = List.filter (is_visible_cl) (class_of_package from model)
	val tc = List.filter (is_visible_cl) (class_of_package to model)
	val gc = group_cl fc tc model
    in	
	[]
	List.map (fn (a,b) => refine_classifier a b model) gc
    end
*)

end;
