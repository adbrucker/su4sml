(*****************************************************************************
 * su4sml --- a SML repository for managing (Secure)UML/OCL models
 *             http://projects.brucker.ch/su4sml/
 *                                                                            
 * context_declarations.sml --- 
 * This file is part of su4sml.
 *
 * Copyright (c) 2005-2007, ETH Zurich, Switzerland
 *
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *
 *     * Redistributions in binary form must reproduce the above
 *       copyright notice, this list of conditions and the following
 *       disclaimer in the documentation and/or other materials provided
 *       with the distribution.
 *
 *     * Neither the name of the copyright holders nor the names of its
 *       contributors may be used to endorse or promote products derived
 *       from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 ******************************************************************************)
(* $Id: context_declarations.sml 6727 2007-07-30 08:43:40Z brucker $ *)

signature WFCPOG_REFINE_CONSTRAINT =
sig
    type RFM_args 

    structure WFCPOG_RFM_Data :
	      sig
		  type T = RFM_args
		  val get : WFCPOG.wfpo -> T
		  val put : T -> WFCPOG.wfpo -> WFCPOG.wfpo
		  val map : (T -> T) -> WFCPOG.wfpo -> WFCPOG.wfpo
	      end	 
    val print_refine_args       : RFM_args -> string	      

    val check_syntax            : WFCPOG.wfpo -> Rep.Model -> bool

    val generate_pos            : WFCPOG.wfpo -> Rep.Model -> (Rep_OclType.Path * Rep_OclTerm.OclTerm) list 
end
structure WFCPOG_Refine_Constraint : WFCPOG_REFINE_CONSTRAINT =
struct

(* su4sml *)
open Rep_Helper
open Rep_Core
open Rep_OclTerm
open Rep_OclType
open Rep2String
open ModelImport
open Rep_HolOcl_Namespace
open Rep_HolOcl_Helper
(* wfcpo-gen *)
open WFCPOG_Library

exception ClassGroupError of Rep_Core.Classifier list * string
exception OpGroupError of Rep_Core.operation list * string


type RFM_args = {
     key : int,
     rfm_tuple : (Rep_OclType.Path * Rep_OclType.Path)
}



structure WFCPOG_RFM_Data = WFCPOG_DataFun
		     (struct
		      type T = RFM_args;
		      val empty = ({key=10,rfm_tuple=([]:Path,[]:Path)});
		      fun copy T = T;
		      fun extend T = T;
		      end);

fun print_refine_args (args:RFM_args) = 
    let 
	val tuple = (#rfm_tuple args)
    in
	(String.concat ["Refine rmf_tuple with args: abstract package = ",(string_of_path (#1 tuple)),", concrete package = ",(string_of_path (#2 tuple)),".\n"])
    end
fun rm x [] = [] 
  | rm x [b] = if (x = b) then [] else [b] 
  | rm x (h::tail) = if (x = h) then (rm x tail) else (h::(rm x tail))
						      
fun group_cl [] [] = []
  | group_cl [] toC = []
  | group_cl FromC [] = 
    let
	val s1 = "SYNTAX ERROR: OO Refinement syntax check\n\n"
	val s2 = "Classifiers " ^(String.concat (List.map (fn a => ((string_of_path (name_of a))^", ")) FromC))^" of abstract package have no corresponding classifiers in concrete package.\n"
    in
	raise WFCPOG.WFC_FailedMessage (s1^s2)
    end
  | group_cl (h1::t1) list =
    let
	val _ = trace function_calls ("WFCPOG_Refine_Constraint.group_cl \n")
	val _ = trace wgen ("Class: " ^ string_of_path (name_of h1) ^ "\n")
	val x = List.filter (fn a => ((List.last (name_of a)) = (List.last (name_of h1)))) list
	val res = 
	    case  (List.length(x)) of 
		0 => 
		let
		    val s1 = "SYNTAX ERROR: OO Refinement syntax check\n\n"
		    val s2 = "Classifier " ^ (string_of_path (name_of h1)) ^ " of abstract package has no corresponding classifier in concrete package.\n"
		in
		    raise WFCPOG.WFC_FailedMessage (s1^s2)
		end
	      | 1 => (h1,hd(x))::(group_cl t1 (rm (hd(x)) list))
	      | x => 
		let
		    val s1 = "SYNTAX ERROR: OO Refinement syntax check\n\n"
		    val s2 = "Something extremely strange happened. It seemed that the concrete package has two classifiers with the same name!. \nPLEASE CHECK THE CLASSIFIERS OF THE CONCRETE PACKAGE FOR THE OCCURENCE OF DUPLICATES.\n"
		in
		    raise WFCPOG.WFC_FailedMessage (s1^s2)
		end
	val _ = trace function_ends ("WFCPOG_Refine_Constraint.group_cl \n")
    in
	res
    end

fun group_op class_name [] [] = []
  | group_op class_name fromOps [] = 
    let
	val s1 = "SYNTAX ERROR: OO Refinement syntax check\n\n"
	val s2 = "The abstract classifier "^class_name^" its operations " ^(String.concat (List.map (fn a => ((name_of_op a)^", ")) fromOps))^" have no corresponding operations in the concrete classifier.\n"
    in
	raise WFCPOG.WFC_FailedMessage (s1^s2)
    end
  | group_op class_name [] toOps = []
  | group_op class_name ((h1:operation)::t1) list =
    let
	(* TODO: Check also signature because of the overloaded operations! *)
	val _ = trace function_calls ("WFCPOG_Refine_Constraint.group_op \n")
	val x = (List.filter (fn a => ((name_of_op a) = (name_of_op h1))) list)
	val res = 
	    case  (List.length(x)) of 
		0 => 
		let
		    val s1 = "SYNTAX ERROR: OO Refinement syntax check\n\n"
		    val s2 = "The abstract classifier "^class_name^" its operation " ^(name_of_op h1)^ " has no corresponding operation in the concrete classifier.\n"
		in
		    raise WFCPOG.WFC_FailedMessage (s1^s2)
		end
	      | 1 => (h1,List.hd(x))::(group_op class_name t1 (rm (List.hd(x)) list))
	      | x => 
		let
		    val s1 = "SYNTAX ERROR: OO Refinement syntax check\n\n"
		    val s2 = "Something extremely strange happened. It seemed that the classifier "^class_name^"has two operations with the same name!. \nPLEASE CHECK THE CLASSIFIERS OF THE CONCRETE PACKAGE FOR THE OCCURENCE OF DUPLICATES.\n"
		in
		    raise WFCPOG.WFC_FailedMessage (s1^s2)
		end
	val _ = trace function_ends ("WFCPOG_Refine_Constraint.group_op \n")
    in
	res
    end

(* RETURN: (Classifier * Classifer) list *)
fun map_public_classes fromPath toPath (model as (clist,alist)) = 
    let
	val _ = trace function_calls ("WFCPOG_Refine_Constraint.map_public_classes\n")
	val abs_c = List.filter (is_visible_cl) (List.filter (fn a => if (package_of a = fromPath) then true else false) (clist))
	val _ = trace wgen ("abstract public classes: \n")
	val _ = List.map (fn a => trace wgen (" - " ^ (string_of_path (name_of a))^"\n")) abs_c
	val _ = trace wgen ("map_public_classes 2 \n")
	val _ = trace wgen ("Package " ^ string_of_path (fromPath) ^ " contains " ^ Int.toString (List.length(abs_c)) ^ " public classes.\n")
	val conc_c = List.filter (is_visible_cl) (List.filter (fn a => if (package_of a = toPath) then true else false) (clist))
	val _ = trace wgen ("concrete public classes: \n")
	val _ = List.map (fn a => trace wgen (" - " ^ (string_of_path (name_of a))^"\n")) conc_c
	val _ = trace wgen ("Package " ^ string_of_path (toPath) ^ " contains " ^ Int.toString (List.length(conc_c)) ^ " public classes.\n")
	val _ = trace wgen ("map_public_classes 3 \n")
	val res = group_cl abs_c conc_c
(* 
	handle WFCPOG.WFC_FailedMessage s => 
	       let
		   val s1 = ("SYNTAX ERROR: Class consistency \n\n")
		   val s2 = ("The following public classes are not included in the refined class:\n\n") 
		   val s3 = (String.concat (List.map (fn a => (" * " ^ (string_of_path (name_of a)) ^ "\n")) clist))
	       in 
		   raise WFCPOG.WFC_FailedMessage (s1^s2^s3)
	       end
	val _ = trace function_calls ("WFCPOG_Refine_Constraint.map_public_classes\n")
*)
    in
	res
    end

fun map_public_ops [] = [[]]
  | map_public_ops ((f,t)::tail) = 
    let
	val _ = trace function_calls ("Refine_Constraint.map_public_ops\n")
	val f_ops = List.filter (is_visible_op) (local_operations_of f)
	val t_ops = List.filter (is_visible_op) (local_operations_of t)
	val _ = trace wgen ("Number of operations of f_class(" ^ (string_of_path (name_of f)) ^ ") = " ^ Int.toString (List.length(f_ops)) ^ "\n")
	val _ = trace wgen ("Number of operations of t_class(" ^ (string_of_path (name_of t)) ^ ") = " ^ Int.toString (List.length(t_ops)) ^ "\n")
	val res = 
	    [(List.map (fn (a,b) => (f,t,a,b)) (group_op (List.last (Rep_Core.name_of f))  f_ops t_ops
						handle OpGroupError (oplist,s) => 
						       let
							   val s1 = ("SYNTAX ERROR: Operation consistency \n\n")
							   val s2 = ("FromClass = " ^ (string_of_path (name_of f)) ^ "\n")
							   val s3 = ("ToClass = " ^ (string_of_path (name_of t)) ^ "\n")
							   val s4 = ("The following public operations are not included in the refined classes:\n\n") 
							   val s5 = (String.concat (List.map (fn a => (" * " ^ (operation2string a) ^ "\n")) oplist))
						       in 
							   raise WFCPOG.WFC_FailedMessage (s1^s2^s3^s4^s5)
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
	val _ = trace function_calls ("WFCPOG_Refine_Constrain.map_types\n")
	val _ = trace wgen ("map_types: f_cl = " ^ string_of_path (name_of h1) ^ "\n")
	val _ = trace wgen ("map_types: t_cl = " ^ string_of_path (name_of h2) ^ "\n")
	val _ = trace wgen ("map_types: f_op = " ^ name_of_op h3 ^ "\n")
	val _ = trace wgen ("map_types: t_op = " ^ name_of_op h4 ^ "\n")
	(* classifier of return type *)
	(* TODO: remove next trace lines *)
	val _ = trace wgen ("Length of model: " ^ Int.toString (List.length(#1 model))^ ".\n")
	val ret_fC = class_of_type (#result (h3)) model
	val _ = trace wgen ("map_types 2 \n")
	(* name of classifier of return type *)
	val ret_namefC = name_of ret_fC
	val _ = trace wgen ("map_types_3: " ^ string_of_path (ret_namefC) ^ "\n")
	val _ = trace wgen ("map_types_4: " ^ string_of_path fP ^ "\n")
	(* relative path of return type *)
	val new_path = substitute_package fP tP ret_namefC
	val _ = trace wgen ("map_types_5: name of return type: " ^ string_of_path (ret_namefC) ^ "\n")
	val c1 = class_of (new_path) (model)
		 handle _ => 
			let
			    val s1 = ("\n\n#####################################################################\n")
			    val s2 = ("#####################################################################\n\n")
			    val s3 = ("SYNTAX ERROR: Map types \n\n")
			    val s4 = ("The return type of the operation " ^ (operation2string h3) ^ " is inconsistent.\n")
			    val s4 = ("The refining package has no corresponding class.\n")
			    val s5 = ("Existing FromClass = " ^ (string_of_path (name_of h1)) ^ "\n")
			    val s6 = ("Inexisting ToClass = " ^ (string_of_path (name_of h2)) ^ "\n")
			in 
			    raise WFCPOG.WFC_FailedMessage (s1^s2^s3^s4^s5^s6)
			end
	(* name of the arguments *)
	val _ = trace wgen ("map_types_6: " ^ string_of_path (name_of c1) ^ "\n")
	val arg_class_name1 = List.map (fn (a,b) => (name_of (class_of_type b model))) (arguments_of_op (h3))
	val _ = trace wgen ("map_types_7: \n")
	val c2 = List.map (fn a => 
			      let
				  val rel_path = substitute_package fP tP a
			      in
				  class_of (rel_path) (model)
				  handle _ => 
					 let
					     val s1 = ("\n\n########################################################\n")
					     val s2 = ("########################################################\n\n")
					     val s3 = ("SYNTAX ERROR: Map types \n\n")
					     val s4 = ("One of the arguments type of the operation " ^ (operation2string h3) ^ " is inconsistent.\n")
					     val s5 = ("The refining package has no corresponding class.\n")
					     val s6 = ("Existing FromClass = " ^ (string_of_path (name_of h1)) ^ "\n")
					     val s7 = ("Inexisting ToClass = " ^ (string_of_path (name_of h2)) ^ "\n")
					 in 
					     raise WFCPOG.WFC_FailedMessage (s1^s2^s3^s4^s5^s6^s7)
					 end
			      end
			  ) arg_class_name1
	val _ = trace wgen ("map_types_8: \n")
	val res = (true)::(map_types tail fP tP model)
	val _ = trace function_ends ("WFCPOG_Refine_Constrain.map_types\n")
    in 
	res
    end

fun check_syntax_help (model:Rep.Model as (clist,alist)) fromPath toPath = 
    let
	val _ = trace function_calls ("WFCPOG_Refine_Constraint.check_syntax_help\n")
	(* check public classes of the two packages *)
	val x = map_public_classes fromPath toPath model
        val _ = trace wgen ("check syntax 2 \n")
	(* check public methods of the public classes *)
	val y = List.concat (map_public_ops x)
	val _ = trace wgen ("check syntax 3 \n")
	(* check types of the public operations of public classes *)
	val z = map_types y fromPath toPath model
	val _ = trace wgen ("check syntax 4 \n")
	val res = List.all (fn a => a) z
	val _ = trace function_ends ("WFCPOG_Refine_Constraint.check_syntax_help\n")
    in
	res
    end



fun check_syntax wfpo (model:Rep.Model as (clist,alist)) = 
    let
	val _ = trace function_calls ("WFCPOG_Refine_Constraint.check_syntax\n")
	val data = WFCPOG_RFM_Data.get wfpo
	val _ = trace wgen (print_refine_args data)
	val packages = (#rfm_tuple data)
	val from = (#1 packages)
	val to = (#2 packages)
	val model_packages = all_packages_of_model model
	val _ = trace wgen ("Packages of model: "^(String.concat (List.map (fn a => ((string_of_path a)^", ")) model_packages))^".\n")
	val _ = trace wgen ("Abstract Package = "^(string_of_path from)^", contains "^(Int.toString (List.length(from)))^" strings.\n")
	val _ = trace wgen ("Concrete Package = "^(string_of_path to)^", contains "^(Int.toString(List.length(from)))^" strings.\n")
	val check = if ((member from model_packages) andalso (member to model_packages))
		    then check_syntax_help model from to
			 handle WFCPOG.WFC_FailedMessage s => raise WFCPOG.WFC_FailedException (wfpo,s)
		    else
			let
			    val s1 = ("\n\n########################################################\n")
			    val s2 = ("########################################################\n\n")
			    val s3 = ("SYNTAX ERROR: check_syntax\n\n")
			    val s4 = ("No classifier where found with the package name of the abstract or concrete path.\n")
			in
			    raise WFCPOG.WFC_FailedException (wfpo,s1^s2^s3^s4)
			end 
	val _ = trace function_ends ("WFCPOG_Refine_Constraint.check_syntax\n")
    in
	check
    end

(* 
fun refine_operation abs_oper conc_oper abs_class conc_class model = 
    let
	val _ = trace function_calls ("WFCPOG_Refine_Constraint.refine_classifier\n")
	val state = Variable("mystate",OclState)
 	val op_term = get_holocl_operation ("abs") abs_oper abs_class model
	val refine = OperationCall (op_term,DummyT,["holOclLib","Boolean","OclLocalValid"],[(state,OclState)],Boolean)
	val _ = trace function_ends ("WFCPOG_Refine_Constraint.refine_classifier\n")	
    in
	(["po_refine_"^(string_of_path (name_of abs_class))^"_"^(string_of_path (name_of conc_class))^"_"^(name_of_op abs_oper)],refine)
    end
*)

fun refine_operation abs_oper conc_oper abs_class conc_class model = 
    let
	val _ = trace function_calls ("WFCPOG_Refine_Constraint.refine_classifier\n")
 	val R = get_holocl_abstraction_relation abs_class conc_class model
 	val S = get_holocl_operation ("abs") abs_oper abs_class model
	val T = get_holocl_operation ("conc") conc_oper conc_class model
	val refine = OperationCall(S,DummyT,["holOclLib","methodology","refinement","OclForwardRefinement"],[(T,DummyT),(R,DummyT)],Boolean)
	val _ = trace function_ends ("WFCPOG_Refine_Constraint.refine_classifier\n")	
    in
	(["po_refine_"^(string_of_path (name_of abs_class))^"_"^(string_of_path (name_of conc_class))^"_"^(name_of_op abs_oper)],refine)
    end

fun refine_classifier abs_class conc_class model = 
    let
	val _ = trace function_calls ("WFCPOG_Refine_Constraint.refine_classifier\n")
	val abs_ops = List.filter (is_visible_op) (local_operations_of abs_class)
	val conc_ops = List.filter (is_visible_op) (local_operations_of conc_class)
	val gops = group_op (List.last (Rep_Core.name_of abs_class)) abs_ops conc_ops
	val res = List.map (fn (a,b) => refine_operation a b abs_class conc_class model) (gops)
	val _ = trace function_ends ("WFCPOG_Refine_Constraint.refine_classifier\n")
    in
	res
    end

fun refine_package abs_path conc_path (model as (clist,alist)) =
    let
	val _ = trace function_calls ("WFCPOG_Refine_Constraint.refine_package\n")
	val abs_classes = List.filter (fn a => (package_of a = abs_path) andalso (is_visible_cl a)) (clist)
	val conc_classes = List.filter (fn a => (package_of a = conc_path) andalso (is_visible_cl a)) (clist)
	val cl_grouped = group_cl abs_classes conc_classes
	val res = (List.concat (List.map (fn (a,b) => refine_classifier a b model) cl_grouped))
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
	val rfm_args = WFCPOG_RFM_Data.get wfpo
	val packages = (#rfm_tuple rfm_args)
	val _ = trace wgen("Args extracted ...\n")
	val res = refine_package (#1 packages) (#2 packages) model
	val _ = trace function_ends ("WFCPOG_Refine_Constraint.generate_pos\n")
    in	
	res
    end
end;
