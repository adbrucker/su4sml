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


    exception ClassGroupError of Rep_Core.Classifier list * string
    exception OpGroupError of Rep_Core.operation list * string
end
structure WFCPOG_Refine_Constraint : WFCPOG_REFINE_CONSTRAINT =
struct

(* su4sml *)
open Rep_Helper
open Rep_Logger
open Rep_Core
open Rep_OclTerm
open Rep_OclType
open Rep2String
open ModelImport
open Rep_HolOcl_Namespace

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
	(String.concat ["Refine rmf_tuples with args: abstract package = ",(string_of_path (#1 tuple)),", concrete package = ",(string_of_path (#2 tuple)),".\n"])
    end
fun rm x [] = [] 
  | rm x [b] = if (x = b) then [] else [b] 
  | rm x (h::tail) = if (x = h) then (rm x tail) else (h::(rm x tail))
						      
fun group_cl [] [] = []
  | group_cl [] toC = []
  | group_cl FromC [] = raise ClassGroupError (FromC,("Some classes of the abstract package are public where in the concrete not.\n")) 
  | group_cl (h1::t1) list =
    let
	val _ = trace function_calls ("WFCPOG_Refine_Constraint.group_cl \n")
	val _ = trace wgen ("Class: " ^ string_of_path (name_of h1) ^ "\n")
	val x = List.filter (fn a => ((List.last (name_of a)) = (List.last (name_of h1)))) list
	val res = 
	    if (List.length(x) = 0)
	    then raise ClassGroupError ([h1],("Some classes of the abstract package are public where in the concrete not.\n"))
	    else (h1,hd(x))::(group_cl t1 (rm (hd(x)) list))
	val _ = trace function_ends ("WFCPOG_Refine_Constraint.group_cl \n")
    in
	res
    end

fun group_op [] [] = []
  | group_op fromOps [] = raise OpGroupError (fromOps,("Some operations of the abstract class are public where int the concrete not.\n"))
  | group_op [] toOps = []
  | group_op ((h1:operation)::t1) list =
    let
	val _ = trace function_calls ("WFCPOG_Refine_Constraint.group_op \n")
	val x = hd(List.filter (fn a => ((name_of_op a) = (name_of_op h1))) list)
	val res = (h1,x)::(group_op t1 (rm x list))
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
	val res = 
	group_cl abs_c conc_c 
	handle ClassGroupError (clist,s) => 
	       let
		   val s1 = ("SYNTAX ERROR: Class consistency \n\n")
		   val s2 = ("The following public classes are not included in the refined class:\n\n") 
		   val s3 = (String.concat (List.map (fn a => (" * " ^ (string_of_path (name_of a)) ^ "\n")) clist))
	       in 
		   raise WFCPOG.WFCPOG_WFC_FailedException (s1^s2^s3)
	       end
	val _ = trace function_calls ("WFCPOG_Refine_Constraint.map_public_classes\n")
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
	    [(List.map (fn (a,b) => (f,t,a,b)) (group_op f_ops t_ops
						handle OpGroupError (oplist,s) => 
						       let
							   val s1 = ("SYNTAX ERROR: Operation consistency \n\n")
							   val s2 = ("FromClass = " ^ (string_of_path (name_of f)) ^ "\n")
							   val s3 = ("ToClass = " ^ (string_of_path (name_of t)) ^ "\n")
							   val s4 = ("The following public operations are not included in the refined classes:\n\n") 
							   val s5 = (String.concat (List.map (fn a => (" * " ^ (operation2string a) ^ "\n")) oplist))
						       in 
							   raise WFCPOG.WFCPOG_WFC_FailedException (s1^s2^s3^s4^s5)
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
			    raise WFCPOG.WFCPOG_WFC_FailedException (s1^s2^s3^s4^s5^s6)
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
					     raise WFCPOG.WFCPOG_WFC_FailedException (s1^s2^s3^s4^s5^s6^s7)
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
	val packages = (#rfm_tuple data)
	val from = (#1 packages)
	val to = (#2 packages)
	val model_packages = all_packages_of_model model
	val check = if ((member from model_packages) andalso (member to model_packages))
		    then check_syntax_help model from to
		    else
			let
			    val s1 = ("\n\n########################################################\n")
			    val s2 = ("########################################################\n\n")
			    val s3 = ("SYNTAX ERROR: check_syntax\n\n")
			    val s4 = ("No classifier where found with the package name of the abstract or concrete path.\n")
			in
			    raise WFCPOG.WFCPOG_WFC_FailedException (s1^s2^s3^s4)
			end 
	val _ = trace function_ends ("WFCPOG_Refine_Constraint.check_syntax\n")
    in
	check
    end

fun get_holocl_operation var_name oper class model = 
    let
	val _ = trace function_calls ("WFCPOG_Refine_Constraint.get_holocl_operation\n") 
	(** use Rep_Encoder to get operation as HOL-OCL-Term **)
	(* val term = Rep_Encoder. .... *)
	val hol_name = (name_of class)@[(name_of_op oper)]
	val args = List.map (fn (a,b) => (Variable(a,b),b)) (arguments_of_op oper)
	val typ = type_of class
	val predicate = Predicate(Variable(var_name,typ),typ,hol_name,args)
	val _ = trace function_ends ("WFCPOG_Refine_Constraint.get_holocl_operation\n") 
    in
	predicate
    end

fun get_holocl_abstraction_relation  abs_path conc_path class_name =
    let
	val _ = trace function_calls ("WFCPOG_Refine_Constraint.get_holocl_abstraction_relation\n")
	val predicate_name = "R_"^(string_of_path abs_path)^"_"^(string_of_path conc_path)^"_"^class_name
	val dummy_term = Variable("R",DummyT)
	val predicate = Predicate(dummy_term,DummyT,[predicate_name],[])
	val _ = trace function_ends ("WFCPOG_Refine_Constraint.get_holocl_abstraction_relation\n")
    in
	predicate
    end


fun refine_operation abs_oper conc_oper abs_class conc_class model = 
    let
	val _ = trace function_calls ("WFCPOG_Refine_Constraint.refine_classifier\n")
	val R = get_holocl_abstraction_relation (package_of abs_class) (package_of conc_class) (List.last(name_of abs_class))
	val S = get_holocl_operation (name_of_op abs_oper) abs_oper abs_class model
	val T = get_holocl_operation (name_of_op conc_oper) conc_oper conc_class model
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
	val gops = group_op abs_ops conc_ops
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
