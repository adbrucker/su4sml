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

(** Implementation of the Liskov Substitiution Principle. *)
signature WFCPOG_VISIBILITY_CONSTRAINT =
sig
    val are_conditions_visible : WFCPOG.wfpo -> Rep.Model -> bool

    (** 
     * Checks if the visibility of the class is at least as visible as 
     * as the most visible member.
     *)
    val model_entity_consistency                  : WFCPOG.wfpo -> Rep.Model -> bool
    (**
     * Checks if the modificators of overriden features are maintained in the subclasses.
     *)
    val model_inheritance_consistency                   : WFCPOG.wfpo -> Rep.Model -> bool
    (**
     * Runtime checking/enforcement in mind:
     * pre-condition, post-conditions, invariants are shall only contain
     * calls to visible features (i.e., public features of other classes,
     * package features of other classes within the same package, 
     * protected features of superclasses, and own private features).
     * Checks the pre-,postconditions and invariants with respect to 
     *)
(*
    val constraint_check_by_runtime_consistency   : WFCPOG.wfpo -> Rep.Model -> bool
    (** 
     * Design by contract in mind: 
     * Here, clients (callers) should be able to check/establish the pre-condition of operations:
     * pre-conditions should only contain feature calls that are at least as visible as 
     * the operation itself.
     *)
    val constraint_desing_by_contract_consistency : WFCPOG.wfpo -> Rep.Model -> bool
*)
end
structure WFCPOG_Visibility_Constraint:WFCPOG_VISIBILITY_CONSTRAINT = 
struct

(* su4sml *)
open Rep_Core
open Rep_Logger
open Rep_OclTerm
open Rep_OclType
open Rep2String
open XMI_DataTypes
open Ocl2String
(* oclparser *)
open ModelImport

(* wfcpo-gen *)
open WFCPOG_Library


exception WFCPOG_VisibilityError of string

type Visibility = XMI_DataTypes.VisibilityKind


fun visibility2string public = "public"
  | visibility2string package = "package"
  | visibility2string protected = "protected"
  | visibility2string private = "private"

fun visibility_conforms_to public _ = true
  | visibility_conforms_to package public = false
  | visibility_conforms_to package _ = true
  | visibility_conforms_to protected protected = true
  | visibility_conforms_to protected private = true
  | visibility_conforms_to protected _ = false
  | visibility_conforms_to private private = true
  | visibility_conforms_to private _ = false

fun expr_is_visible_CollPart modif (CollectionItem(term,typ)) model = expr_is_visible modif term model
  | expr_is_visible_CollPart modif (CollectionRange(first,last,typ)) model = 
    (expr_is_visible modif first model) andalso (expr_is_visible modif last model)

and expr_is_visible modif (Literal(s,typ)) model = true 
  | expr_is_visible modif (Variable(s,typ)) model = true
  | expr_is_visible modif (CollectionLiteral(part,typ)) model = 
    List.all (fn a => expr_is_visible_CollPart modif a model) part
  | expr_is_visible modif (If(cond,cond_typ,else_t,else_typ,then_t,then_typ,rtyp)) model = 
    expr_is_visible modif cond model 
    andalso expr_is_visible modif else_t model
    andalso expr_is_visible modif then_t model
  | expr_is_visible modif (AssociationEndCall(src,styp,path,rtyp)) model = 
    let
	val _ = trace 50 ("start expr_is_visible")
	val cl = class_of_term src model
	val att_name = List.last(path)
	val _ = trace 50 ("start get_attribute ")
	val att = get_attribute att_name cl model
	val _ = trace 50 ("end expr_is_visible")
    in
	if (visibility_conforms_to (#visibility att) modif)
	then (expr_is_visible modif src model)
	else false
    end    
  | expr_is_visible modif (x as OperationCall(src,styp,path,args,rtyp)) model = 
    let
	val typ = trace 50 ("expr_is_visible: OpCall \n")
	val typ = type_of_term src
	val cl = class_of_term (Variable("x",typ)) model
	val op_name = List.last(path)
	val _ = trace 50 ("OperationCall: " ^ (Ocl2String.ocl2string false x)  ^ "\n")
	val _ = trace 50 ("Classifier : " ^ Rep2String.classifier2string cl ^ "\n")
	val _ = trace 50 ("Op_name : " ^ op_name ^ "\n")
	val oper = get_operation op_name cl model
	val _ = trace 50 ("end expr_is_visible")
    in
	if (visibility_conforms_to (#visibility oper) modif) 
	then ((List.all (fn (a,b) => (expr_is_visible modif a model)) args) andalso (expr_is_visible modif src model))
	else false
    end
  | expr_is_visible modif (x as AttributeCall(src,styp,path,rtyp)) model =
    let
	val _ = trace 50 ("expr_is_visible: AttCall \n ")
	val cl = class_of_term src model
	val att_name = List.last(path)
	val att = get_attribute att_name cl model
	val _ = trace 100 ("end expr_is_visible")
    in
	if (visibility_conforms_to (#visibility att) modif)
	then (expr_is_visible modif src model)
	else false
    end
  | expr_is_visible modif (OperationWithType(src,styp,name,ntyp,rtyp)) model = expr_is_visible modif src model
  | expr_is_visible modif (Let(var,vtyp,rhs,rtyp,t_in,ityp)) model =
    (expr_is_visible modif rhs model) andalso (expr_is_visible modif t_in model)
  | expr_is_visible modif (Iterate(iter_vars,rname,rtyp,rterm,src,styp,body,btyp,retyp)) model = 
    (expr_is_visible modif src model) andalso (expr_is_visible modif body model)
  | expr_is_visible modif (Iterator(name,vars,src,styp,body,btyp,rtyp)) model = 
    (expr_is_visible modif src model) andalso (expr_is_visible modif body model)


fun are_conditions_visible_help [] model = true
  | are_conditions_visible_help (h::classes) (model as (clist,alist)) = 
    if (not (is_visible_cl h))
    then 
	let
	    val _ = trace 50 ("Classifier " ^ (string_of_path (name_of h)) ^ " is not visible.\n")
	in
	    are_conditions_visible_help classes model
	end
    else
	let
	    val _ = trace 50 ("Classifier " ^ (string_of_path (name_of h)) ^ " is visible.\n")
	    val pub_op = List.map (fn a => 
				      let
					  val _ = trace 50 ("public operation = " ^ (name_of_op a) ^ "\n")
					  val posts = postcondition_of_op a
				      in
					  List.map (fn (x,y) => 
						       let
							   val _ = trace 50 ("next post: \n" )
						       in
							   if (expr_is_visible public y model)
							   then true
							   else raise WFCPOG.WFCPOG_WFC_FailedException ("WFC not hold in class " ^ (string_of_path (name_of h)) ^ " in the condition " ^ (valOf (x)) ^ " with term: " ^ (ocl2string false y) ^ "\n")
						       end
						   ) posts
				      end
				  ) (public_operations_of h model)
	    val _ = trace 50 ("public operations done.\n\n")
	    val pac_op = List.map (fn a => 
				      let
					  val _ = trace 50 ("package operations = " ^ (name_of_op a) ^ "\n")
					  val posts = postcondition_of_op a
				      in
					  List.map (fn (x,y) => 
						       let
							   val _ = trace 50 ("next post: \n" )
						       in
							   if (expr_is_visible public y model)
							   then true
							   else raise WFCPOG.WFCPOG_WFC_FailedException ("WFC not hold in class " ^ (string_of_path (name_of h)) ^ " in the condition " ^ (valOf (x)) ^ " with term: " ^ (ocl2string false y) ^ "\n")
						       end
						   ) posts
				      end
				  ) (package_operations_of h model)
	    val _ = trace 50 ("package operations done.\n\n")
	    val pro_op = List.map (fn a => 
				      let
					  val _ = trace 50 ("protected operations " ^ (name_of_op a) ^ "\n")
					  val posts = postcondition_of_op a
				      in
					  List.map (fn (x,y) => 
						       let
							   val _ = trace 50 ("next post: \n" )
						       in
							   if (expr_is_visible public y model)
							   then true
							   else raise WFCPOG.WFCPOG_WFC_FailedException ("WFC not hold in class " ^ (string_of_path (name_of h)) ^ " in the condition " ^ (valOf (x)) ^ " with term: " ^ (ocl2string false y) ^ "\n")
						       end
						   ) posts
				      end
				  ) (protected_operations_of h model)
	    val _ = trace 50 ("protected operations done.\n\n")
	    val _ = trace 50 ("visibility" ^ (string_of_path (name_of h)) ^ " done.\n")
	in
	    if (List.all (fn a => a) (List.concat (pub_op@pro_op@pac_op)))
	    then are_conditions_visible_help classes model
	    else false
	end

fun are_conditions_visible wfpo (model:Rep.Model as (clist,alist)) =
    let
	val _ = trace function_calls ("WF_visibility_CS.are_conditions_visible\n")
	val classes = removeOclLibrary clist
	val _ = trace 50 ("OclLibrary removed ...\n")
	val res = are_conditions_visible_help classes model
	val _ = trace function_ends ("WF_visibility_CS.are_conditions_visible\n")
    in
	res
    end

fun check_visibility_of_classifier class model = 
    let
	val vis_ops = List.map (fn (a:operation) => ((#visibility a),[a],[],[])) (all_operations_of class model)
	val vis_atts = List.map (fn (a:attribute) => ((#visibility a),[],[a],[])) (all_attributes_of class model)
	val vis_assocs = List.map (fn (a:associationend) => ((#visibility a),[],[],[a])) (all_associationends_of class model)
	val vis_class = visibility_of class
	val _ = 
	    List.map (fn ((a:Visibility),x,y,z) => 
			 if (visibility_conforms_to vis_class a)
			 then ()
			 else 
			     let
				 val s1 = "SYNTAX ERROR: Visibility consistency\n\n"
				 val s2 = "Classifier " ^ (string_of_path (name_of class)) ^ " is not visible enough.\n"
				 val s3 = "Visibility of classifer: " ^ (visibility2string (visibility_of class)) ^ ".\n"
				 val s4 = case List.length(x) of
					      1 => ("Visibility of operation " ^ (name_of_op (List.hd(x))) ^ " : " ^ (visibility2string a))
					    | _ => ""
				 val s5 = case List.length(y) of
					      1 => ("Visibility of attribute " ^ (name_of_att (List.hd(y))) ^ " : " ^ (visibility2string a))
					    | _ => ""
				 val s6 = case List.length(z) of
					      1 => ("Visibility of operation " ^ (name_of_aend (List.hd(z))) ^ " : " ^ (visibility2string a))
					    | _ => ""
			     in
				 raise WFCPOG.WFCPOG_WFC_FailedException (s1^s2^s3^s4^s5^s6)
			     end
		     ) ((vis_ops)@(vis_atts)@(vis_assocs))
    in
	true
    end
    
    
fun check_inheritance_visibility_of_classifier class model =
    let
	(* modified operations *)
        val mod_ops_this = modified_operations_of class model
					  (* TODO: support for 
					   * - modified_attributes_of
					   * - modified_associationends_of
					   *)
	val mod_ops_super_this = List.map (fn oper =>
					      let 
						  val op_name = name_of_op oper
						  val super_class = go_up_hierarchy class (class_has_local_op op_name model) model
						  val super_op = get_operation op_name super_class model
					      in
						  (super_class,super_op,oper)
					      end) mod_ops_this
						  
	val _ = 
	    List.map (fn (super,sop,this_op) =>
			 if (visibility_conforms_to (#visibility this_op) (#visibility sop))
			 then ()
			 else 
			     let
				 val s1 = "SYNTAX ERROR: Visibility inheritance consistency\n\n"
				 val s2 = "Classifier " ^ (string_of_path (name_of class)) ^ " has inconsistent visibility for the overriden operation: " ^ (name_of_op this_op) ^ ".\n"
				 val s3 = "Visibility of the overriden operation : " ^ (visibility2string (#visibility this_op)) ^ ".\n"
				 val s4 = "Visibility of the original operation (located in " ^ (string_of_path (name_of super)) ^ " ) : " ^ (visibility2string (#visibility sop)) ^ ".\n" 
			     in
				 raise WFCPOG.WFCPOG_WFC_FailedException (s1^s2^s3^s4)
			     end
		     ) mod_ops_super_this
	val _ = trace function_ends ("WFCPOG_Visibility_Consistency.check_inheritance_visibility_consistency\n")
    in
	true
    end

					      
fun check_runtime_classifier class model = 
    let
	val ops_pre = List.map (fn a => (a,(precondition_of_op a))) (all_operations_of class model)
	val ops_pre_vis = 
	    List.concat (
	    List.map (fn (a,b) => 
				       let
					   val op_vis = (#visibility a)
				       in
					   List.map (fn (opt_name,expr) => ((name_of_op a),opt_name,(expr_is_visible op_vis expr model))) b
				       end) ops_pre)
	val ops_post = List.map (fn a => (a,(postcondition_of_op a))) (all_operations_of class model)
	val ops_post_vis = 
	    List.concat (
	    List.map (fn (a,b) => 
					let
					    val op_vis = (#visibility a)
					in
					    List.map (fn (opt_name,expr) => ((name_of_op a),opt_name,(expr_is_visible op_vis expr model))) b
					end)  ops_post)
	val invs = all_invariants_of class model
	val invs_vis = List.map (fn (a,b) =>
				    let
					val class_vis = (visibility_of class)
				    in
					(a,expr_is_visible class_vis b model)
				    end) invs
	val check_pres = 
	    List.map (fn (a,b,c) =>
			 if c = true
			 then ()
			 else 
			     let
				 val s1 = "error"
			     in
				 raise WFCPOG.WFCPOG_WFC_FailedException s1
			     end) ops_pre_vis

	val check_post =
	    List.map (fn (a,b,c) =>
			 if c = true
			 then ()
			 else
			     let
				 val s1 = "error"
			     in
				 raise WFCPOG.WFCPOG_WFC_FailedException s1
			     end) ops_post_vis
	val check_invs = 
	    List.map (fn (a,b) =>
			 if b = true
			 then () 
			 else
			     let 
				 val s1 = "error"
			     in
				 raise WFCPOG.WFCPOG_WFC_FailedException s1
			     end) invs_vis
    in
	true
    end


fun model_entity_consistency_help [] model = true
  | model_entity_consistency_help (h::classes) model = 
    let 
	val _ = check_visibility_of_classifier h model
    in
	model_entity_consistency_help classes model
    end

fun model_inheritance_consistency_help [] model = true
  | model_inheritance_consistency_help (h::classes) model = 
    let
	val _ = check_inheritance_visibility_of_classifier h model
    in
	model_inheritance_consistency_help classes model
    end
	
fun constraint_check_by_runtime_consistency_help [] model = true
  | constraint_check_by_runtime_consistency_help (h::classes) model =
    let
	val _ = check_runtime_classifier h model
    in
	constraint_check_by_runtime_consistency_help classes model
    end

fun model_entity_consistency wfc_sel (model as (clist,alist)) =
    let
	val classes = removeOclLibrary (clist)
    in
	model_entity_consistency_help classes model
    end

fun model_inheritance_consistency wfc_sel (model as (clist,alist)) = 
    let
	val classes = removeOclLibrary (clist)
    in
	model_inheritance_consistency_help classes model
    end

fun constraint_check_by_runtime_consistency wfc_sel (model as (clist,alist)) = 
    let
	val classes = removeOclLibrary clist
    in
	constraint_check_by_runtime_consistency_help classes model
    end

(*    
fun constratin_design_by_constract_consistency 
*)
end;
