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
    (** 
     * Checks if the visibility of the class is at least as visible
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
     *)
    val constraint_check_by_runtime_consistency   : WFCPOG.wfpo -> Rep.Model -> bool
    (** 
     * Design by contract in mind: 
     * Here, clients (callers) should be able to check/establish the pre-condition of operations:
     * pre-conditions should only contain feature calls that are at least as visible as 
     * the operation itself.
     *)
    val constraint_design_by_contract_consistency : WFCPOG.wfpo -> Rep.Model -> bool

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
	val _ = trace wgen ("start expr_is_visible")
	val cl = class_of_term src model
	val att_name = List.last(path)
	val _ = trace wgen ("start get_attribute ")
	val att = get_attribute att_name cl model
	val _ = trace wgen ("end expr_is_visible")
    in
	if (visibility_conforms_to (#visibility att) modif)
	then (expr_is_visible modif src model)
	else false
    end    
  | expr_is_visible modif (x as OperationCall(src,styp,path,args,rtyp)) model = 
    let
	val _ = trace function_calls ("WFCPGO_Visibility_Constraint.expr_is_visible : " ^ (ocl2string false x) ^ "\n")
	val typ = type_of_term src
	val cl = class_of_term (Variable("x",typ)) model
	val op_name = List.last(path)
	val _ = trace 50 ("Classifier : " ^ (string_of_path (name_of cl)) ^ "\n")
	val _ = trace 50 ("Op_name : " ^ op_name ^ "\n")
	val oper = get_operation op_name cl model
	val _ = trace wgen ("got operation\n")
	val res = 
	    if (visibility_conforms_to (#visibility oper) modif) 
	    then ((List.all (fn (a,b) => (expr_is_visible modif a model)) args) andalso (expr_is_visible modif src model))
	    else false
	val _ = trace function_ends ("WFCPGO_Visibility_Constraint.expr_is_visible\n")
    in
	res 
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


fun check_entity_classifier class model = 
    let
	val vis_ops = List.map (fn (a:operation) => ((#visibility a),SOME(a),NONE,NONE)) (all_operations_of class model)
	val vis_atts = List.map (fn (a:attribute) => ((#visibility a),NONE,SOME(a),NONE)) (all_attributes_of class model)
        (* TODO: AssociationEnd support *)
(*	val vis_assocs = List.map (fn (a:associationend) => ((#visibility a),NONE,NONE,SOME(a))) (associationends_of class) *)
	val vis_class = visibility_of class
	val check = 
	    List.map (fn ((a:Visibility),x,y,z) => 
			 if (visibility_conforms_to vis_class a)
			 then true
			 else 
			     let
				 val s1 = "SYNTAX ERROR: Visibility consistency\n\n"
				 val s2 = "Classifier " ^ (string_of_path (name_of class)) ^ " is not visible enough.\n"
				 val s3 = "Visibility of classifer: " ^ (visibility2string (visibility_of class)) ^ ".\n"
				 val s4 = case x of
					      SOME(oper) => ("Visibility of operation " ^ (name_of_op oper) ^ " : " ^ (visibility2string a))
					    | _ => ""
				 val s5 = case y of
					      SOME(att) => ("Visibility of attribute " ^ (name_of_att att) ^ " : " ^ (visibility2string a))
					    | _ => ""
				 val s6 = case z of
					      SOME(aend) => ("Visibility of operation " ^ (name_of_aend aend) ^ " : " ^ (visibility2string a))
					    | _ => ""
			     in
				 raise WFCPOG.WFCPOG_WFC_FailedException (s1^s2^s3^s4^s5^s6)
			     end
		     ) ((vis_ops)@(vis_atts))
    in
	List.all (fn a => a = true) check
    end
    
    
fun check_inheritance_classifier class model =
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
						  
	val check = 
	    List.map (fn (super,sop,this_op) =>
			 if (visibility_conforms_to (#visibility this_op) (#visibility sop))
			 then true
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
	List.all (fn a => a = true) check
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
	val check_pre = 
	    List.map (fn (a,b,c) =>
			 if c = true
			 then true
			 else 
			     let
				 val s1 = "SYNTAX ERROR: Visibility runtime consistency\n\n"
				 val s2 = "Classifier " ^ (string_of_path (name_of class)) ^ " has in operation " ^ a ^ " in the precondition " ^ (opt2string b) ^ " inconsistent modificators.\n"
			     in
				 raise WFCPOG.WFCPOG_WFC_FailedException (s1^s2)
			     end) ops_pre_vis

	val check_post =
	    List.map (fn (a,b,c) =>
			 if c = true
			 then true
			 else
			     let
				 val s1 = "SYNTAX ERROR: Visibility runtime consistency\n\n"
				 val s2 = "Classifier " ^ (string_of_path (name_of class)) ^ " has in operation " ^ a ^ " in the postcondition " ^ (opt2string b) ^ " inconsistent modificators.\n"
			     in
				 raise WFCPOG.WFCPOG_WFC_FailedException (s1^s2)
			     end) ops_post_vis
	val check_inv = 
	    List.map (fn (a,b) =>
			 if b = true
			 then true
			 else
			     let 
				 val s1 = "SYNTAX ERROR: Visibility runtime consistency\n\n"
				 val s2 = "Classifier " ^ (string_of_path (name_of class)) ^ " has in the invariant " ^ (opt2string(a)) ^ "inconsistent modificators.\n"
			     in
				 raise WFCPOG.WFCPOG_WFC_FailedException (s1^s2)
			     end) invs_vis
    in
	List.all (fn a => a = true) (check_pre@check_post@check_inv)
    end


fun check_design_classifier class model = 
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
	val check_pre = 
	    List.map (fn (a,b,c) =>
			 if c = true
			 then true
			 else 
			     let
				 val s1 = "SYNTAX ERROR: Visibility design by contract consistency\n\n"
				 val s2 = "Classifier " ^ (string_of_path (name_of class)) ^ " has in operation " ^ a ^ " in the precondition " ^ (opt2string b) ^ " inconsistent modificators.\n"
			     in
				 raise WFCPOG.WFCPOG_WFC_FailedException (s1^s2)
			     end) ops_pre_vis
    in
	List.all (fn a => a = true) check_pre
    end

fun model_entity_consistency wfc_sel (model as (clist,alist)) =
    let
	val _ = trace function_calls ("WFCPOG_Visibility_Constraint.model_entity_consistency\n")
	(* remove OclLibrary *)
	val cl = removeOclLibrary (clist)
        (* visiblity only for Classes and AssocClasses *)
	val classes = List.filter (fn a => (is_Class a) orelse (is_AssoClass a)) cl
	val res = List.all (fn a => a = true) (List.map (fn a => check_entity_classifier a model) classes)
	val _ = trace function_ends ("WFCPOG_Visibility_Constraint.model_entity_consistency\n")
    in
	res
    end

fun model_inheritance_consistency wfc_sel (model as (clist,alist)) = 
    let
	val _ = trace function_calls ("WFCPOG_Visibility_Constraint.model_inheritance_consistency\n")
	val cl = removeOclLibrary (clist)
	val classes = List.filter (fn a => (is_Class a) orelse (is_AssoClass a)) cl
	val res  = List.all (fn a => a = true) (List.map (fn a => check_inheritance_classifier a model) classes)
	val _ = trace function_ends ("WFCPOG_Visibility_Constraint.model_inheritance_consistency\n")
    in
	res
    end

fun constraint_check_by_runtime_consistency wfc_sel (model as (clist,alist)) = 
    let
	val _ = trace function_calls ("WFCPOG_Visibility_Constraint.constraint_check_by_runtime_consistency\n")
	val cl = removeOclLibrary clist
	val classes = List.filter (fn a => (is_Class a) orelse (is_AssoClass a)) cl
	val res = List.all (fn a => a = true) (List.map (fn a => check_runtime_classifier a model) classes)
	val _ = trace function_ends ("WFCPOG_Visibility_Constraint.constraint_check_by_runtime_consistency\n")
    in
	res
    end

fun constraint_design_by_contract_consistency wfc_sel (model as (clist,alist)) = 
    let
	val _ = trace function_calls ("WFCPOG_Visibility_Constraint.constraint_design_by_contract_consistency\n")
	val cl = removeOclLibrary clist
	val classes = List.filter (fn a => (is_Class a) orelse (is_AssoClass a)) cl
	val res = List.all (fn a => a = true) (List.map (fn a => check_design_classifier a model) classes)
 	val _ = trace function_calls ("WFCPOG_Visibility_Constraint.constraint_design_by_contract_consistency\n")
    in
	res
    end
end;
