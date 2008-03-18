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
signature VISIBILITY_CONSTRAINT =
sig
    val are_conditions_visible : WFCPOG.wfpo -> Rep.Model -> bool



end
structure Visibility_Constraint:VISIBILITY_CONSTRAINT = 
struct

(* su4sml *)
open library
open Rep_Core
open Rep_OclTerm
open Rep_OclType
open Rep2String
open XMI_DataTypes

(* oclparser *)
open ModelImport

(* wfcpo-gen *)
open WFCPOG_Library
open WFCPO_Naming

exception WFCPO_VisibilityError of string



fun modificator_conforms_to private private = true
  | modificator_conforms_to protected private = true
  | modificator_conforms_to package private = true
  | modificator_conforms_to public private = true

  | modificator_conforms_to protected protected = true
  | modificator_conforms_to package protected = true
  | modificator_conforms_to public protected = true

  | modificator_conforms_to package package = true
  | modificator_conforms_to public package = true

  | modificator_conforms_to public public = true

  | modificator_conforms_to x y = false

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
	if (modificator_conforms_to (#visibility att) modif)
	then (expr_is_visible modif src model)
	else false
    end    
  | expr_is_visible modif (x as OperationCall(src,styp,path,args,rtyp)) model = 
    let
	val typ = trace 50 ("expr_is_visible: OpCall \n")
	val typ = type_of_term src
	val cl = class_of_term (Variable("x",typ)) model
	val op_name = List.last(path)
	val _ = trace 100 ("OperationCall: " ^ (Ocl2String.ocl2string false x)  ^ "\n")
	val _ = trace 100 ("Classifier : " ^ Rep2String.classifier2string cl ^ "\n")
	val _ = trace 100 ("Op_name : " ^ op_name ^ "\n")
	val oper = get_operation op_name cl model
	val _ = trace 100 ("end expr_is_visible")
    in
	if (modificator_conforms_to (#visibility oper) modif) 
	then ((List.all (fn (a,b) => (expr_is_visible modif a model)) args) andalso (expr_is_visible modif src model))
	else false
    end
  | expr_is_visible modif (x as AttributeCall(src,styp,path,rtyp)) model =
    let
	val cl = class_of_term src model
	val att_name = List.last(path)
	val att = get_attribute att_name cl model
	val _ = trace 100 ("end expr_is_visible")
    in
	if (modificator_conforms_to (#visibility att) modif)
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
					  (* TODO: bug must be here *)
					  (* crashed after the third extraction of 
					     OpCall from postcondition of 'isEmpty
					   *)
					  List.map (fn (x,y) => 
						       let
							   val _ = trace 50 ("next post: \n" )
						       in
							   expr_is_visible public y model
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
					  List.map (fn (x,y) => expr_is_visible package y model) posts
				      end
				  ) (package_operations_of h model)
	    val _ = trace 50 ("package operations done.\n\n")
	    val pro_op = List.map (fn a => 
				      let
					  val _ = trace 50 ("protected operations " ^ (name_of_op a) ^ "\n")
					  val posts = postcondition_of_op a
				      in
					  List.map (fn (x,y) => expr_is_visible protected y model) posts
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
	val _ = trace 50 ("Visibility Constraint starts ...\n")
	val classes = removeOclLibrary clist
	val _ = trace 50 ("OclLibrary removed ...\n")
    in
	are_conditions_visible_help classes model
    end
end;
