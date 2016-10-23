(*****************************************************************************
 * su4sml --- an SML repository for managing (Secure)UML/OCL models
 *             http://projects.brucker.ch/su4sml/
 *                                                                            
 * make_classifier_list.sml --- 
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
(* $Id$ *)

signature UPDATE_MODEL = 
sig
    (* exceptions *)
    exception AlreadyInitValueError of string * Rep_OclTerm.OclTerm * string
    exception NotYetSupportedError of string
    exception ContextToClassifierError of Rep_OclTerm.OclTerm * string
    exception OperationUpdateError of string * Context.ConditionType * Rep_OclTerm.OclTerm * string 
				      
    (* operations *) 
    val add_precondition           : string * string option * Rep_OclTerm.OclTerm -> Rep_Core.operation list -> Rep_Core.operation list
    val add_postcondition          : string * string option * Rep_OclTerm.OclTerm -> Rep_Core.operation list -> Rep_Core.operation list
    val add_body                   : string * string option * Rep_OclTerm.OclTerm -> Rep_Core.operation list -> Rep_Core.operation list
    val add_attribute              : string * Rep_OclTerm.OclTerm -> Rep_Core.attribute list -> Rep_Core.attribute list
    val context_to_classifier      : Context.context -> Rep_Core.transform_model -> Rep_Core.Classifier
    val merge_classifier           : Rep_Core.Classifier -> Rep_Core.Classifier list -> Rep_Core.Classifier list
    val gen_updated_classifier_list: (Context.context option) list -> Rep_Core.Classifier list -> Rep_Core.Classifier list
end

structure Update_Model:UPDATE_MODEL =
struct

open Rep_Core;
open Context;

exception AlreadyInitValueError of string * Rep_OclTerm.OclTerm * string
exception NotYetSupportedError of string
exception ContextToClassifierError of Rep_OclTerm.OclTerm * string
exception OperationUpdateError of string * Context.ConditionType * Rep_OclTerm.OclTerm * string 

(* Error logging *)
val high = 5
val medium = 20
val low = 100



(* RETURN: operation list *)
fun add_precondition (op_name,cond_name,term) ((oper: operation)::operations_tail) =
 if (#name oper = op_name) then
     ({name = #name oper,
       precondition = (#precondition oper)@[(cond_name,term)],
       postcondition = #postcondition oper,
       body = #body oper,
       arguments = #arguments oper,
       result = #result oper,
       isQuery = #isQuery oper,
       visibility = #visibility oper,
       stereotypes = #stereotypes oper,
       scope = #scope oper})
     ::(operations_tail)
 else
     oper::(add_precondition (op_name,cond_name,term) operations_tail)

(* RETURN: operation list *)
fun add_postcondition (op_name,cond_name,term) ((oper: operation)::operations_tail) =
 if (#name oper = op_name) then
     ({name = #name oper,
       precondition = #precondition oper, 
       postcondition = (#postcondition oper)@[(cond_name,term)],
       body = #body oper,
       arguments = #arguments oper,
       result = #result oper,
       isQuery = #isQuery oper,
       stereotypes = #stereotypes oper,
       visibility = #visibility oper,
       scope = #scope oper})
     ::(operations_tail)
 else
     oper::(add_postcondition (op_name,cond_name,term) operations_tail)

(* RETURN: operation list *)
fun add_body (op_name,cond_name,term) ((oper: operation)::operations_tail) =
 if (#name oper = op_name) then
     ({name = #name oper,
       precondition = #precondition oper, 
       postcondition = #postcondition oper,
       body = (#body oper)@[(cond_name,term)],
       arguments = #arguments oper,
       result = #result oper,
       isQuery = #isQuery oper,
       stereotypes = #stereotypes oper,
       visibility = #visibility oper,
       scope = #scope oper})
     ::(operations_tail)
 else
     oper::(add_body (op_name,cond_name,term) operations_tail)

(* RETURN: operation list *)
fun add_operations cond_type (op_name,cond_name,term) [] = raise OperationUpdateError (op_name,cond_type,term,"Class has no operations\n")
  | add_operations cond_type (op_name,cond_name,term) op_list =
    case cond_type of
	pre => 
	let
	    val _ = Logger.debug3 ("pre\n")
	in
	    add_precondition (op_name,cond_name,term) op_list
	end
      | post => 
	let 
	    val _ = Logger.debug3 ("post\n")
	in
	    add_postcondition (op_name,cond_name,term) op_list
	end
      | body =>
	let
	    val _ = Logger.debug3 ("body\n")
	in
	    add_body (op_name,cond_name,term) op_list
	end

(* RETURN: attribute list *)
fun add_attribute (attr_name,term) ((attr: attribute)::attribute_tail) =
  if (#name attr = attr_name) then
      if (#init attr = NONE) then
      ({name = #name attr,
	attr_type = #attr_type attr,
	visibility = #visibility attr,
	scope = #scope attr,
	stereotypes = #stereotypes attr,
	init = SOME (term)})
       ::(attribute_tail)
      else
	  raise (AlreadyInitValueError (attr_name,term, "Classifier already defined an init value ..." ^ "\n"))
  else
      attr::(add_attribute (attr_name,term) attribute_tail)

(* RETURN: Classifier *)
(* INVARIANTS *)
fun context_to_classifier (Inv (path,string_opt,term)) model = 
    let 
	val _ = Logger.debug3 ("Invariant to Classifier ... " ^ "\n")     
	val c = class_of_type (Rep_OclType.Classifier (path)) model 
    in
	(
	 case c of
	     (Class {name,parent,attributes,operations,associations,visibility,interfaces,stereotypes,invariant,thyname,activity_graphs}) =>
	     Class 
		 {
		  name = name,
		  parent = parent,
		  attributes = attributes,
		  operations = operations,
		  associations = associations,
		  interfaces = interfaces,
		  stereotypes = stereotypes,
		  invariant = invariant@[(string_opt,term)],
		  thyname = thyname,
		  visibility = visibility,
 		  activity_graphs = activity_graphs
		 }
	   | (AssociationClass {name,visibility,parent,attributes,operations,associations,association,interfaces,stereotypes,invariant,thyname,activity_graphs}) =>
	     AssociationClass 
		 {
		  name = name,
		  parent = parent,
		  attributes = attributes,
		  operations = operations,
		  associations = associations,
		  association = association,
		  interfaces = interfaces,
		  visibility = visibility,
		  stereotypes = stereotypes,
		  invariant = invariant@[(string_opt,term)],
		  thyname = thyname,
 		  activity_graphs = activity_graphs
		 }
	   | (Interface {name,parents,operations,stereotypes,invariant,thyname}) =>
	     Interface
	         {
		  name = name,
		  parents = parents,
		  operations = operations,
		  stereotypes = stereotypes,
		  invariant = invariant,
		  thyname = thyname 
		 }
	   | (Primitive {...}) => raise ContextToClassifierError (term,"Not possible to have an invariant on a 'Primitve' Type\n")
	   | (Enumeration {...}) => raise ContextToClassifierError (term,"Not possible to have an invariant on an 'Enumeration'Type\n")
	)
    end
  (* Attribute constraints *)
  |  context_to_classifier (Attr (path,typ,attrorassoc,term)) model = 
     let
   	 val _ = Logger.debug3 ("Attribute to Classifier ... " ^ "\n")
	 val c = class_of_type (Rep_OclType.Classifier (real_path path)) model
     in
	 (
	  case c of
	      (Class {name,parent,attributes,visibility,operations,associations,interfaces,stereotypes,invariant,thyname,activity_graphs}) =>
	      (
	       case attrorassoc of 
		   init => 
		   Class 
		       {name = name,
			parent = parent,
			attributes = add_attribute (List.last path,term) (attributes),
			operations = operations,
			associations = associations,
			interfaces = interfaces,
			stereotypes = stereotypes,
			visibility = visibility,
			invariant = invariant,
			thyname = thyname,
 			activity_graphs = activity_graphs
		       }
		 | derive => raise NotYetSupportedError ("derive not yet supported ... sorry" ^ "\n")
		 | def => raise NotYetSupportedError ("def not yet supported ... sorry" ^ "\n")
	      )
	    | (AssociationClass {name,parent,visibility,attributes,operations,associations,association,interfaces,stereotypes,invariant,thyname,activity_graphs}) =>
	      (
	       case attrorassoc of 
		   init => 
		   AssociationClass 
		       {
			name = name,
			parent = parent,
			attributes = add_attribute (List.last path,term) (attributes),
			operations = operations,
			visibility = visibility,
			associations = associations,
			association = association,
			interfaces = interfaces,
			stereotypes = stereotypes,
			invariant = invariant,
			thyname = thyname,
 			activity_graphs = activity_graphs
		       }
		 | derive => raise NotYetSupportedError ("derive not yet supported ... sorry" ^ "\n")
		 | def => raise NotYetSupportedError ("def not yet supported ... sorry" ^ "\n")
	      )
	    | (Interface {...}) => raise ContextToClassifierError (term,"Not possible to have an attribute constraint on an 'Interface' ... \n")
	    | (Primitive {...}) => raise ContextToClassifierError (term,"Not possible to have an attribute constraint on a 'Primitive' ... \n") 
	    | (Enumeration {...}) => raise ContextToClassifierError (term,"Not possible to have an attribute constraint on a 'Enumeration' ...\n")
	 )
     end     
  (* Operation constraints *)
  | context_to_classifier (Cond (path,op_name,args,ret_typ,cond_type,cond_name,term)) model=
    let
	val _ = Logger.debug3 ("Cond to Classifier ... " ^ "\n") 
	val c = class_of_type (Rep_OclType.Classifier (path)) model
    in
	(
	 case c of
	     (Class {name,parent,attributes,visibility,operations,associations,interfaces,stereotypes,invariant,thyname,activity_graphs}) =>
	     Class 
		 {
		  name = name,
		  parent = parent,
		  visibility = visibility,
		  attributes = attributes,
		  operations = add_operations cond_type (op_name,cond_name,term) operations,
		  associations = associations,
		  interfaces = interfaces,
		  stereotypes = stereotypes,
		  invariant = invariant,
		  thyname = thyname,
 		  activity_graphs = activity_graphs
		  }
	   | (AssociationClass {name,visibility,parent,attributes,operations,associations,association,interfaces,stereotypes,invariant,thyname,activity_graphs}) =>
	     AssociationClass 
		 {
		  name = name,
		  parent = parent,
		  visibility = visibility,
		  attributes = attributes,
		  operations = add_operations cond_type (op_name,cond_name,term) operations,
		  associations = associations,
		  association = association,
		  interfaces = interfaces,
		  stereotypes = stereotypes,
		  invariant = invariant,
		  thyname = thyname,
 		  activity_graphs = activity_graphs
		 }
	   | (Interface {name,parents,operations,stereotypes,invariant,thyname}) =>
	     Interface
	         {
		  name = name,
		  parents = parents,
		  operations = add_operations cond_type (op_name,cond_name,term) operations,
		  stereotypes = stereotypes,
		  invariant = invariant,
		  thyname = thyname 
		 }
	   | (Primitive {...}) => raise ContextToClassifierError (term,"Not possible to have pre/post-conditions on a 'Primitve' ... \n") 
	   | (Enumeration {...}) => raise ContextToClassifierError (term,"Not possible to have pre/post-conditions on an 'Enumeration' ... \n")
	)
    end   

(* RETURN: Classifier list *)
fun merge_classifier classifier (h::classifier_list_tail) =
  if (type_of classifier = type_of h) then
      (* update classifier *)
      (classifier)::(classifier_list_tail)
  else
      (* take next classifier *)  
      h::(merge_classifier classifier (classifier_list_tail))

(* RETURN: Classifier list *)
fun gen_updated_classifier_list [] model = model
| gen_updated_classifier_list  (SOME(h)::context_list_tail) model  =
  let
      val updated_classifier = context_to_classifier h (model,[])
  in
      gen_updated_classifier_list context_list_tail (merge_classifier updated_classifier model)
      handle AlreadyInitValueError (attr_path,term,mes) =>
	     let
	       val _ = Logger.error("AlreadyInitValueError:\n"
			            ^"Error Message: " ^ mes ^ "\n"
				    ^"In attribute or association: " ^ (attr_path) ^ "\n"
				    ^"In Term: " ^ Ocl2String.ocl2string false term ^ "\n")
	     in
	       []
	     end
	   | NotYetSupportedError mes =>
	     let
	       val _ = Logger.error ("NotYetSupportedError:\n"
				     ^"Error Message: " ^ mes ^ "\n")
	     in
	       []
	     end
	   | ContextToClassifierError (term,mes) =>
	     let
	       val _ = Logger.error ("ContextToClassifierError:\n"
				     ^"Error Message: " ^ mes ^ "\n"
				     ^"In Term: " ^ Ocl2String.ocl2string false term ^ "\n")
	     in
	       []
	     end
	   | OperationUpdateError (meth_path,cond_type,term,mes) =>
	     let
	       val _ = Logger.error ("AlreadyInitValueError:\n"
				     ^"Error Message: " ^ mes ^ "\n"
				     ^"In condition: " ^ (cond_type_to_string cond_type) ^ "\n"
				     ^"In operation: " ^ (meth_path) ^ "\n"
				     ^"In Term: " ^ Ocl2String.ocl2string false term ^ "\n")
	     in
	       []
	     end
  end
| gen_updated_classifier_list (NONE::context_list_tail) model = gen_updated_classifier_list context_list_tail model
(* end struct *)
end 
