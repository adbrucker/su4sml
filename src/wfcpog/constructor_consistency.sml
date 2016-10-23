(*****************************************************************************
 * su4sml --- an SML repository for managing (Secure)UML/OCL models
 *             http://projects.brucker.ch/su4sml/
 *                                                                            
 * context_declarations.sml --- 
 * This file is part of su4sml.
 *
 * Copyright (c) 2005-2007 ETH Zurich, Switzerland
 *               2008-2009 Achim D. Brucker, Germany
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

(** Implementation of the wellformed constraint for a constructor *)
signature WFCPOG_CONSTRUCTOR_CONSTRAINT = 
sig
    (** sub constraint, included in constructor consistency.*)
    val post_implies_invariant           : WFCPOG.wfpo -> Rep.Model -> (Rep_OclType.Path * Rep_OclTerm.OclTerm) list
    (** sub constraint, included in constructor consistency.*)
    val override_old_creators            : WFCPOG.wfpo -> Rep.Model -> bool
    (** sub constraint, included in constructor consistency.*)
    val force_initialize_attributes      : WFCPOG.wfpo -> Rep.Model -> (Rep_OclType.Path * Rep_OclTerm.OclTerm) list
    (** Any kind of Exception *) 
    exception WFCPO_ConstructorError of string
end
structure WFCPOG_Constructor_Constraint : WFCPOG_CONSTRUCTOR_CONSTRAINT = 
struct

(* SU4SML *)
open Rep_Core
open Rep
open Rep_OclTerm
open Rep_OclType
open Rep_HolOcl_Namespace
open Rep_HolOcl_Helper
(* OclParser *)
open ModelImport

(* WFCPO *)
open WFCPOG_Library

exception WFCPO_ConstructorError of string


fun term_post_implies_inv class oper model = 
    let
	val _ = Logger.info ("WFCPOG_Constructor_Consistency_Constraint.term_post_implies_inv\n")
	val inv = Predicate (Variable("self",type_of class),type_of class,name_of_inv class,[])
	val post = Predicate (Variable ("self",type_of class),type_of class,name_of_post class oper,args2varargs ((arguments_of_op oper)@[("result",(#result oper))]))
	val implies = OperationCall (post,Boolean,["holOclLib","Boolean","implies"],[(inv,Boolean)],Boolean)
    	val _ = Logger.info ("WFCPOG_Constructor_Consistency_Constraint.term_post_implies_inv\n")
    in
	implies
    end

fun term_init_attributes class oper model = 
    let
	val atts = all_attributes_of class model
	val _ = Logger.debug1  ("Number of attributes found = "^(Int.toString(List.length(atts))))
	val post = Predicate (Variable ("self",type_of class),type_of class,name_of_post class oper,args2varargs ((arguments_of_op oper)@[("result",(#result oper))]))
        val att_is_defined = 
	    List.map (fn a => 
			 let
			     val attr_name = (string_of_path (name_of class))^"."^(#name a)
			     val attr_typ = (#attr_type a)
			     val src = Predicate(Variable(attr_name,attr_typ),attr_typ,(name_of class)@[(#name a)],[])
			 in
			     OperationCall(src,Boolean,["oclIsDefined"],[],Boolean)
			 end) atts
	val and_atts = holocl_and_all att_is_defined
	val post_impl_atts = holocl_implies post and_atts
    in
	post_impl_atts
    end

fun check_override_classifier class (model as (clist,alist)) = 
    let
	val _ = Logger.info ("WFCPOG_Constructor_Consistency_Constraint.check_override_classifier\n")
	val creas = creation_operations_of class model
	val over_ops = modified_operations_of class model
	val check = 
	    List.map (fn a =>
			 if (List.exists (fn b => a = b) over_ops)
			 then true
			 else 
			     let 
				 val s1 = "SYNTAX ERROR: Constructor Consistency override old creators\n\n"
				 val s2 = "In the classifier " ^ (string_of_path (name_of class)) ^ " the Creator " ^ (name_of_op a) ^ "is not overriden.\n"
			     in 
				 raise WFCPOG.WFC_FailedMessage (s1^s2)
			     end) creas
	val res = List.all (fn a => a = true) check
	val _ = Logger.info ("WFCPOG_Constructor_Consistency_Constraint.check_override_classifier\n")
    in
	res
    end

fun generate_post_classifier class model = 
    let
	val _ = Logger.info ("WFCPOG_Constructor_Consistency_Constraint.check_post_classifier\n")
	val atts = all_attributes_of class model
	val pos = if (List.length(atts) = 0)
		    then []
		    else
			List.map (fn a => 
				     let
					 val term = term_post_implies_inv class a model
					 val path = ["po_cstr_","post_",(string_of_path (name_of class)),(name_of_op a)]
				     in
					 (path,term)
				     end) (creation_operations_of class model)
	val _ = Logger.info ("WFCPOG_Constructor_Consistency_Constraint.check_override_classifier\n")
    in
	pos
    end


fun generate_attribute_classifier class model = 
    let
	val _ = Logger.info ("WFCPOG_Constructor_Consistency_Constraint.generate_attribute_classifier\n")
	val pos = 
	    List.map (fn a => 
			 let
			     val term = term_init_attributes class a model
			     val path = ["po_cstr_","attribute_",(string_of_path (name_of class)),(name_of_op a)]
			 in
			     (path,term)
			 end) (creation_operations_of class model)
	val _ = Logger.info ("WFCPOG_Constructor_Consistency_Constraint.generate_attribute_classifier\n")
    in
	pos
    end


fun override_old_creators wfpo (model as (clist, alist)) = 
    let
	val _ = Logger.info ("WFCPOG_Constructor_Consistency.overrides_old_creators\n")
	val cl = removeOclLibrary clist
	val classes = List.filter (fn a => (is_Class a) orelse (is_AssoClass a)) cl
	val res = List.all (fn a => a = true) (List.map (fn a => check_override_classifier a model
							    handle WFCPOG.WFC_FailedMessage s => raise WFCPOG.WFC_FailedException (wfpo,s)) classes)
	val _ = Logger.info ("WFCPOG_Constructor_Consistency.overrides_old_creators\n")
    in
	res
    end

fun post_implies_invariant wfpo (model as (clist, alist)) =
    let
	val _ = Logger.info ("WFCPOG_Constructor_Consistency_Constraint.post_implies_invariant\n")
	val cl = removeOclLibrary clist
	val classes = List.filter (fn a => (is_Class a) orelse (is_AssoClass a)) cl
	val list = List.concat (List.map (fn a => generate_post_classifier a model) classes)
	val _ = Logger.info ("WFCPOG_Constructor_Consistency_Constraint.post_implies_invariant\n")
    in 
	list
    end


fun force_initialize_attributes wfpo (model as (clist,alist)) = 
    let
	val _ = Logger.info ("WFCPOG_Constructor_Consistency_Constraint.force_initialize_attributes\n")
	val cl = removeOclLibrary clist
	val classes = List.filter (fn a => (is_Class a) orelse (is_AssoClass a)) cl
	val list = List.concat (List.map (fn a => generate_attribute_classifier a model) classes)
	val _ = Logger.info ("WFCPOG_Constructor_Consistency_Constraint.force_initialize_attributes\n")
    in
	list
    end
end; 
