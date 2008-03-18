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

(** Implementation of the wellformed constraint for a constructor *)
signature CONSTRUCTOR_CONSTRAINT = 
sig
    (** sub constraint, included in constructor consistency.*)
    val post_implies_invariant     : Rep.Model -> Rep_OclTerm.OclTerm list
    (** sub constraint, included in constructor consistency.*)
    val overwrites_old_creators    : Rep.Model -> bool
    (** sub constraint, included in constructor consistency.*)
    val attributes_are_inited      : Rep.Model -> bool
    (** Any kind of Exception *)
    exception WFCPO_ConstructorError of string
end
structure Constructor_Constraint : CONSTRUCTOR_CONSTRAINT = 
struct

(* SU4SML *)
open library
open Rep_Core
open Rep
open Rep_OclTerm
open Rep_OclType

(* OclParser *)
open ModelImport

(* WFCPO *)
open WFCPO_Naming
open WFCPOG_Library

exception WFCPO_ConstructorError of string

(* Make a string case insensitive *)


fun generate_return_value crea_op classifier model = 
    let
	(* wrap invariants *)
	val invariants = all_invariants_of classifier model
	val self_type = type_of classifier
	val self_arg = Variable ("self",self_type)
	val invs = List.map (fn (a,b) => wrap_predicate b a [(self_arg,self_type)]) invariants
	val final_inv = conjugate_terms invs
	(* wrap postconditions *)
	val sig_args = arguments_of_op crea_op
	val args = (self_arg,self_type)::(List.map (fn (a,b) => (Variable(a,b),b)) sig_args)
	val postconditions = postcondition_of_op crea_op
	val posts = List.map (fn (a,b) => wrap_predicate b a args) postconditions
	val final_post = conjugate_terms posts
    in
	  OperationCall(final_post,Boolean,["Boolean","implies"],[(final_inv,type_of_term final_inv)],Boolean)
    end

fun post_implies_invariant_help [] model = []
  | post_implies_invariant_help (h::class) (model as (clist,alist)) = 
    let
	val crea_ops = creation_operations_of h model
	val pos = List.map (fn a => generate_return_value a h model) crea_ops		       
							    
    in
	pos@(post_implies_invariant_help (class) model)
    end


fun post_implies_invariant (model as (clist, alist)) =
    let
	val class = removeOclLibrary clist
    in
	post_implies_invariant_help class model
    end

fun overwrites_old_creators_help [] model = [true]
  | overwrites_old_creators_help (h::class) (model as (clist,alist)) = 
    let
	val crea = creation_operations_of h model
	val over_ops = modified_operations_of h model
    in
	(List.all (fn a => List.exists (fn b => b =a) (over_ops)) crea)::(overwrites_old_creators_help class model)
    end

fun overwrites_old_creators (model as (clist, alist)) = 
    let
	val class = removeOclLibrary clist
    in
	List.all (fn a => a) (overwrites_old_creators_help class model)
    end

fun attributes_are_inited_help [] model = [true]
  | attributes_are_inited_help (h::class) (model as (clist,alist)) = 
    let
	val attrs = attributes_of h
	fun inited NONE = false
	  | inited (SOME(x)) = true
    in
	(List.all (fn a => inited (#init a)) (attrs))::(attributes_are_inited_help class model)
    end


fun attributes_are_inited (model as (clist,alist)) = 
    let
	val class = removeOclLibrary clist
    in
	List.all (fn a => a) (attributes_are_inited_help class model)
    end
end; 
