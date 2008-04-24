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
signature WFCPOG_CONSTRUCTOR_CONSTRAINT = 
sig
    (** sub constraint, included in constructor consistency.*)
    val post_implies_invariant     : WFCPOG.wfpo -> Rep.Model -> (string * Rep_OclTerm.OclTerm) list
    (** sub constraint, included in constructor consistency.*)
    val overwrites_old_creators    : WFCPOG.wfpo -> Rep.Model -> bool
    (** sub constraint, included in constructor consistency.*)
    val attributes_are_inited      : WFCPOG.wfpo -> Rep.Model -> (string * Rep_OclTerm.OclTerm) list
    (** all wfs of package constructor *)
    val generate_wfs               : WFCPOG.wfpo -> Rep.Model -> bool
    (** all pos of package constructor *)
    val generate_pos               : WFCPOG.wfpo -> Rep.Model -> (string * Rep_OclTerm.OclTerm) list
    (** Any kind of Exception *) 
    exception WFCPO_ConstructorError of string
end
structure WFCPOG_Constructor_Constraint : WFCPOG_CONSTRUCTOR_CONSTRAINT = 
struct

(* SU4SML *)
open Rep_Logger
open Rep_Core
open Rep
open Rep_OclTerm
open Rep_OclType

(* OclParser *)
open ModelImport

(* WFCPO *)
open WFCPOG_Library

exception WFCPO_ConstructorError of string

(* Make a string case insensitive *)


fun generate_return_value crea_op classifier model = 
    let
	val invs1 = all_invariants_of classifier model
	val invs2 = List.map (fn (a,b) => b) invs1
	val invs = conjugate_terms invs2
	val posts1 = postcondition_of_op crea_op
	val posts2 = List.map (fn (a,b) => b) posts1
	val posts = conjugate_terms posts2 
    in
	  (("post_implies_inv_"^(string_of_path (name_of classifier))),OperationCall(posts,Boolean,["Boolean","implies"],[(invs,type_of_term invs)],Boolean))
    end

fun post_implies_invariant_help [] model = []
  | post_implies_invariant_help (h::class) (model as (clist,alist)) = 
    let
	val creas = creation_operations_of h model
	val pos = List.map (fn a => generate_return_value a h model) creas		    
    in
	pos@(post_implies_invariant_help (class) model)
    end


fun post_implies_invariant wfpo (model as (clist, alist)) =
    let
	val _ = trace function_calls ("WP_constructor_CS.post_implies_invariant\n")
	val class = removeOclLibrary clist
	val res = post_implies_invariant_help class model
	val _ = trace function_ends ("WP_constructor_CS.post_implies_invariant\n")
    in 
	res
    end

fun overwrites_old_creators_help [] model = [true]
  | overwrites_old_creators_help (h::classes) (model as (clist,alist)) = 
    let
	val creas = creation_operations_of h model
	val over_ops = modified_operations_of h model
    in
	(List.all (fn a => List.exists (fn b => b = a) (over_ops)) creas)::(overwrites_old_creators_help classes model)
    end

fun overwrites_old_creators wfpo (model as (clist, alist)) = 
    let
	val class = removeOclLibrary clist
    in
	List.all (fn a => a) (overwrites_old_creators_help class model)
    end

fun attributes_are_inited_help [] model = []
  | attributes_are_inited_help (h::classes) (model as (clist,alist)) = 
    let
	val _ = trace function_calls ("WP_constructor_CS.attributes_are_inited_help\n")
	val creas = creation_operations_of h model
	(* TODO:  express term *)
	val _ = trace function_ends ("WP_constructor_CS.attributes_are_inited_help\n")
    in
	[]:(string * OclTerm) list
    end


fun attributes_are_inited wfpo (model as (clist,alist)) = 
    let
	val classes = removeOclLibrary clist
    in
	(attributes_are_inited_help classes model)
    end

fun generate_wfs wfpo (model:Rep.Model as (clist,alist)) = 
    let
	val _ = trace function_calls ("WP_constructor_CS.generate_wfs\n")
	val res = overwrites_old_creators wfpo model
	val _ = trace function_ends ("WP_constructor_CS.generate_wfs\n")
    in
	res
    end

fun generate_pos wfpo (model:Rep.Model as (clist,alist)) = 
    let
	val _ = trace function_calls ("WP_constructor_CS.generate_pos\n")
	val res1 = post_implies_invariant wfpo model
	val res2 = attributes_are_inited wfpo model
	val _ = trace function_ends ("WP_constructor_CS.generate_pos\n")
    in
	res1@res2
    end
end; 
