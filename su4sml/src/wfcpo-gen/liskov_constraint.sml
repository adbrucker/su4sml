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
signature LISKOV_CONSTRAINT =
sig
    include BASE_CONSTRAINT
    (** sub constraint, included in liskov.*)
    val weaken_precondition         : Rep.Model -> Rep_OclTerm.OclTerm list
    (** sub constraint, included in liskov.*)
    val strengthen_postcondition    : Rep.Model -> Rep_OclTerm.OclTerm list
    (** sub constraint, included in liskov.*)
    val conjugate_invariants        : Rep.Model -> Rep_OclTerm.OclTerm list

end 
functor Liskov_Constraint (SuperCon : BASE_CONSTRAINT) : LISKOV_CONSTRAINT =
struct

(* su4sml *)
open Rep_Core
open Rep_OclTerm
open Rep_OclType
open Ext_Library
open ModelImport
open Rep2String
(* wfcpo-gen *)
open WFCPO_Library
open WFCPO_Naming

exception ConstraintError of string
exception BaseConstraintError of string
exception LiskovError of string

type constraint = SuperCon.constraint




fun generate_return_value typ oper sub_class super_class model = 
    let
	val op_of_super = get_operation (name_of_op oper) super_class
	val sub_name = string_of_path (name_of sub_class)
	val super_name = string_of_path (name_of super_class)
	val op_name = name_of_op oper
    in
	case typ of
	    (* precondition *)
	    (* DEFINITION (OOSC p.578): 
	     
             --------------------------------
          |   super_pres -> sub_posts    |
             ________________________________
             *)	 
	    1 => 
	    let
                (* preconditions of super type in one term *)
		val term_super = conjugate_terms (List.map (fn (a,b) => (Predicate(b,type_of_term b,["liskov","weaken precondition",super_name,op_name,(generate_opt_name "gen_pre" a)],[]))) (precondition_of_op op_of_super))
                (* preconditions of sub type in one term *)
		val term_sub = conjugate_terms (List.map (fn (a,b) => (Predicate(b,type_of_term b,["liskov","weaken precondition",sub_name,op_name,(generate_opt_name "gen_pre" a)],[]))) (precondition_of_op oper))
	    in 
		OperationCall(term_super,Boolean,["Boolean","implies"],[(term_sub,type_of_term term_sub)],Boolean)
	    end

	  (* postcondition *)
	  (* DEFINITION (OOSC p.578): 

             --------------------------------
             |   sub_posts -> super_posts   |
             ________________________________
           *)

	  | 2 => 
	    let 
		(* postconditions of sub type in one term *)
		val term_sub = conjugate_terms (List.map (fn (a,b) => (Predicate(b,type_of_term b,["liskov","strengthen postcondition",sub_name,op_name,(generate_opt_name "gen_post" a)],[]))) (postcondition_of_op oper))
                (* postconditions of super type in one term *)
		val term_super = conjugate_terms (List.map (fn (a,b) => (Predicate(b,type_of_term b,["liskov","strengthen postcondition",super_name,op_name,(generate_opt_name "gen_post" a)],[]))) (postcondition_of_op op_of_super))
	    in 
		OperationCall(term_sub,Boolean,["Boolean","implies"],[(term_super,type_of_term term_super)],Boolean)
	    end
	  | x => raise LiskovError ("Wrong Argument for generate_return_value. Only 1 (pre) and 2 (post) allowed.\n")
    end
    
		 
fun weaken_precondition_help [] model = []
  | weaken_precondition_help (class::clist) model =
    let
	val mo = modified_operations_of (name_of class) model
	(* (operation of subclass, classifier of super class) *)
	val raw_po = List.map (fn a => (a,(go_up_hierarchy class (class_contains_op a model) model))) mo
        (* proofs obligation for classifier [(term,constraint info)] *)
 	val pos = List.map (fn (a,b) => generate_return_value 1 a class b model) raw_po
    in
	pos@(weaken_precondition_help clist model)
    end

fun weaken_precondition (model as (clist,alist)) = 
    let
	val cl = removeOclLibrary clist
    in
	weaken_precondition_help cl model
    end


fun strengthen_postcondition_help [] model = []
  | strengthen_postcondition_help (class::clist) model =
    let
	val mo = modified_operations_of (name_of class) model
	(* (operation of subclass, classifier of super class) *)
	val raw_po = List.map (fn a => (a,(go_up_hierarchy class (class_contains_op a model) model))) mo
        (* proof obligations for classifier [(term,constraint info)] *)
 	val pos = List.map (fn (a,b) => generate_return_value 2 a class b model) raw_po
    in
	pos@(strengthen_postcondition_help clist model)
    end

fun strengthen_postcondition (model as (clist,alist)) = 
    let
	val cl = removeOclLibrary clist
    in
	strengthen_postcondition_help cl model
    end

fun conjugate_invariants_help [] model = []
  | conjugate_invariants_help (class::clist) model = 
    let
	(* get the invariants of all parents *)
	val invariants = all_invariants_of class model
	val c_name = string_of_path (name_of class)
	val invs = List.map (fn (a,b) => Predicate(b,type_of_term b,["liskov","conjugate invariants",c_name,(generate_opt_name "inv" a)],[])) invariants
    in
	if (List.length(invs) = 0)
	then (conjugate_invariants_help clist model)
	else
	    (conjugate_terms invs)::(conjugate_invariants_help clist model)
    end

fun conjugate_invariants (model as (clist,alist)) = 
    let
	val cl = removeOclLibrary clist
    in
	conjugate_invariants_help cl model
    end

fun generate_po (model as (clist,alist)) = 
    let
	val _ = trace zero ("generate_po: \n")
	val _ = trace zero ("weaken the precondition.\n")
	val x1 = weaken_precondition model
	val _ = trace zero ("strengthen the postcondition.\n")
	val x2 = strengthen_postcondition model
	val _ = trace zero ("conjugate the invariants.\n")
	val x3 = conjugate_invariants model
    in
	x1@x2@x3
    end


end
