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
signature WFCPOG_LISKOV_CONSTRAINT =
sig
    type Liskov_args
    
    structure WFCPOG_LSK_Data :
	      sig
		  type T = Liskov_args
		  val get : WFCPOG.wfpo -> T
		  val put : T -> WFCPOG.wfpo -> WFCPOG.wfpo
		  val map : (T -> T) -> WFCPOG.wfpo -> WFCPOG.wfpo
	      end

    val print_liskov_args : Liskov_args -> unit
   
    (** sub constraint, included in liskov.*)
    val weaken_precondition         : WFCPOG.wfpo -> Rep.Model -> Rep_OclTerm.OclTerm list

    (** sub constraint, included in liskov.*)
    val strengthen_postcondition    : WFCPOG.wfpo -> Rep.Model -> Rep_OclTerm.OclTerm list

    (** sub constraint, included in liskov.*)
    val conjugate_invariants        : WFCPOG.wfpo -> Rep.Model -> Rep_OclTerm.OclTerm list

    (** all three subconstraints together *)
    val generate_po                 : WFCPOG.wfpo -> Rep.Model -> Rep_OclTerm.OclTerm list
end 
structure WFCPOG_Liskov_Constraint : WFCPOG_LISKOV_CONSTRAINT = 
struct

exception WFCPOG_LiskovError of string
(* su4sml *)
open Rep_Logger
open Rep_Core
open Rep_OclTerm
open Rep_OclType
open Rep2String

(* oclparser *)
open ModelImport

(* wfcpo-gen *)
open WFCPOG_Library
open Datatab
exception WFCPO_LiskovError of string

type Liskov_args = {
     model:string,
     size:int
}
		   
structure WFCPOG_LSK_Data = WFCPOG_DataFun
			  (struct
			   type T =  Liskov_args;
			   val empty = ({model="", size=0});
			   fun copy T = T;
			   fun extend T = T;
			   end);
	  

fun print_liskov_args (args:Liskov_args) = 
    print (concat["Lisov with args: size=\"",Int.toString (#size args), "\""," and model=\"", #model (args),"\"\n\n"]) 

fun generate_return_value typ oper sub_class super_class model = 
    let
	val op_of_super = get_operation (name_of_op oper) super_class model
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
		val term_super = conjugate_terms (List.map (fn (a,b) => (Predicate(b,type_of_term b,[("dummy_name")],(args2varargs (arguments_of_op op_of_super))))) (precondition_of_op op_of_super))
                (* preconditions of sub type in one term *)
		val term_sub = conjugate_terms (List.map (fn (a,b) => (Predicate(b,type_of_term b,[("dummy_name")],(args2varargs (arguments_of_op oper))))) (precondition_of_op oper))
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
		val term_sub = conjugate_terms (List.map (fn (a,b) => (Predicate(b,type_of_term b,[("dummy_name")],(args2varargs (arguments_of_op oper))))) (postcondition_of_op oper))
                (* postconditions of super type in one term *)
		val term_super = conjugate_terms (List.map (fn (a,b) => (Predicate(b,type_of_term b,[("dummy_name")],(args2varargs (arguments_of_op op_of_super))))) (postcondition_of_op op_of_super))
	    in 
		OperationCall(term_sub,Boolean,["Boolean","implies"],[(term_super,type_of_term term_super)],Boolean)
	    end
	  | x => raise WFCPO_LiskovError ("Wrong Argument for generate_return_value. Only 1 (pre) and 2 (post) allowed.\n")
    end
    
		 
fun weaken_precondition_help [] model = []
  | weaken_precondition_help (class::clist) model =
    let
	val mo = modified_operations_of class model 
	(* (operation of subclass, classifier of super class) *)
	val raw_po = List.map (fn a => (a,(go_up_hierarchy class (class_contains_op a model) model))) mo
        (* proofs obligation for classifier [(term,constraint info)] *)
 	val pos = List.map (fn (a,b) => generate_return_value 1 a class b model) raw_po
    in
	pos@(weaken_precondition_help clist model)
    end

fun weaken_precondition wfpo (model as (clist,alist)) = 
    let
	val cl = removeOclLibrary clist
    in
	weaken_precondition_help cl model
    end


fun strengthen_postcondition_help [] model = []
  | strengthen_postcondition_help (class::clist) model =
    let
	val mo = modified_operations_of class model
	(* (operation of subclass, classifier of super class) *)
	val raw_po = List.map (fn a => (a,(go_up_hierarchy class (class_contains_op a model) model))) mo
        (* proof obligations for classifier [(term,constraint info)] *)
 	val pos = List.map (fn (a,b) => generate_return_value 2 a class b model) raw_po
    in
	pos@(strengthen_postcondition_help clist model)
    end

fun strengthen_postcondition wfpo (model as (clist,alist)) = 
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
	val invs = List.map (fn (a,b) => Predicate(b,type_of_term b,[("dummy_name")],[selfarg (type_of class)])) invariants
    in
	if (List.length(invs) = 0)
	then (conjugate_invariants_help clist model)
	else (conjugate_terms invs)::(conjugate_invariants_help clist model)
	    
    end

fun conjugate_invariants wfpo (model as (clist,alist)) = 
    let 
	val cl = removeOclLibrary clist
	
    in
	conjugate_invariants_help cl model
    end

fun generate_po wfpo (model as (clist,alist)) = 
    let
	val msg1 = ("\n\ngenerate_po: \n")
	val x1 = weaken_precondition wfpo model
	val msg2 = ("weaken the precondition: " ^ Int.toString(List.length(x1)) ^ " terms generated.\n") 
	val x2 = strengthen_postcondition wfpo model 
	val msg3 = ("strengthen the postcondition: " ^ Int.toString(List.length(x2)) ^ " terms generated.\n")
	val x3 = conjugate_invariants wfpo model 
	val msg4 = ("conjugate the invariants: " ^ Int.toString(List.length(x3)) ^ " terms generated.\n\n")
	val _ = trace wgen (msg1^msg2^msg3^msg4)
    in
	x1@x2@x3
    end
end

