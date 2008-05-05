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
    val weaken_precondition         : WFCPOG.wfpo -> Rep.Model -> (Rep_OclType.Path * Rep_OclTerm.OclTerm) list

    (** sub constraint, included in liskov.*)
    val strengthen_postcondition    : WFCPOG.wfpo -> Rep.Model -> (Rep_OclType.Path * Rep_OclTerm.OclTerm) list

    (** sub constraint, included in liskov.*)
    val conjugate_invariants        : WFCPOG.wfpo -> Rep.Model -> (Rep_OclType.Path * Rep_OclTerm.OclTerm) list

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
open Rep_HolOcl_Namespace
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
	

datatype lisk_type = weaken_pre | strengthen_post
	   
structure WFCPOG_LSK_Data = WFCPOG_DataFun
			  (struct
			   type T =  Liskov_args;
			   val empty = ({model="", size=0});
			   fun copy T = T;
			   fun extend T = T;
			   end);
	  

fun print_liskov_args (args:Liskov_args) = 
    print (concat["Lisov with args: size=\"",Int.toString (#size args), "\"",
		  " and model=\"", #model (args),"\"\n\n"]) 

fun generate_return_value typ oper sub_class super_class  model = 
    let
      val op_of_super = get_operation (name_of_op oper) super_class model
      val sub_name = string_of_path (name_of sub_class)
      val super_name = string_of_path (name_of super_class)
      val op_name = name_of_op oper
      val super_type = Rep_Core.type_of super_class
      val sub_type   = Rep_Core.type_of sub_class
      fun OclLocalValid t = (OperationCall (t,
					    Boolean,["holOclLib","Boolean","OclLocalValid"],
					    [(Literal("\\<tau>",OclState),DummyT)],Boolean))
    in
      case typ of
	weaken_pre => 
	(* DEFINITION (OOSC p.578): sub_pre -> super_pre*)
	let
	  val pre_super = Predicate(Variable("self_super",super_type),super_type, 
				    name_of_pre super_class oper,
				    args2varargs (arguments_of_op op_of_super)) 
	  val pre_sub = Predicate(Variable("self_sub", sub_type),sub_type, 
				  name_of_pre sub_class oper,
				  args2varargs (arguments_of_op op_of_super)) 

        in 
	  OperationCall(OclLocalValid pre_super,Boolean,["holOclLib","Boolean","implies"],
			[(OclLocalValid pre_sub,Boolean)],Boolean) 
	end
      | strengthen_post => 
	(* DEFINITION (OOSC p.578): sub_posts -> super_post *)
	let
	  val post_super = Predicate(Variable("self_super",Rep_Core.type_of super_class),
				     Rep_Core.type_of super_class, 
				     name_of_post super_class oper,
				     args2varargs ((arguments_of_op op_of_super)@[("result",DummyT)])) 
	  val post_sub = Predicate(Variable("self_sub",Rep_Core.type_of sub_class),
				   Rep_Core.type_of sub_class, 
				   name_of_post sub_class oper,
				   args2varargs ((arguments_of_op op_of_super)@[("result",DummyT)])) 
        in 
	  OperationCall(OclLocalValid post_sub,Boolean,["holOclLib","Boolean","implies"],
			[(OclLocalValid post_super,Boolean)],Boolean) 
	end	  
    end
    
		 
fun weaken_precondition_help [] model = []
  | weaken_precondition_help (class::clist) model =
    let
	val mo = modified_operations_of class model 
	(* (operation of subclass, classifier of super class) *)
	val raw_po = List.map (fn a => (a,(go_up_hierarchy class (class_contains_op a model) model))) mo
        (* proofs obligation for classifier [(term,constraint info)] *)
 	val pos = List.map (fn (a,b) => (["po_lsk_pre_"]@(name_of class)@["_"]@[name_of_op a],
	                                  generate_return_value weaken_pre a class b model)) raw_po
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
 	val pos = List.map (fn (a,b) => (["po_lsk_post"]@["_"]@(name_of class)@["_"]@[name_of_op a],
	                                 generate_return_value strengthen_post a class b model)) raw_po
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
        val parents = parents_of class model
 	val invs = List.map (fn a => Predicate(Variable(varcounter.nextStr(),Rep_Core.type_of a),Rep_Core.type_of a,name_of_inv a,[selfarg (Rep_Core.type_of a)])) parents 
    in
	if (List.length(invs) = 0)
	then (conjugate_invariants_help clist model)
	else (["po_lsk_inv"]@["_"]@(name_of class)@["_"],
	                               conjugate_terms invs)::(conjugate_invariants_help clist model)
	    
    end

fun conjugate_invariants wfpo (model as (clist,alist)) = 
    let 
	val cl = removeOclLibrary clist
    in
	conjugate_invariants_help cl model
    end

end
