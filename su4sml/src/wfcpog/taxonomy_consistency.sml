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
signature WFCPOG_TAXONOMY_CONSTRAINT =
sig 
    type TAX_args      
    structure WFCPOG_TAX_Data :
	      sig
		  type T = TAX_args
		  val get : WFCPOG.wfpo -> T
		  val put : T -> WFCPOG.wfpo -> WFCPOG.wfpo
		  val map : (T -> T) -> WFCPOG.wfpo -> WFCPOG.wfpo
	      end
    
    val print_taxonomy_args : TAX_args -> string

    (** Subconstraint *)
    val check_depth   : WFCPOG.wfpo -> Rep.Model -> bool	   

    exception WFCPOG_TaxonomyError of string
end

structure WFCPOG_Taxonomy_Constraint:WFCPOG_TAXONOMY_CONSTRAINT = 
struct

(* su4sml *)
open Rep_Logger
open Rep_Core
open Rep_OclTerm
open Rep_OclType
open Rep2String
open XMI_DataTypes

(* oclparser *)
open ModelImport

(* wfcpo-gen *)
open WFCPOG_Library
open Datatab

exception WFCPOG_TaxonomyError of string

type TAX_args = {
     key : int,
     max_depth : int
}

structure WFCPOG_TAX_Data = WFCPOG_DataFun
(struct
 val name =" tax";
 type T = TAX_args;
 val empty = ({key=0,max_depth=0});
 fun copy T = T;
 fun extend T = T;
end);

 
fun print_taxonomy_args (args:TAX_args) = 
    (String.concat["Taxonomy max_Depth with args: max_depth=\"",Int.toString (#max_depth args)," and key", Int.toString(#key args),"\n\n\n"])

fun deep_of_classifier x (Class{parent,...}) (model as (clist,alist)) = 
    (case parent of 
	NONE => x+1
      | SOME(OclAny) => x+1
      | SOME(typ) => deep_of_classifier (x+1) (class_of_type typ model) model
    )
  | deep_of_classifier x (AssociationClass{parent,...}) (model as (clist,alist)) = 
    (case parent of
	 NONE => x+1
       | SOME(OclAny) => x+1
       | SOME(typ) => deep_of_classifier (x+1) (class_of_type typ model) model
    )
  | deep_of_classifier x (Primitive {parent,...}) (model as (clist,alist)) =
    (case parent of 
	NONE => x+1
      | SOME(OclAny) => x+1
      | SOME(typ) => deep_of_classifier (x+1) (class_of_type typ model) model
    )
  | deep_of_classifier x y model = raise WFCPOG_TaxonomyError ("Only Classes can check for maxDepth.\n")



fun  check_depth_classifier depth class (model as (clist,alist)) = 
    let 
	val _ = trace wgen ("look for deep ...\n")
	val d = deep_of_classifier 0 class model
	val check = 
	    if (depth  > d) 
	    then true
	    else 
		let
		    val s1 = "SYNTAX ERROR: Taxonomy design consistency\n\n"
		    val s2 = "Classifier " ^ (string_of_path (name_of class)) ^ " has depth " ^ (Int.toString (d)) ^ ".\n"
		in
		    raise WFCPOG.WFCPOG_WFC_FailedException (s1^s2)
		end 
    in
	check
    end
    
fun check_depth wfpo (model as (clist,alist)) =
    let
	val _ = trace function_calls ("WFCPG_Taxonomy_Consistency.check_maxDepth\n")
	val cl = removeOclLibrary clist
	val classes = List.filter (fn a => (is_Class a) orelse (is_AssoClass a) orelse (is_Iface a) orelse (is_Enum a) orelse (is_Primi a)) cl
	val tax_args = WFCPOG_TAX_Data.get wfpo
	val depth = (#max_depth tax_args)
	val res = List.all (fn a => a = true) (List.map (fn a => check_depth_classifier depth a model) classes)
	val _ = trace function_calls ("WFCPG_Taxonomy_Consistency.check_maxDepth\n")
    in
	res
    end
end;
