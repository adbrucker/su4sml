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
signature TAXONOMY_CONSTRAINT =
sig 
    type TAX_args      
    structure TAX_Data :
	      sig
		  type T = TAX_args
		  val get : WFCPOG.wfpo -> T
		  val put : T -> WFCPOG.wfpo -> WFCPOG.wfpo
		  val map : (T -> T) -> WFCPOG.wfpo -> WFCPOG.wfpo
	      end
    
    val print_taxonomy_args : TAX_args -> unit

    (** Subconstraint *)
    val has_maxDepth   : WFCPOG.wfpo -> Rep.Model -> bool	   

    exception WFCPOG_TaxonomyError of string
end

structure Taxonomy_Constraint:TAXONOMY_CONSTRAINT = 
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
open WFCPO_Naming
open Datatab

exception WFCPOG_TaxonomyError of string

type TAX_args = {
     key : int,
     max_depth : int
}

structure TAX_Data = WfpoDataFun
(struct
 val name =" tax";
 type T = TAX_args;
 val empty = ({key=0,max_depth=0});
 fun copy T = T;
 fun extend T = T;
end);

 
fun print_taxonomy_args (args:TAX_args) = 
    print (concat["Taxonomy max_Depth with args: max_depth=\"",Int.toString (#max_depth args)," and key", Int.toString(#key args),"\n\n\n"])

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


fun has_maxDepth_help depth [] model = true
  | has_maxDepth_help depth (h::classes) (model as (clist,alist)) = 
    let 
	val _ = trace 50 ("look for deep ...\n")
	val d = deep_of_classifier 0 h model
	val _ = trace 50 ("deep of classifier " ^ (String.concat (Rep_Core.name_of h)) ^ " = " ^ (Int.toString d) ^ "\n")
    in
	if (d > depth) 
	then false
	else has_maxDepth_help depth classes model
    end
    
fun has_maxDepth wfpo (model as (clist,alist)) =
    let
	val _ = trace 50 ("remove oclLib ...\n")
	val classes = removeOclLibrary clist
	val _ = trace 50 ("oclLib removed ...\n")
	val tax_args = TAX_Data.get wfpo
	val _ = trace 50 ("args extracted ...\n")
	val depth = (#max_depth tax_args)
	val _ = trace 50 ("depth = " ^ (Int.toString (depth)) ^ "\n")
    in
	has_maxDepth_help depth classes model 
    end
end;
