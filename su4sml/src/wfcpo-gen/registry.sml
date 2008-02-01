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

(** 
 * This registry a provides a list of constraints which can be applied to a given UML/OCL model.
 * All the implemented constraints are accessible form this component. 
 * For the ease of use a default constraint list is provided by the signature. This constraint list 
 * can be modified to add or remove constraints form the default constraint list. New constraints 
 * can be generated by writing a new file implementing the WFCPO_CONSTRAINT signature.
 *)
signature WFCPO_REGISTRY =
sig

    
 
    val analyze_model         : string -> string -> Constraint.constraint list -> Rep_OclTerm.OclTerm list
    val supported_constraints : (Constraint.constraint list) 
    val getConstraint         : string -> Constraint.constraint list -> Constraint.constraint
    (** Any kind of exceptions *)
    exception RegistryError of string


end 
structure WFCPO_Registry (* : WFCPO_REGISTRY *) =
struct

(* SU4SML *)
open Rep_Core
open Rep
open Rep_OclTerm
open Rep_OclType
open OclLibrary

(* OclParser *)
open Ext_Library
open ModelImport

(* WFCPO *)
open WFCPO_Library
open WFCPO_Naming

(*** IMPORTED STRUCTURES ***)
(** BASE CONSTRAINTS *)
structure Con = Constraint
structure Base = Base_Constraint
structure Plugin = Plugin_Constraint

(** STATIC CONSTRAINTS *)
(* Liskov *)
structure Liskov = Liskov_Constraint(Base)
(* Data model consistency *)
structure Data = Data_Model_Consistency_Constraint(Base)
(* Constructor consistency *)
structure Constructor = Constructor_Constraint(Base)
(* Operational consistency *)
structure Operational = Operational_Constraint(Base)
(** PLUGIN CONSTRAINTS *)
(* Refinement *)
structure Refine = Refine_Constraint(Plugin)

(*** TYPES ***)
type constraint = Constraint.constraint

(*** EXCEPTIONS ***)
exception RegistryError of string
exception WFCPO_SyntaxError_ClassConsistency = Refine.WFCPO_SyntaxError_ClassConsistency
exception WFCPO_SyntaxError_OpConsistency = Refine.WFCPO_SyntaxError_OpConsistency
exception WFCPO_SyntaxError_TypeConsistency = Refine.WFCPO_SyntaxError_TypeConsistency

val supported_constraints = [
    (* Liskov Substitution Principle *)
    { name        = "liskov",
      description = "liskov substitution principle",
      generator   = Liskov.generate_po},
    { name        = "data model consistency",
      description = "data model consistency using single model consistency, class model consistency and strong model consistency.",
      generator   = Data.generate_po},
    { name        = "constructor consistency",
      description = "constructor consistency. sub1: post -> inv",
      generator   = Constructor.generate_po}]

fun getConstraint s [] = raise RegistryError ("Constraint not found.\n")
  | getConstraint s ((h:constraint)::tail) = 
    if (#name h) = s 
    then h
    else (getConstraint s tail)

fun name_po s [] = []
  | name_po s (h::tail) = 
    (Predicate (h,type_of_term h,[(s ^ Int.toString (get_po_number()))],[]))::(name_po s tail)

fun analyze_model_m [] model = []
  | analyze_model_m (({name=n,generator=gen,...}:constraint)::tail) model =
    let 
	val _ = trace zero ("analyzse a " ^ n ^ " constraint.\n")
    in
	(name_po n (gen model))@(analyze_model_m tail model)
    end

fun analyze_model xmi_file ocl_file [] = raise RegistryError ("No constraints listed.\n")
  | analyze_model xmi_file ocl_file con_list =
    let
	val _ = trace zero ("\n\n#####################################\nSTART: WFCPO  \n")
	val _ = trace zero ("#####################################\n")
	val i_model = import xmi_file ocl_file []
	val n_model = normalize_ext i_model
	val model = ((#1 n_model@oclLib),(#2 n_model))
 	val _ = trace zero ("[*] model analyzed \n")
	(* init number generator *)
	val _ = reset_po_nr()
    in
	analyze_model_m con_list model 
    end 

end;

