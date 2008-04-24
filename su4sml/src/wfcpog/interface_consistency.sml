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
signature WFCPOG_INTERFACE_CONSTRAINT =
sig
    val has_consistent_stereotypes      : WFCPOG.wfpo -> Rep.Model -> bool

    val is_nameclash_free               : WFCPOG.wfpo -> Rep.Model -> bool								      
end
structure WFCPOG_Interface_Constraint:WFCPOG_INTERFACE_CONSTRAINT =
struct 

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

exception WFCPOG_InterfaceError of string

fun list_has_dup [] = false
  | list_has_dup [x] = false
  | list_has_dup (h::tail) = 
    if List.exists (fn a => a = h) tail
    then false
    else list_has_dup (tail)


fun has_consistent_stereotypes_help [] model = true
  | has_consistent_stereotypes_help ((Interface{operations,...})::classes) (model as (clist,alist)) = 
    List.all (fn a => not (List.exists (fn b => (b = "create") orelse (b = "destroy")) (#stereotypes a))) (operations) 
  | has_consistent_stereotypes_help (h::classes) model = has_consistent_stereotypes_help classes model

fun is_nameclash_free_help [] model = true
  | is_nameclash_free_help ((Class{interfaces,...})::classes) (model as (clist,alist)) =
    if (List.length(interfaces)) <= 1
    then is_nameclash_free_help classes model
    else
	let
	    val if_list = List.map (fn a => class_of_type a model) interfaces
	    val op_name_list = List.concat (List.map (fn a => (List.map (name_of_op) (all_operations_of a model))) if_list)
	in 
	    list_has_dup (op_name_list)
	end
  | is_nameclash_free_help ((AssociationClass{interfaces,...})::classes) (model as (clist,alist)) = 
        if (List.length(interfaces)) <= 1
    then is_nameclash_free_help classes model
    else
	let
	    val if_list = List.map (fn a => class_of_type a model) interfaces
	    val op_name_list = List.concat (List.map (fn a => (List.map (name_of_op) (all_operations_of a model))) if_list)
	in 
	    list_has_dup (op_name_list)
	end
  | is_nameclash_free_help (x::classes) model = is_nameclash_free_help classes model


fun has_consistent_stereotypes wfpo (model as (clist,alist)) = 
    let
	val classes = removeOclLibrary clist
    in
	has_consistent_stereotypes_help classes model
    end

fun is_nameclash_free wfpo (model as (clist,alist)) = 
    let
	val classes = removeOclLibrary clist
    in
	is_nameclash_free_help classes model
    end


end;
