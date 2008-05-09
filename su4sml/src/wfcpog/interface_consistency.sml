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
    (**
     * Checks if the operations of the interface do not have the
     * stereotypyes 'create' or 'destroy'.
     *)
    val check_stereotypes      : WFCPOG.wfpo -> Rep.Model -> bool
    (**
     * Checks that a classifier implementing more then one interface
     * has no nameclashes.
     *)
    val check_nameclashes      : WFCPOG.wfpo -> Rep.Model -> bool								      
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


fun check_stereotypes_interface (i as Interface{operations,...}) (model as (clist,alist)) = 
    if (List.all (fn a => not (List.exists (fn b => (b = "create") orelse (b = "destroy")) (#stereotypes a))) (operations))
    then true
    else
	let
	    val s1 = "SYNTAX ERROR: Interface stereotypes consistency\n\n"
	    val s2 = "Interface " ^ (string_of_path (name_of i)) ^ " has a operations with stereotypes 'create' or 'destroy' \n"
	in
	    raise WFCPOG.WFC_FailedMessage (s1^s2)
	end
  | check_stereotypes_interface  x model = true


fun check_nameclash_classifier (c as Class{interfaces,...}) (model as (clist,alist)) =
    if (List.length(interfaces)) <= 1
    then true
    else
	let
	    val if_list = List.map (fn a => class_of_type a model) interfaces
	    val op_name_list = List.concat (List.map (fn a => (List.map (name_of_op) (all_operations_of a model))) if_list)
	in 
	    if (not (list_has_dup (op_name_list)))
	    then true
	    else
		let
		    val s1 = "SYNTAX ERROR: Interface nameclash consistency\n\n"
		    val s2 = "Classifier " ^ (string_of_path (name_of c)) ^ " has a nameclash resulting from the interfaces " ^ (String.concat (List.map (fn a => (string_of_OclType a)) interfaces)) ^ ".\n"
		in
		    raise WFCPOG.WFC_FailedMessage (s1^s2)
		end
	end
  | check_nameclash_classifier (a as AssociationClass{interfaces,...}) (model as (clist,alist)) = 
    if (List.length(interfaces)) <= 1
    then true
    else
	let
	    val if_list = List.map (fn a => class_of_type a model) interfaces
	    val op_name_list = List.concat (List.map (fn a => (List.map (name_of_op) (all_operations_of a model))) if_list)
	in 
	    if (not (list_has_dup (op_name_list)))
	    then true
	    else
		let
		    val s1 = "SYNTAX ERROR: Interface nameclash consistency\n\n"
		    val s2 = "Classifier " ^ (string_of_path (name_of a)) ^ " has a nameclash resulting from the interfaces " ^ (String.concat (List.map (fn a => (string_of_OclType a)) interfaces)) ^ ".\n"
		in
		    raise WFCPOG.WFC_FailedMessage (s1^s2)
		end
	end
  | check_nameclash_classifier (e as Enumeration{interfaces,...}) model = 
    if (List.length(interfaces)) <= 1
    then true
    else
	let
	    val if_list = List.map (fn a => class_of_type a model) interfaces
	    val op_name_list = List.concat (List.map (fn a => (List.map (name_of_op) (all_operations_of a model))) if_list)
	in 
	    if (not (list_has_dup (op_name_list)))
	    then true
	    else
		let
		    val s1 = "SYNTAX ERROR: Interface nameclash consistency\n\n"
		    val s2 = "Classifier " ^ (string_of_path (name_of e)) ^ " has a nameclash resulting from the interfaces " ^ (String.concat (List.map (fn a => (string_of_OclType a)) interfaces)) ^ ".\n"
		in
		    raise WFCPOG.WFC_FailedMessage (s1^s2)
		end
	end
  | check_nameclash_classifier (p as Primitive{interfaces,...}) model = 
    if (List.length(interfaces)) <= 1
    then true
    else
	let
	    val if_list = List.map (fn a => class_of_type a model) interfaces
	    val op_name_list = List.concat (List.map (fn a => (List.map (name_of_op) (all_operations_of a model))) if_list)
	in 
	    if (not (list_has_dup (op_name_list)))
	    then true
	    else
		let
		    val s1 = "SYNTAX ERROR: Interface nameclash consistency\n\n"
		    val s2 = "Classifier " ^ (string_of_path (name_of p)) ^ " has a nameclash resulting from the interfaces " ^ (String.concat (List.map (fn a => string_of_OclType a) interfaces)) ^ ".\n"
		in
		    raise WFCPOG.WFC_FailedMessage (s1^s2)
		end
	end
  | check_nameclash_classifier x model = true

fun check_stereotypes wfpo (model as (clist,alist)) = 
    let
	val _ = trace function_calls ("WFCPOG_Interface_Consistency.check_stereotypes\n")
	val cl = removeOclLibrary clist
	val classes = List.filter (fn a => (is_Iface a)) cl
	val res = List.all (fn a => a = true) (List.map (fn a => check_stereotypes_interface a model
							    handle WFCPOG.WFC_FailedMessage s => raise WFCPOG.WFC_FailedException (wfpo,s)) classes)
	val _ = trace function_ends ("WFCPOG_Interface_Consistency.check_stereotypes\n")
    in
	res
    end

fun check_nameclashes wfpo (model as (clist,alist)) = 
    let
	val _ = trace function_calls ("WFCPOG_Interface_Consistency.check_nameclashes\n")
	val cl = removeOclLibrary clist
	val classes = List.filter (fn a => (is_Class a) orelse (is_AssoClass a) orelse (is_Primi a) orelse (is_Enum a)) cl
	val res = List.all (fn a => a = true) (List.map (fn a => check_nameclash_classifier a model
							    handle WFCPOG.WFC_FailedMessage s => raise WFCPOG.WFC_FailedException (wfpo,s)) classes)
	val _ = trace function_ends ("WFCPOG_Interface_Consistency.check_nameclashes\n")
    in
	res

    end
end;
