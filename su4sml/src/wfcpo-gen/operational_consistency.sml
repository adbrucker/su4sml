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
signature OPERATIONAL_CONSTRAINT = 
sig
    include BASE_CONSTRAINT
    (** sub constraint, included in operational consistency.*)
    val generate_class_constraints          : Rep.Model -> Rep_OclTerm.OclTerm list
    (** sub constraint, included in operational consistency.*)
    val generate_operation_constraints      : Rep.Model -> Rep_OclTerm.OclTerm list
    (** sub constraint, included in operational consistency.*)
    val generate_attribute_constraints      : Rep.Model -> Rep_OclTerm.OclTerm list

end
functor Operational_Constraint (SuperCon : BASE_CONSTRAINT) : OPERATIONAL_CONSTRAINT =
struct


(* SU4SML *)
open Rep_Core
open Rep
open Rep_OclTerm
open Rep_OclType

(* OclParser *)
open Ext_Library
open ModelImport

(* WFCPO *)
open WFCPO_Naming
open WFCPO_Library

exception ConstraintError of string
exception BaseConstraintError of string
exception ConstructorError of string

type constraint = Constraint.constraint



(*
fun generate_class_constraints_single classe (model as (clist,alist)) = 
    (* [Variable("s",DummyT)]  *)
    let
	val creators = creation_operations_of 
	val x = List.map (fn a => 
    in
	
    end
*)
fun generate_operation_constraints_help classes (model as (clist,alist)) = [Variable("s",DummyT)]

fun generate_attribute_constraints_help classes (model as (clist,alist)) = [Variable("s",DummyT)] 

(** 
    tink about which classes to filter.
    Maybe the classes from the SecureUML metamodel 
    must be deletet to.
 **)
fun generate_class_constraints (model as (clist,alist)) = 
    let
	val classes = removeOclLibrary clist
    in
	generate_class_constraints_help classes model
    end

fun generate_operation_constraints (model as (clist,alist)) = 
    let
	val classes = removeOclLibrary clist
    in
	generate_operation_constraints_help classes model
    end

fun generate_attribute_constraints (model as (clist,alist)) = 
    let
	val classes = removeOclLibrary clist
    in
	generate_attribute_constraints_help classes model
    end



fun generate_po (model as (clist,alist)) = (* [Variable("s",DummyT)] *)
    let
(*	val po1 = generate_class_constraints model
	val po2 = generate_operation_constraints model
	val po3 = generate_attribute_constraints model
 *)   in
	[] (* po1@po2@po3 *)
    end
end;
