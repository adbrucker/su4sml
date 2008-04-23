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
signature WFCPOG_COMMAND_QUERY_CONSTRAINT =
sig
    val ops_are_query            : WFCPOG.wfpo -> Rep.Model -> (string * Rep_OclTerm.OclTerm) list
						
    val ops_are_command          : WFCPOG.wfpo -> Rep.Model -> (string * Rep_OclTerm.OclTerm) list
end
structure WFCPOG_Command_Query_Constraint:WFCPOG_COMMAND_QUERY_CONSTRAINT = 
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

exception WFCPO_QueryCommandError of string



fun post_implies_args_and_self_at_pre oper class = 
    let
	val posts = List.map (fn (a,b) => b) (postcondition_of_op oper)
	val fst = conjugate_terms posts
	val args = arguments_of_op oper
	val args2term = List.map (fn (a,b) => Variable(a,b)) args
	val arg_terms = List.map (fn a => 
				     let
					 val ta = type_of_term a 
					 val x = OperationCall (a,ta,["oclLib","OclAny","atPre"],[],ta) 
				     in
					 OperationCall (a,ta,["oclLib","Boolean","="],[(x,type_of_term x)],Boolean)
				     end
				 ) args2term
	val selft = type_of class
	val self = Variable("self",selft)
	val self_term_help = OperationCall (self,selft,["oclLib","OclAny","atPre"],[],selft)
	val self_term = OperationCall (self,selft,["oclLib","OclAny","="],[(self_term_help,type_of_term self_term_help)],Boolean) 
	val snd = conjugate_terms (self_term::arg_terms)
    in
	OperationCall(fst,Boolean,["oclLib","Boolean","implies"],[(snd,Boolean)],Boolean)
    end

fun post_implies_not_args_or_not_self_at_pre oper class =
    let
	val posts = List.map (fn (a,b) => b) (postcondition_of_op oper)
	val fst = conjugate_terms posts
	val args = arguments_of_op oper
	val args2term = List.map (fn (a,b) => Variable(a,b)) args
	val arg_terms = List.map (fn a => 
				     let
					 val ta = type_of_term a 
					 val x = OperationCall (a,ta,["oclLib","OclAny","atPre"],[],ta) 
				     in
					 OperationCall (a,ta,["oclLib","OclAny","<>"],[(x,type_of_term x)],Boolean)
				     end
				 ) args2term 
	val selft = type_of class
	val self = Variable("self",selft)
	val self_term_help = OperationCall (self,selft,["oclLib","OclAny","atPre"],[],selft)
	val self_term = OperationCall (self,selft,["oclLib","OclAny","<>"],[(self_term_help,type_of_term self_term_help)],Boolean) 
	val snd = disjugate_terms (self_term::arg_terms)
    in
	OperationCall(fst,Boolean,["oclLib","Boolean","implies"],[(snd,Boolean)],Boolean)
    end

fun ops_are_query_help [] model = []
  | ops_are_query_help (h::classes) (model as (clist,alist)) = 
    let
	val qops = query_operations_of h model
	val x = List.map (fn a => (("quy_"^(string_of_path(name_of h))),post_implies_args_and_self_at_pre a h)) qops
    in
	(x)@(ops_are_query_help classes model)
    end

fun ops_are_command_help [] model = []
  | ops_are_command_help (h::classes) (model as (clist,alist)) =  
    let
	val cops = command_operations_of h model
	val x = List.map (fn a => (("cmd_"^(string_of_path (name_of h))),post_implies_not_args_or_not_self_at_pre a h)) cops 
    in
	(x)@(ops_are_command_help classes model)
    end

fun ops_are_query wfpo (model:Rep.Model as (clist,alist)) = 
    let
	val class = removeOclLibrary clist
    in
	ops_are_query_help class model
    end

fun ops_are_command wfpo (model:Rep.Model as (clist,alist)) =
    let
	val class = removeOclLibrary clist
    in

	ops_are_command_help class model
    end
end;
