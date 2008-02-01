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
signature DATA_MODEL_CONSISTENCY_CONSTRAINT = 
sig
    include BASE_CONSTRAINT
    
    val class_model_consistency        : Rep.Model -> Rep_OclTerm.OclTerm
    val strong_model_consistency       : Rep.Model -> Rep_OclTerm.OclTerm

end
functor Data_Model_Consistency_Constraint (SuperCon : BASE_CONSTRAINT) : BASE_CONSTRAINT =
struct

(* su4sml *)
open Rep_Core
open Rep_OclTerm
open Rep_OclType
open Ext_Library
open ModelImport
open Rep2String

(* ocl-parser *)
open ModelImport
open Context

(* wfcpo-gen *)
open WFCPO_Library

exception ConstraintError of string
exception BaseConstraintError of string
exception LiskovError of string

type constraint = SuperCon.constraint

fun c_allInstance_term (c:Classifier) =
    let
	(* create terms form right to left *)
	val x = Variable("x",DummyT)
	val oclIsTypeOf = OperationWithType (x,DummyT,"oclIsTypeOf",type_of c,Boolean)
	val exists = Iterator("exists",[("x",DummyT)],Literal("dummy_source",DummyT),DummyT,oclIsTypeOf,Boolean,Boolean)
	val allInstances = OperationCall (Literal("dummy_source",DummyT),DummyT,["oclLib","OclAny","allInstances"],[],Boolean)
	val class = Literal("c",type_of c)
	(* nest sources *)
	val term = add_source (class,allInstances)
	val term = add_source (allInstances,exists)
    in
	OperationCall (term,DummyT,["holOclLib","Boolean","OclLocalValid"],[],Boolean)
    end

(* E t. t |= c::allInstances()->exists(x|x.oclIsTypeOf(c)) *)
fun single_model_consistency (c:Classifier) (model as (clist,alist)) = 
    let
	val term = c_allInstance_term c
    in
	OperationCall (Variable("tau",DummyT),DummyT,["holOclLib","Quantor","existence"],[(term,type_of_term term)],Boolean)
    end


fun class_model_consistency_help [] (model as (clist,alist)) = []
  | class_model_consistency_help (h::classes) (model as (clist,alist)) =
    (single_model_consistency h model)::(class_model_consistency_help classes model)

fun class_model_consistency (model as (clist,alist)) = 
    let
	val classifiers = removeOclLibrary (clist)
    in
	class_model_consistency_help classifiers model
    end

fun strong_model_consistency_help classes model = 
    let 
	val terms = List.map (c_allInstance_term) classes
	val n_term = nest_source terms
    in
	OperationCall (Variable ("tau",DummyT),DummyT,["holOclLib","Quantor","existence"],[(n_term,type_of_term n_term)],Boolean)
    end

fun strong_model_consistency (model as (clist,alist)) = 
    let
	val classifiers = removeOclLibrary (clist)
    in
	strong_model_consistency_help classifiers model
    end

fun generate_po model = 
    let
	val po2 = class_model_consistency model
	val po3 = strong_model_consistency model
    in
	po2@[po3]
    end

end;
