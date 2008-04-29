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
signature WFCPOG_DATA_MODEL_CONSISTENCY_CONSTRAINT = 
sig
    (** sub constraint, included in liskov.*)
    val class_model_consistency        : WFCPOG.wfpo -> Rep.Model -> (Rep_OclType.Path * Rep_OclTerm.OclTerm) list
    (** sub constraint, included in liskov.*)
    val strong_model_consistency       : WFCPOG.wfpo -> Rep.Model -> (Rep_OclType.Path * Rep_OclTerm.OclTerm) list
end
structure WFCPOG_Data_Model_Consistency_Constraint : WFCPOG_DATA_MODEL_CONSISTENCY_CONSTRAINT = 
struct

(* su4sml *)
open Rep_Logger
open Rep_Core
open Rep_OclTerm
open Rep_OclType
open ModelImport
open Rep2String
open HolOcl_Namespace
(* ocl-parser *)
open ModelImport
open Context

(* wfcpo-gen *)
open WFCPOG_Library


exception WFCPO_DataModelError


fun c_allInstance_term (c:Classifier) =
    let 
	val _ = trace function_calls ("WF_data_CS.c_allInstances\n")
	val uni_name_x = HolOcl_Namespace.gen_unique_string()
	val x = Variable (uni_name_x,DummyT)
        (* get class as an holocl term *)
	val holocl_class_name = get_class_of (name_of c)
	val class_type = type_of c
	val uni_name_c = HolOcl_Namespace.gen_unique_string()
	val class = Variable(uni_name_c,class_type)
	(* OclAny.allInstances() *)
	val allInstances = OperationCall (class,class_type,["oclLib","OclAny","allInstances"],[],Set (class_type))
	(* x.oclIsTypeOf(c) *)
	val oclIsTypeOf = OperationWithType (x,DummyT,"oclIsTypeOf",class_type,Boolean)
	(* Iterator exists *)
	val exists = Iterator("exists",[(uni_name_x,DummyT)],allInstances,Set(class_type),oclIsTypeOf,Boolean,Boolean)
	val _ = trace function_ends ("WF_data_CS.c_allInstances\n")
    in
	exists
    end

(* E t. t |= c::allInstances()->exists(x|x.oclIsTypeOf(c)) *)
fun single_model_consistency (c:Classifier) (model as (clist,alist)) = 
    let
	val _ = trace function_calls("WFCPOG_Data_Model_Consistency_Constraint.single_model_consistency\n")
	val term = c_allInstance_term c
	val local_valid = OperationCall(term,Boolean,["holOclLib","Boolean","OclLocalValid"],[(Literal("\\<tau>",OclState),DummyT)],Boolean)
	val dummy_source = Literal("",DummyT)
	val res = Iterator("holOclLib.exists",[("\\<tau>",OclState)],dummy_source,DummyT,local_valid,Boolean,Boolean)
	val _ = trace function_ends("WFCPOG_Data_Model_Consistency_Constraint.single_model_consistency\n")
    in
	res
    end


fun class_model_consistency_help [] (model as (clist,alist)) = []
  | class_model_consistency_help (h::classes) (model as (clist,alist)) =
    (["po_class_model_"]@(name_of h),single_model_consistency h model)::(class_model_consistency_help classes model)

fun class_model_consistency wfpo (model as (clist,alist)) = 
    let
	val classifiers = removeOclLibrary (clist)
    in
	class_model_consistency_help classifiers model
    end

fun strong_model_consistency_help classes model = 
    let 
	val _ = trace function_calls("WFCPOG_Data_Model_Consistency_Constraint.strong_model_consistency\n")
	val terms = List.map (c_allInstance_term) classes
	val local_valids = List.map (fn a => OperationCall(a,Boolean,["holOclLib","Boolean","OclLocalValid"],[(Literal("\\<tau>",OclState),DummyT)],Boolean)) terms
	val con_term = conjugate_terms local_valids
	val dummy_source = Literal("",DummyT)
	val res = Iterator("holOclLib.exists",[("\\<tau>",OclState)],dummy_source,DummyT,con_term,Boolean,Boolean)
	val _ = trace function_ends("WFCPOG_Data_Model_Consistency_Constraint.strong_model_consistency\n")
    in
	[(["po_strong_model"],res)]
    end

fun strong_model_consistency wfpo (model as (clist,alist)) = 
    let
	val classifiers = removeOclLibrary (clist)
    in
	strong_model_consistency_help classifiers model
    end
end;
