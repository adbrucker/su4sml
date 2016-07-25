(*****************************************************************************
 * su4sml --- a SML repository for managing (Secure)UML/OCL models
 *             http://projects.brucker.ch/su4sml/
 *                                                                            
 * context_declarations.sml --- 
 * This file is part of su4sml.
 *
 * Copyright (c) 2005-2007 ETH Zurich, Switzerland
 *               2008-2009 Achim D. Brucker, Germany
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
signature WFCPOG_OPERATIONAL_CONSTRAINT = 
sig
    val implementable_operation : WFCPOG.wfpo -> Rep.Model -> (Rep_OclType.Path * Rep_OclTerm.OclTerm) list
end
structure WFCPOG_Operational_Constraint : WFCPOG_OPERATIONAL_CONSTRAINT = 
struct


(* SU4SML *)
open Rep_Core
open Rep
open Rep_OclTerm
open Rep_OclType
open Rep_HolOcl_Namespace
(* OclParser *)
open ModelImport

(* WFCPO *)
open WFCPOG_Library

exception WFCPO_OperationalError
	

fun impl_op_operation class oper = 
    let
	val _ = Logger.info ("WFCPOG_Operational_Consistency.prestate_complete_operation\n")
	(* Generate Variables : (sigma_pre,sigma_post) => sigma *)
	val sigma = Literal("sigma",OclState)
	val sigma_s = Literal("sigma_s",OclState)
	val tuple_term = Tuple [("sigma",sigma,OclState),("sigma_s",sigma_s,OclState)]
	(* local valid post *)
	val pre_of_op = Predicate(Variable("self",type_of class),type_of class,name_of_pre class oper,args2varargs (arguments_of_op oper))
	val post_of_op = Predicate(Variable("self",type_of class),type_of class,name_of_post class oper,args2varargs ((arguments_of_op oper)@[("result",DummyT)]))
	val lv_post = OperationCall(post_of_op,Boolean,["holOclLib","Boolean","OclLocalValid"],[(tuple_term,OclState)],Boolean)
	val impl = OperationCall (pre_of_op,Boolean,["holOclLib","Boolean","implies"],[(lv_post,Boolean)],Boolean)
	val lv_state = OperationCall(impl,Boolean,["holOclLib","Boolean","OclLocalValid"],[(tuple_term,OclState)],Boolean)
	val holocl_exists = Iterator("holOclLib.exists",[("sigma_s",OclState)],Literal("",DummyT),DummyT,lv_state,Boolean,Boolean)
	val holocl_forall = Iterator("holOclLib.forAll",[("sigma",OclState)],Literal("",DummyT),DummyT,holocl_exists,Boolean,Boolean)
	val _ = Logger.info ("WFCPOG_Operational_Consistency.prestate_complete_operaiton\n")
    in
	holocl_forall
    end

fun impl_op_classifier class (model as (clist,alist)) = 
    let
	val _ = Logger.info ("WFCPOG_Operational_Consistency.prestate_complete_classifier\n")
	val ops = all_operations_of class model
	val res = (List.map (fn a => ((["po_class_model_"]@(name_of class)@["_"]@[(name_of_op a)]),(impl_op_operation class a))) ops)
	val _ = Logger.info ("WFCPOG_Operational_Consistency.prestate_complete_classifier\n")
    in
	res
    end

fun implementable_operation wfc_sel (model as (clist,alist)) = 
    let
	val _ = Logger.info ("WFCPOG_Operational_Consistency.prestate_complete\n")
	val cl = removeOclLibrary clist
	val res = List.concat (List.map (fn a => impl_op_classifier a model) cl )
	val _ = Logger.info ("WFCPOG_Operational_Consistency.prestate_complete\n")
    in
	res
    end
end;
