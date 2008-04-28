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
signature WFCPOG_SECUREUML_CONSTRAINT =
sig
    type SSD_args

    structure WFCPOG_SSD_Data:
	      sig
		  type T = SSD_args
		  val get : WFCPOG_wfpo -> T
		  val put : T -> WFCPOG.wfpo -> WFCPOG.wfpo
		  val map : (T -> T) -> WFCPOG.wfpo -> WFCPOG.wfpo
	      end

    val separation_of_duty : WFCPOG.wfpo -> Rep.Model -> (Rep_OclType.Path * Rep_OclTerm.OclTerm) list
    val binding_of_duty    : WFCPOG.wfpo -> Rep.Model -> (Rep_OclType.Path * Rep_OclTerm.OclTerm) list

end
structure WFCPOG_SecureUML_Constraint:WFCPOG_SECUREUML_CONSTRAINT = 
struct

(* su4sml *)
open Rep_Core
open Rep_Logger
open Rep_OclTerm
open Rep_OclType
open Rep2String
open XMI_DataTypes

(* SecureUML *)
open Rep_SecureUML

(* oclparser *)
open ModelImport



(* wfcpo-gen *)
open WFCPOG_Library



exception WFCPO_SecureUMLError of string



type SSD_args = 
     {key : int,
      mutex_perm_sets: (Permission list) list
     }

structure WFCPOG_SSD_Data = WFCPOG_DataFun
	             (struct
		      type T = SSD_args;
		      val empty = ({key=11,rfm_tuples=[[]:Permission list];
		      fun copy T = T;
		      fun extend T = T;
		      end);

fun separation_of_duty_help (cl::clist) model = []
	
fun binding_of_duty_help (cl::clist) model = []




fun separation_of_duty wfpo model =  
    let 
	val _ = trace function_call ("WFCPOG_SecureUML_Constraint.separation_of_duty\n")
	val _ = trace wgen ("remove OclLib ...\n")
	val cl = removeOclLibrary clist
	val _ = trace wgen ("oclLib removed ...\n")
	val _ = trace wgen ("Extract args ...\n")
	val ssd_args = WFCPOG_SSD_Data.get wfpo
	val res = separation_of_duty_help cl model
	val _ = trace function_ends ("WFCPOG_SecureUML_Constraint.separation_of_duty\n")
    in
	res
    end

fun binding_of_duty wfpo model = 
    let 
	val _ = trace function_call ("WFCPOG_SecureUML_Constraint.binding_of_duty\n")
	val _ = trace wgen ("remove OclLib ...\n")
	val cl = removeOclLibrary clist
	val res = binding_of_duty cl model
	val _ = trace function_ends ("WFCPOG_SecureUML_Constraint.binding_of_duty\n")
    in
	res
    end

end;
