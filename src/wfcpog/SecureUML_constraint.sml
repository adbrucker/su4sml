(*****************************************************************************
 * su4sml --- an SML repository for managing (Secure)UML/OCL models
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

(** Implementation of the Liskov Substitiution Principle. *)
signature WFCPOG_SECUREUML_CONSTRAINT =
sig
    type SSD_args

    structure WFCPOG_SSD_Data:
	      sig
		  type T = SSD_args
		  val get : WFCPOG.wfpo -> T
		  val put : T -> WFCPOG.wfpo -> WFCPOG.wfpo
		  val map : (T -> T) -> WFCPOG.wfpo -> WFCPOG.wfpo
						       
	      end
(*	      
    val are_roles_mutex    : WFCPOG.wfpo -> Rep_Core.transform_model -> bool

    val rh_is_tree         : WFCPOG.wfpo -> Rep_Core.transform_model -> bool

    val executable_by_some_role : WFCPOG.wfpo -> Rep_Core.transform_model -> bool

    val authorized_for_some_action : WFCPOG.wfpo -> Rep_Core.transform_model -> bool
*)
    val separation_of_duty : WFCPOG.wfpo -> Rep.Model -> (Rep_OclType.Path * Rep_OclTerm.OclTerm) list
    val binding_of_duty    : WFCPOG.wfpo -> Rep.Model -> (Rep_OclType.Path * Rep_OclTerm.OclTerm) list

end
structure WFCPOG_SecureUML_Constraint:WFCPOG_SECUREUML_CONSTRAINT = 
struct

(* su4sml *)
open Rep_Core
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

type SSD_args = 
     {key : int,
      mutex_perm_sets: (Permission list) list
     }

structure WFCPOG_SSD_Data = WFCPOG_DataFun
	             (struct
		      type T = SSD_args;
		      val empty = ({key=11,mutex_perm_sets=[[]:Permission list]});
		      fun copy T = T;
		      fun extend T = T;
		      end);

fun ssd_generate_pos [] classes model = []
  | ssd_generate_pos perms class model = []

fun separation_of_duty_help [] classes model = []:((Path * OclTerm) list )
  | separation_of_duty_help (h::perm_sets) classes model =
    let
	val _ = Logger.info ("WFCPOG_SecureUML_Constraint.separation_of_duty_help\n")
	val x = ssd_generate_pos h classes model
	val res = x@(separation_of_duty_help perm_sets classes model)
	val _ = Logger.info ("WFCPOG_SecureUML_Constraint.separation_of_duty_help\n")
    in
	res
    end


fun binding_of_duty_help [] (cl::clist) model = []
						

(*
fun rh_is_tree wfpo (model as (clist,alist)) = 
    let
	val _ = Logger.info ("WFCPOG_SecureUML_Constraint.rh_is_tree\n")
	val cl = removeOclLibrary clist
	val classes = List.filter (fn a => (is_Role a)) cl

		 

	val _ = Logger.info ("WFCPOG_SecureUML_Constraint.rh_is_tree\n")
    in
	res
    end
*)
fun separation_of_duty wfpo (model as (clist,alist)) =  
    let 
	val _ = Logger.info ("WFCPOG_SecureUML_Constraint.separation_of_duty\n")
	val _ = Logger.debug1 ("remove OclLib ...\n")
	val cl = removeOclLibrary clist
	val _ = Logger.debug1 ("oclLib removed ...\n")
	val _ = Logger.debug1 ("Extract args ...\n")
	val ssd_args = WFCPOG_SSD_Data.get wfpo
	val perm_sets = (#mutex_perm_sets ssd_args)
	val res = separation_of_duty_help perm_sets cl model
	val _ = Logger.info ("WFCPOG_SecureUML_Constraint.separation_of_duty\n")
    in
	res
    end

fun binding_of_duty wfpo (model as (clist,alist)) = []:((Path * OclTerm) list)
(*    let 
	val _ = Logger.info ("WFCPOG_SecureUML_Constraint.binding_of_duty\n")
	val _ = Logger.debug1 ("remove OclLib ...\n")
	val cl = removeOclLibrary clist
	val res = binding_of_duty cl model
	val _ = Logger.info ("WFCPOG_SecureUML_Constraint.binding_of_duty\n")
    in
	res
    end
*)
end;
