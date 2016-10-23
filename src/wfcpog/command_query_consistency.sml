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
signature WFCPOG_COMMAND_QUERY_CONSTRAINT =
sig
    (**
     * All OCL-formulas should only contain operations with are
     * side-effect free. *)
    val strong_is_query          : WFCPOG.wfpo -> Rep.Model -> bool
    (**
     * All operations declared to be side-effect free should only contain
     * OCL-formulas which are side-effect free.
     *)
    val weak_is_query            : WFCPOG.wfpo -> Rep.Model -> bool
    (**
     * ?
     *)					
(*     val modified_only            : WFCPOG.wfpo -> Rep.Model -> (Rep_OclType.Path * Rep_OclTerm.OclTerm) list *)
end
structure WFCPOG_Command_Query_Constraint:WFCPOG_COMMAND_QUERY_CONSTRAINT = 
struct

(* su4sml *)
open Rep_Core
open Rep_OclTerm
open Rep_OclType
open Rep2String
open Ocl2String
(* oclparser *)
open ModelImport

(* wfcpo-gen *)
open WFCPOG_Library

exception WFCPO_QueryCommandError of string



fun check_weak_classifier class (model as (clist,alist)) =
    let
	val ops = query_operations_of class model
	val op_posts = List.map (fn a => (a,postcondition_of_op a)) ops
	val op_pres = List.map (fn a => (a,precondition_of_op a)) ops  
	val check_pres = List.map (fn (oper,pres) => 
				      (List.all (fn (a,b) =>  
						   if (side_effect_free b model)
						   then true
						   else
						       let
							   val s1 = "WFC ERROR: Weak Command/Query constraint\n\n"
							   val s2 = "Classifier " ^ (string_of_path (name_of class)) ^ " has in the operatin "^(name_of_op oper)^" the precondition " ^ (opt2string a) ^ " with the term "^(ocl2string false b)^" a call to an operation which is not isQuery.\n"
						       in
							   raise WFCPOG.WFC_FailedMessage (s1^s2)
						       end
					       ) pres)
				  ) op_pres
	val check_posts = List.map (fn (oper,posts) =>
				        (List.all (fn (a,b) =>  
						    if (side_effect_free b model)
						    then true
						    else
							let
							    val s1 = "WFC ERROR: Weak Command/Query constraint\n\n"
							    val s2 = "Classifier " ^ (string_of_path (name_of class)) ^ " has in the operation "^(name_of_op oper)^" postcondition " ^ (opt2string a) ^ " with the term "^(ocl2string false b)^" a call to an operation which is not isQuery.\n"
							in
							    raise WFCPOG.WFC_FailedMessage (s1^s2)
							end
						) posts)
				   ) op_posts
    in
	List.all (fn a => a = true) (check_pres@check_posts)
    end
    

fun check_strong_classifier class (model as (clist,alist)) =
    let
	val ops = local_operations_of class
	val invs = local_invariants_of class
	val op_posts = List.map (fn a => (a,postcondition_of_op a)) ops
	val op_pres = List.map (fn a => (a,precondition_of_op a)) ops  
	val check_pres = List.map (fn (oper,pres) =>
				      (List.all (fn (a,b) => 
						   if (side_effect_free b model)
						   then true
						   else
						       let
							   val s1 = "WFC ERROR: Strong Command/Query constraint\n\n"
							   val s2 = "Classifier " ^ (string_of_path (name_of class)) ^ " has in the operation "^(name_of_op oper)^" precondition " ^ (opt2string a) ^ " with the term "^(ocl2string false b)^" a call to an operation which is not isQuery.\n"
						       in
							   raise WFCPOG.WFC_FailedMessage (s1^s2)
						       end
					       ) pres)
				  ) op_pres
	val check_posts = List.map (fn (oper,posts) => 
				       (List.all (fn (a,b) => 
						    if (side_effect_free b model)
						    then true
						    else
							let
							    val s1 = "WFC ERROR: Strong Command/Query constraint\n\n"
							    val s2 = "Classifier " ^ (string_of_path (name_of class)) ^ " has in the operation "^(name_of_op oper)^" postcondition " ^ (opt2string a) ^ " with the term "^(ocl2string false b)^" a call to an operation which is not isQuery.\n"
							in
							    raise WFCPOG.WFC_FailedMessage (s1^s2)
							end
						) posts)
				   ) op_posts
	val check_invs = List.all (fn (a,b) =>
				      if (side_effect_free b model)
				      then true
				      else
					  let
					      val s1 = "WFC ERROR: Strong Command/Query constraint\n\n"
					      val s2 = "Classifier " ^ (string_of_path (name_of class)) ^ " has in the invariant " ^ (opt2string a) ^ " with the term "^(ocl2string false b)^" a call to an operation which is not isQuery.\n"
					  in
					      raise WFCPOG.WFC_FailedMessage (s1^s2)
					  end
				  ) invs
    in
	List.all (fn a => a = true) (check_pres@check_posts@[check_invs])
    end

fun weak_is_query po (model as (clist,alist)) =
    let
	val _ = Logger.info ("WFCPOG_Command_Query_Constraint.strong_is_query\n")
	val classes = removeOclLibrary clist
	val res = List.all (fn a => a = true) (List.map (fn a => check_weak_classifier a model
								 handle WFCPOG.WFC_FailedMessage s => raise WFCPOG.WFC_FailedException(po,s)) classes)
	val _ = Logger.info ("WFCPOG_Command_Query_Constraint.strong_is_query\n")
    in
	res
    end



fun strong_is_query po (model as (clist,alist)) = 
    let
	val _ = Logger.info ("WFCPOG_Command_Query_Constraint.strong_is_query\n")
	val classes = removeOclLibrary clist
	val res = List.all (fn a => a = true) (List.map (fn a => check_strong_classifier a model
								 handle WFCPOG.WFC_FailedMessage s => raise WFCPOG.WFC_FailedException(po,s)) classes)
	val _ = Logger.info ("WFCPOG_Command_Query_Constraint.strong_is_query\n")
    in
	res
    end

 (* | OperationCall (src,styp,["oclLib",_,"modifiedOnly"],[],_) 
	=> OclModifiedOnly styp u (ocl2holocl u  src)             *)
end;
    
