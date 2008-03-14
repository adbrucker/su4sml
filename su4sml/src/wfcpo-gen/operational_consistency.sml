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
    (** sub constraint, included in operational consistency.*)
    val generate_secureUML_creators_po      : Rep_Core.Classifier -> Rep.Model -> Rep_OclTerm.OclTerm list
    (** sub constraint, included in operational consistency.*)
    val generate_secureUML_destructors_po   : Rep_Core.Classifier -> Rep.Model -> Rep_OclTerm.OclTerm list
    (** sub constraint, included in operational consistency.*)
    val generate_secureUML_getters_po       : Rep_Core.Classifier -> Rep.Model -> Rep_OclTerm.OclTerm list
    val generate_secureUML_setters_po       : Rep_Core.Classifier -> Rep.Model -> Rep_OclTerm.OclTerm list
    val generate_secureUML_op_sec_po        : Rep_Core.Classifier -> Rep.Model -> Rep_OclTerm.OclTerm list
    val generate_pos                        : WFCPOG.wfpo -> Rep.Model -> Rep_OclTerm.OclTerm list

end
structure Operational_Constraint : OPERATIONAL_CONSTRAINT = 
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
open WFCPOG_Library

exception WFCPO_OperationalError
	
fun case_insensitive "" = ""
  | case_insensitive s:string = 
    let
	fun to_lower [x] = [Char.toLower x]
	  | to_lower (h::tail) = (Char.toLower h)::(to_lower tail)
    in 
	String.implode (to_lower (String.explode s))
    end


(* get corresponding operation x from x_sec of. *) 
fun secureUML_real_op oper class model = 
    let
	val name = name_of_op oper
	val len = String.size name
	val substr = String.substring (name,0,len-4)
    in
	get_operation substr class model 
    end

fun secureUML_sec_op oper class model = 
    let
	val name = name_of_op oper ^ "_sec"
    in
	get_operation name class model 
    end

fun secureUML_get_att_name oper = 
    let
	val name = name_of_op oper
	val len = String.size name
    in
	String.substring (name,0,len-4)
    end

(* *)
fun secureUML_getter_operations_of class_n model = List.filter (fn a => String.isPrefix "get" (name_of_op a)) (local_operations_of class_n model)
(* *)
fun secureUML_setter_operations_of class_n model = List.filter (fn a => String.isPrefix "set" (name_of_op a)) (local_operations_of class_n model)
(* *)
fun secureUML_op_sec_operations_of class_n model = List.filter (fn a => String.isSuffix "_sec" (name_of_op a)) (local_operations_of class_n model)

fun secureUML_substitute_terms (OperationCall(src,typ,path,args,rtyp)) class = 
    let
	val op_name = List.last path
	val p_len = List.length path
	val p = List.take (path,p_len-1)
    in    
	if (name_of class) = p
	then (* substitute operation x with x_sec *)
	    (OperationCall(src,typ,p@[op_name^"_sec"],args,rtyp))
	else OperationCall(src,typ,path,args,rtyp)
    end
  | secureUML_substitute_terms (AttributeCall(src,typ,path,rtyp)) class =
    let
	val att_name = List.last path
	val p_len = List.length path
	val p = List.take (path,p_len-1)
    in
	if (name_of class) = p
	then (* substitute attribute x with operation getX() *)
	    OperationCall(src,typ,p@["get"^att_name],[],rtyp)
	else AttributeCall(src,typ,path,rtyp)
    end
  | secureUML_substitute_terms x class = x

fun generate_secureUML_creators_po class (model as (clist,alist)) = 
    let
		val creators = creation_operations_of (name_of class) model 
    in
	(List.map (fn a => 
		      let
			  val op_res = result_of_op a
			  val result = Variable("result",op_res)
			  val term1 = OperationCall(result,op_res,(name_of class)@["oclIsUndefined"],[],Boolean)
			  val term2 = OperationCall(result,op_res,(name_of class)@["modifiedOnly"],[],Boolean)
		      in 
			  OperationCall(term1,Boolean,["oclLib","Boolean","and"],[(term2,Boolean)],Boolean) 
		      end
		  ) creators)
    end

fun generate_secureUML_destructors_po class (model as (clist,alist)) = 
    let
	val destructors = destruction_operations_of (name_of class) model
    in
	(List.map (fn a => 
		      let
			  val self = Variable("self",type_of class)
			  val term1 = OperationCall(self,type_of class,(name_of class)@["oclIsUndefined"],[],Boolean)
			  val atPre = OperationCall(self,type_of class,["oclLib","OclAny","atPre"],[],type_of class)
			  val term2 = OperationCall(atPre,type_of class,(name_of class)@["modifiedOnly"],[],Boolean)
		      in
			  OperationCall(term1,Boolean,["oclLib","Boolean","and"],[(term2,Boolean)],Boolean) 
		      end
		  ) destructors)
    end
	
fun generate_secureUML_getters_po class (model as (clist,alist)) = 
    let
	val getter = secureUML_getter_operations_of (name_of class) model
    in
	(List.map (fn a =>
		      let
			  val op_res = result_of_op a
			  val att_name = secureUML_get_att_name a
			  val result = Variable("result",op_res)
			  val self = Variable("self",type_of class)
			  val atCall = AttributeCall(self,type_of class,(name_of class)@[(att_name)],op_res)
		      in
			  OperationCall(result,op_res,["oclLib","Boolean","="],[(atCall,op_res)],Boolean)
		      end
		  ) getter)
    end

fun generate_secureUML_setters_po class (model as (clist,alist)) = 
    let
	val setter = secureUML_setter_operations_of (name_of class) model
    in
    	(List.map (fn a =>
		      let
			  val (name,arg_type)= hd(arguments_of_op a)
			  val self = Variable("self",type_of class)
			  val att_name = secureUML_get_att_name a
			  val arg = Variable(name,arg_type)
			  val att = AttributeCall(self,type_of class,(name_of class)@[att_name],arg_type)
			  val term1 = OperationCall(att,arg_type,["oclLib","Boolean","="],[(arg,arg_type)],Boolean)
			  val term2 = OperationCall(att,arg_type,(name_of class)@["modifiedOnly"],[],Boolean)
		      in
			  OperationCall(term1,Boolean,["oclLib","Boolean","and"],[(term2,Boolean)],Boolean)
		      end
		  ) setter)
    end
	
fun generate_secureUML_op_sec_po class (model as (clist,alist)) =
    let
	val op_sec = secureUML_op_sec_operations_of (name_of class) model
    in
	List.concat (List.map (fn a =>
		      let
			  val real_op = secureUML_real_op a class model
			  val add_pres = List.map (fn (a,b) => b ) (precondition_of_op real_op)
			  val real_posts = postcondition_of_op real_op
			  val add_posts = List.map (fn (a,b) => secureUML_substitute_terms b class) (real_posts)
		      in
			  add_pres@add_posts
		      end
		  ) op_sec)
    end



fun generate_pos wfpo (model as (clist,alist)) = (* [Variable("s",DummyT)] *)
    let
	val design_model_classes = removeOclLibrary clist
	val po1 = List.map (fn a => generate_secureUML_creators_po a model) design_model_classes 
	val po2 = List.map (fn a => generate_secureUML_destructors_po a model) design_model_classes 
	val po3 = List.map (fn a => generate_secureUML_getters_po a model) design_model_classes 
	val po4 = List.map (fn a => generate_secureUML_setters_po a model) design_model_classes 
	val po5 = List.map (fn a => generate_secureUML_op_sec_po a model) design_model_classes 
    in
	List.concat(po1@po2@po3@po4@po5)
    end
end;
