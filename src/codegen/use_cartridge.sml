(*****************************************************************************
 * su4sml --- an SML repository for managing (Secure)UML/OCL models
 *             http://projects.brucker.ch/su4sml/
 *                                                                            
 * use_cartridge.sml --- USE (UML Specification Environment) cartridge 
 * This file is part of su4sml.
 *
 * Copyright (c) 2007, ETH Zurich, Switzerland
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
(* $Id: USE_cartridge.sml 40127 2007-07-04 06:41:30Z brucker $ *)

functor Use_Cartridge(SuperCart : BASE_CARTRIDGE) : BASE_CARTRIDGE =
struct 

type Model = SuperCart.Model
	     
type environment = { extension : SuperCart.environment }
		   
fun initEnv model = { extension = SuperCart.initEnv model } : environment
fun unpack  (env : environment) = #extension env
fun pack superEnv = {extension = superEnv} : environment
					     
(* for BASE_CARTRIDGE *)
fun curClassifier env = SuperCart.curClassifier (unpack env)
fun curArgument env = SuperCart.curArgument (unpack env)
fun curOperation env = SuperCart.curOperation (unpack env)
fun curAttribute env = SuperCart.curAttribute (unpack env)
fun curAssociationEnd env = SuperCart.curAssociationEnd (unpack env)

fun curClassifier' env = Option.valOf(curClassifier env)
fun curOperation' env = Option.valOf(curOperation env)



open Rep_OclType

fun localString_of_OclType Integer        = "Integer"
  | localString_of_OclType Real           = "Real"
  | localString_of_OclType String         = "String"
  | localString_of_OclType Boolean        = "Boolean"
  | localString_of_OclType OclAny         = "OclAny"
  | localString_of_OclType (Set t)             = ("Set("^(localString_of_OclType t)^")")
  | localString_of_OclType (Sequence t)   = ("Sequence("^(localString_of_OclType t)^")")
  | localString_of_OclType (OrderedSet t) = ("OrderedSet("^(localString_of_OclType t)^")")
  | localString_of_OclType (Bag t)        = ("Bag("^(localString_of_OclType t)^")")
  | localString_of_OclType (Collection t) = ("Collection("^(localString_of_OclType t)^")")
  | localString_of_OclType OclVoid        = "OclVoid"
  | localString_of_OclType (Classifier p) = (hd (rev p))
  | localString_of_OclType DummyT         = "DummyT"
  | localString_of_OclType (TemplateParameter s) = "TemplateParameter \""^s^"\""



(* any special variables? *)
fun lookup env "attribute_type" = localString_of_OclType (#attr_type (valOf (curAttribute  env))) 
  | lookup env "argument_type"  = localString_of_OclType (#2 (valOf (curArgument env)))
  | lookup env  "assocend_type"  
               = ((localString_of_OclType o #aend_type o valOf o curAssociationEnd) env) 
  | lookup env "operation_result_type" = localString_of_OclType (Rep.result_of_op
                                                               (valOf (curOperation env)))
  | lookup (env : environment) s =  SuperCart.lookup (unpack env) s


(* any special predicates?*) 
fun test (env : environment)  s = SuperCart.test (unpack env) s

(* any special lists? *)
fun foreach listType (env : environment) =  map pack (SuperCart.foreach listType (unpack env))

end
(*
fun test () = 
    let 
	val uml = "/home/brucker/infsec/src/HOL-OCL/hol-ocl/examples/company/company.zargo"
	val ocl = "/home/brucker/infsec/src/HOL-OCL/hol-ocl/examples/company/company.ocl"
        val model = map Rep.normalize (ModelImport.import uml ocl [])
    in
	Codegen.generateFromModel model "USE"
    end
*)
