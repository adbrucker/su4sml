(*****************************************************************************
 * su4sml --- a SML repository for managing (Secure)UML/OCL models
 *             http://projects.brucker.ch/su4sml/
 *                                                                            
 * junit_cartridge.sml --- a junit cartridge for gcg
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
(* $Id$ *)


(* FIXME: This is blindly copied from the C#_Cartridge.     *)
(* Probably, some things have to be adjusted to Java syntax *)
functor Junit_Cartridge(SuperCart : BASE_CARTRIDGE) : BASE_CARTRIDGE =
struct 

type Model = SuperCart.Model
			 
type environment = { extension : SuperCart.environment }

(* fun getModel (env:environment) = SuperCart.getModel (#extension env)*)
			   
				   
				   
				   
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


(* any special variables? *)
fun lookup (env : environment) (s as "preconditions") = Ocl2DresdenJava.precondString env "testObject" (curOperation' env) "PreconditionFailedException"
  | lookup (env : environment) (s as "postconditions") = Ocl2DresdenJava.postcondString env "testObject" (curOperation' env) "PostconditionFailedException"
  | lookup (env : environment) (s as "invariants") = Ocl2DresdenJava.invString env "testObject" (curClassifier' env) "InvariantFailedException"
  | lookup (env : environment) s =  SuperCart.lookup (unpack env) s

(* any special predicates?*) 
fun test (env : environment ) "operation_isNotPrivate" = not (test env "operation_isPrivate")
  | test (env : environment ) "not_last_argument" = not (test env "last_argument")
  | test (env : environment) "isTestable" = (not (test env "isInterface")) (* andalso (not (test env "isAbstract")) *) andalso (test env "hasOperations")
  | test (env : environment)  s = SuperCart.test (unpack env) s

(* Check if operation is already in given list *)
fun opInList (operation : environment) oplist = foldr (fn (a,b) => if (lookup operation "operation_name") = (lookup a "operation_name") then (false andalso b) else (true andalso b)) true oplist

(* Remove duplicate methods by comparing their names *)
fun unique_op oplist = foldl (fn (el,l) => if (opInList el l) then l @ [el] else l) [] oplist

(* any special lists? *)
fun foreach "unique_operation_list" (env : environment) = unique_op (foreach "operation_list" env)
  | foreach listType (env : environment) 
		=  map pack (SuperCart.foreach listType (unpack env))

end
