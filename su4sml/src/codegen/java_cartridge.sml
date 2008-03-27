(*****************************************************************************
 * su4sml --- a SML repository for managing (Secure)UML/OCL models
 *             http://projects.brucker.ch/su4sml/
 *                                                                            
 * java_cartridge.sml --- a java cartridge for gcg
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
functor Java_Cartridge(SuperCart : BASE_CARTRIDGE) : BASE_CARTRIDGE =
struct
open Rep_Logger
open Rep_OclType
	 
	 
type Model = SuperCart.Model
			 
type environment = { curParent : Rep_OclType.Path option,
		     extension : SuperCart.environment }

(* fun getModel (env:environment) = SuperCart.getModel (#extension env)*)
			   
				   
				   
				   
fun initEnv model = { curParent = NONE,
		      extension = SuperCart.initEnv model } : environment
															  
fun unpack  (env : environment) = #extension env
								  
fun pack (Env : environment) (superEnv : SuperCart.environment) = {curParent = #curParent Env,
								   extension = superEnv} : environment

fun curClassifier env = SuperCart.curClassifier (unpack env)
fun curArgument   env = SuperCart.curArgument   (unpack env)
fun curAssociationEnd env = SuperCart.curAssociationEnd   (unpack env)
fun curOperation  env = SuperCart.curOperation  (unpack env)
fun curAttribute  env = SuperCart.curAttribute  (unpack env)
fun curParent     (env : environment) = #curParent env
		
fun curClassifier' env = Option.valOf(curClassifier env)
fun curOperation' env = Option.valOf(curOperation env)
fun curParent'    (env : environment) = Option.valOf(curParent env)
									 
(* internal translation table, blindly copied from C# *)
fun super2Native "ClassifierScope" = "static"
 |  super2Native "InstanceScope"   = ""
 |  super2Native "package"	   = ""
 |  super2Native "Integer"	   = "int"
 |  super2Native "Real"		   = "double"
 |  super2Native "Boolean"	   = "Boolean"
 |  super2Native "OclVoid"	   = "void"
 |  super2Native s =  ( if ((String.extract (s,0,SOME 8)) = "Sequence")
 			then  (super2Native (String.substring(s,9,size s -10)))^"[]"
 			else if ((String.extract (s,0,SOME 3)) = "Set")
 	 		then "java.util.List<"
 				^(super2Native (String.substring(s,4,size s - 5)))^">"
 			else s )
 	handle Subscript => s

(* Get a stub for the return value of an operation - eg. null for objects *)
fun rvstub operation = case (Rep.result_of_op operation) of 
			   Integer => "0"
			 | Real    => "0"
			 | String  => "\"\""
			 | Boolean => "false"
			 | _       => "null"

(*	lookup  environment -> string -> string			
 * overrides some lookup entries of the base cartridge 
 *)
fun lookup (env : environment) "attribute_name_small_letter" 
	=  StringHandling.uncapitalize (SuperCart.lookup (unpack env) "attribute_name")
  | lookup (env : environment) "attribute_name_capital" 
	=  StringHandling.capitalize (SuperCart.lookup (unpack env) "attribute_name")
  | lookup (env : environment) (s as "attribute_type") 	= super2Native (SuperCart.lookup (unpack env) s )
  | lookup (env : environment) (s as "attribute_visibility")= super2Native (SuperCart.lookup (unpack env) s)
  | lookup (env : environment) (s as "attribute_scope")	= super2Native (SuperCart.lookup (unpack env) s)
  | lookup (env : environment) (s as "operation_result_type")=super2Native (SuperCart.lookup (unpack env) s)
  | lookup (env : environment) (s as "operation_visibility")= super2Native (SuperCart.lookup (unpack env) s)
  | lookup (env : environment) (s as "operation_scope")	= super2Native (SuperCart.lookup (unpack env) s)
  | lookup (env : environment) (s as "argument_type") 	= super2Native (SuperCart.lookup (unpack env) s)
  | lookup (env : environment) (s as "parent_interface") = List.last (Option.valOf (#curParent env))
  | lookup (env : environment) (s as "preconditions") = Ocl2DresdenJava.precondString env "this" (curOperation' env) "OclException"
  | lookup (env : environment) (s as "postconditions") = Ocl2DresdenJava.postcondString env "this" (curOperation' env) "OclException"
  | lookup (env : environment) (s as "invariants") = Ocl2DresdenJava.invString env "this" (curClassifier' env) "OclException"
  | lookup (env : environment) (s as "returnvalue_stub") = rvstub (curOperation' env)
  | lookup (env : environment) s =  SuperCart.lookup (unpack env) s


		 
fun test (env : environment) "hasParentInterfaces" = (length (Rep_Core.parent_interface_names_of (curClassifier' env))) <> 0
  | test env "last_interface" = (List.last (Rep_Core.parent_interface_names_of (curClassifier' env))) =
				curParent' env
  | test env "operation_has_arguments" = (length (Rep_Core.arguments_of_op (curOperation' env))) > 0
  | test env "operation_is_void" = (lookup env "operation_result_type") = "void"
  | test env "operation_non_void" = (lookup env "operation_result_type") <> "void"
  | test (env : environment)  s = SuperCart.test (unpack env) s

fun foreach_parent_interface (env : environment)
    = let val parents = Rep_Core.parent_interface_names_of (curClassifier' env)
	  fun env_from_parent p = { curParent = SOME p,
				    extension = #extension env }
      in 
	  List.map env_from_parent parents
      end

fun foreach "parent_interface_list" env = foreach_parent_interface env
  (* no further functionality to add
   * just unpack the Supercartridge's environment, 
   * pass it to SuperCart.foreach, get back a SuperCart.environment list
   * pack every item into a native environment
   *)
  | foreach listType (env : environment) = map (pack env) (SuperCart.foreach listType (unpack env))
		    
end
