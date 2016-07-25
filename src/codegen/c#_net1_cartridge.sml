(*****************************************************************************
 * su4sml --- a SML repository for managing (Secure)UML/OCL models
 *             http://projects.brucker.ch/su4sml/
 *                                                                            
 * c#_catridge.sml --- a cartridge to gcg_core for C# .NET 1.x
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

functor CSharp_NET1_Cartridge(SuperCart : CARTRIDGE) : CARTRIDGE =
 struct
 open Rep_OclType
 
 
type environment = { extension : SuperCart.environment }
type Model = SuperCart.Model	


fun initEnv model = { extension = SuperCart.initEnv model } : environment

fun unpack  (env : environment) = #extension env

fun pack superEnv = {extension = superEnv} : environment

(* fun getModel (env:environment) = SuperCart.getModel (unpack env)*)

 (* internal translation table *)
fun super2Native "ClassifierScope" = "static"
 |  super2Native "InstanceScope"   = ""
 |  super2Native "package"	   = "public"
 |  super2Native "Integer"	   = "int"
 |  super2Native "Real"		   = "double"
 |  super2Native "String"	   = "string"
 |  super2Native "Boolean"	   = "bool"
 |  super2Native "OclVoid"	   = "void"
 |  super2Native s =  ( if ((String.extract (s,0,SOME 8)) = "Sequence")
 			then  (super2Native (String.substring(s,9,size s -10)))^"[]"
 			else if ((String.extract (s,0,SOME 3)) = "Set")
 			then "System.Collections.ArrayList"
 			(*else (gcg_warning ("Couldn't lookup \""^s^
 				"\" in c#_cartridge.super2Native !"); s)
 			*)
 			else s
 		       )
 		       handle Subscript => 
 			 (*(gcg_warning ("Couldn't lookup \""^s^"\" in c#_cartridge.super2Native !");s)*)
 			 s
 
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
  | lookup (env : environment) s =  SuperCart.lookup (unpack env) s


		 
fun test env  s = SuperCart.test (unpack env) s

		 

(* no further functionality to add
 * just unpack the Supercartridge's environment, 
 * pass it to SuperCart.foreach, get back a SuperCart.environment list
 * pack every item into a native environment
 *)
fun foreach listType (env : environment) 
		=  map pack (SuperCart.foreach listType (unpack env))
		   
 
end
