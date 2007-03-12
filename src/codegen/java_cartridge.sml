(*****************************************************************************
 *          su4sml - a SecureUML repository for SML              
 *                                                                            
 * java_cartridge.sml - a java cartridge for gcg
 * Copyright (C) 2005 Raphael Eidenbenz <eraphael@student.ethz.ch>
 *                                                                            
 * This file is part of su4sml.                                              
 *                                                                            
 * su4sml is free software; you can redistribute it and/or modify it under   
 * the terms of the GNU General Public License as published by the Free       
 * Software Foundation; either version 2 of the License, or (at your option)  
 * any later version.                                                         
 *                                                                            
 * su4sml is distributed in the hope that it will be useful, but WITHOUT ANY 
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS 
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more 
 * details.                                                              
 *                                                                            
 * You should have received a copy of the GNU General Public License along    
 * with this program; if not, write to the Free Software Foundation, Inc.,    
 * 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.                  
 ******************************************************************************)
 

(* FIXME: This is blindly copied from the C#_Cartridge.     *)
(* Probably, some things have to be adjusted to Java syntax *)
functor Java_Cartridge(SuperCart : BASE_CARTRIDGE) : BASE_CARTRIDGE =
struct
open Rep_OclType
open library
	 
	 
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
