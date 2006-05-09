(*****************************************************************************
 *          su4sml GCG - Generic Code Generator               
 *                                                                            
 * c#_catridge.sml - a cartridge to gcg_core for C# .NET 2.0
 * Copyright (C) 2005 Raphael Eidenbenz <eraphael@student.ethz.ch>
 *                                                                            
 * This file is part of su4sml-gcg.                                              
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
 
functor CSharp_Cartridge(SuperCart : CARTRIDGE) : CARTRIDGE =
struct
open Rep_OclType
	 
	 
type Model = SuperCart.Model
			 
type environment = { extension : SuperCart.environment }

fun getModel (env:environment) = SuperCart.getModel (#extension env)
			   
				   
				   
				   
fun initEnv model = { extension = SuperCart.initEnv model } : environment
															  
fun unpack  (env : environment) = #extension env
								  
fun pack superEnv = {extension = superEnv} : environment

											 
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
 			then "System.Collections.Generic.List<"
 				^(super2Native (String.substring(s,4,size s - 5)))^">"
 			(*else (gcg_warning ("Couldn't lookup \""^s^
 				"\" in c#_cartridge.super2Native !"); s)
 			*)
 			else s
 		       )
 		       handle Subscript => 
 			 (*(gcg_warning ("Couldn't lookup \""^s^"\" in c#_cartridge.super2Native !");s)*)
 			 s
 
fun startWithSmallLetter s = let val sl = String.explode s
 			     in
 			       String.implode ((Char.toLower (hd sl))::(tl sl))
 			     end
 
fun startWithCapital s = let val sl = String.explode s
 			     in
 			       String.implode ((Char.toUpper (hd sl))::(tl sl))
			     end
(*	lookup  environment -> string -> string			
 * overrides some lookup entries of the base cartridge 
 *)
fun lookup (env : environment) "attribute_name_small_letter" 
	=  startWithSmallLetter (SuperCart.lookup (unpack env) "attribute_name")
  | lookup (env : environment) "attribute_name_capital" 
	=  startWithCapital (SuperCart.lookup (unpack env) "attribute_name")
  | lookup (env : environment) (s as "attribute_type") 	= super2Native (SuperCart.lookup (unpack env) s )
  | lookup (env : environment) (s as "attribute_visibility")= super2Native (SuperCart.lookup (unpack env) s)
  | lookup (env : environment) (s as "attribute_scope")	= super2Native (SuperCart.lookup (unpack env) s)
  | lookup (env : environment) (s as "operation_result_type")=super2Native (SuperCart.lookup (unpack env) s)
  | lookup (env : environment) (s as "operation_visibility")= super2Native (SuperCart.lookup (unpack env) s)
  | lookup (env : environment) (s as "operation_scope")	= super2Native (SuperCart.lookup (unpack env) s)
  | lookup (env : environment) (s as "argument_type") 	= super2Native (SuperCart.lookup (unpack env) s)
  | lookup (env : environment) s =  SuperCart.lookup (unpack env) s


		 
fun test (env : environment)  s = SuperCart.test (unpack env) s

(* no further functionality to add
 * just unpack the Supercartridge's environment, 
 * pass it to SuperCart.foreach, get back a SuperCart.environment list
 * pack every item into a native environment
 *)
fun foreach listType (env : environment) 
		=  map pack (SuperCart.foreach listType (unpack env))
		   
 
end
