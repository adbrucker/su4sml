(*****************************************************************************
 *          su4sml - a SecureUML repository for SML              
 *                                                                            
 * junit_cartridge.sml - a junit cartridge for gcg
 * Copyright (C) 2006 Manfred Stock
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
fun lookup (env : environment) (s as "preconditions") = Ocl2DresdenJava.precondString env "testObject" (curOperation' env)
  | lookup (env : environment) (s as "postconditions") = Ocl2DresdenJava.postcondString env "testObject" (curOperation' env)
  | lookup (env : environment) (s as "invariants") = Ocl2DresdenJava.invString env "testObject" (curClassifier' env)
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
