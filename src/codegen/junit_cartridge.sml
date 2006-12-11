(*****************************************************************************
 *          su4sml - a SecureUML repository for SML              
 *                                                                            
 * junit_cartridge.sml - a junit cartridge for gcg
 * Copyright (C) 2006
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


(* any special variables? *)
fun lookup (env : environment) s =  SuperCart.lookup (unpack env) s

(* any special predicates?*) 
fun test (env : environment)  s = SuperCart.test (unpack env) s

(* any special lists? *)
fun foreach listType (env : environment) 
		=  map pack (SuperCart.foreach listType (unpack env))

end
