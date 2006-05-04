(*****************************************************************************
 *          su4sml GCG - Generic Code Generator               
 *                                                                            
 * secureuml_cartridge.sml - A cartridge for Access Control features of SecureUML
 * 		  transcribes a su4sml model according to a template tree 
 *		  into code specific to a target language cartridge C
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


signature SECUREUML_CARTRIDGE =
sig

(** the particular secureuml dialect used *)
structure Security:SECURITY_LANGUAGE
		
include BASE_CARTRIDGE where
type Model = Rep.Classifier list * Security.Configuration

val curPermissionSet: environment -> Security.Permission list option
val curPermission : environment   -> Security.Permission option
val curRole : environment -> string option
val curConstraint : environment -> Rep_OclTerm.OclTerm option 
								   
val isInPermission :  Security.Design.Action -> Security.Permission -> bool

end


functor SecureUML_Cartridge(structure SuperCart : BASE_CARTRIDGE; 
						    structure D: DESIGN_LANGUAGE) 
		: SECUREUML_CARTRIDGE =
struct

structure Security = SecureUML(structure Design = D)

type Model = Rep.Classifier list * Security.Configuration
					 
type environment = { model           : Model,
					 curPermissionSet: Security.Permission list option,
					 curPermission   : Security.Permission option,
					 curRole         : string option,
					 curConstraint   : Rep_OclTerm.OclTerm option,	
					 extension       : SuperCart.environment }
				   
fun curPermissionSet (env : environment) =  (#curPermissionSet env)
fun curPermission    (env : environment) =  (#curPermission env)
fun curRole          (env : environment) =  (#curRole env)
fun curConstraint    (env : environment) =  (#curConstraint env)

fun curPermissionSet' (env : environment) = Option.valOf (#curPermissionSet env)
fun curPermission'    (env : environment) = Option.valOf (#curPermission env)
fun curRole'          (env : environment) = Option.valOf (#curRole env)
fun curConstraint'    (env : environment) = Option.valOf (#curConstraint env)
											
fun initEnv model = let val m = Security.parse model 
					in
						{ model = m, 
						  curPermissionSet = NONE,
						  curPermission = NONE,
						  curRole       = NONE,
						  curConstraint = NONE,
						  extension = SuperCart.initEnv (#1 m) } : environment
					end
						
(* unpack : environment -> SuperCart.environment *)
fun unpack (env : environment) = #extension env

(* pack : environment -> SuperCart.environment -> environment *)
fun pack (env: environment) (new_env : SuperCart.environment) 
  = { model            = #model env,
	  curPermissionSet = #curPermissionSet env,
	  curPermission    = #curPermission env,
	  curRole          = #curRole env,
	  curConstraint    = #curConstraint env,
	  extension        = new_env}
	
	
(* Helper functions that get the SuperCartridge's needed environment values *)                    
fun getModel (env : environment) = #model env
fun curClassifier (env : environment) = SuperCart.curClassifier (unpack env)
fun curAttribute (env : environment) = SuperCart.curAttribute (unpack env)
fun curOperation (env : environment) = SuperCart.curOperation (unpack env)
fun curArgument (env : environment) = SuperCart.curArgument (unpack env)
									  
fun is_contained_in a1 a2 = (a1 = a2) orelse 
							List.exists (fn x=> x=true) ((List.map (is_contained_in a1) (D.subordinated_actions a2))) 


fun isInPermission a (p:Security.Permission) = List.exists (is_contained_in a) (#actions p)
                       
fun name_of_role  r 	= r
                           
(********** ADDING/MODIFYING VARIABLE SUBSTITUTIONS *****************************************)
(*	lookup  environment -> string -> string			
 * might override some lookup entries of the base cartridge 
 *)
fun lookup (env : environment) "permission_name" = #name (curPermission' env)
  | lookup (env : environment) "role_name"	 = name_of_role (curRole' env)
  | lookup (env : environment) "constraint"	 = Ocl2String.ocl2string false (curConstraint' env)
 (* pass the unknown variables to the Superior Cartridge *)
  | lookup (env : environment) s =  SuperCart.lookup (unpack env) s

(********** ADDING IF-CONDITION TYPE *****************************************)
fun evalCondition (env : environment) "first_permission" = 
	(curPermission' env 	= hd (curPermissionSet' env))
  | evalCondition (env : environment) "first_role"       = 
	(curRole' env   	= hd (#roles (curPermission' env)))
  | evalCondition (env : environment) "first_constraint" = 
	(curConstraint' env 	= hd (#constraints (curPermission' env)))
  | evalCondition (env : environment) "last_permission"  = 
	(curPermission' env 	= List.last (curPermissionSet' env))
  | evalCondition (env : environment) "last_role"        = 
	(curRole' env      	= List.last (#roles (curPermission' env)))
  | evalCondition (env : environment) "last_constraint"  = 
	(curConstraint' env	= List.last (#constraints (curPermission' env)))
 (* pass unknown condition types to Superior Cartridge *)
  | evalCondition (env : environment) s = SuperCart.evalCondition (unpack env) s


(********** ADDING FOREACH TYPE **********************************************)
			     

fun foreach_role (env : environment) 
  = let val roles = #roles (curPermission' env);      
			fun env_from_list_item r ={ model = #model env,
									    curPermissionSet = #curPermissionSet env,
										curPermission = #curPermission env,
										curRole       = SOME r ,
										curConstraint = NONE,
										extension = #extension env } : environment
	in 
		List.map env_from_list_item roles
	end
		
fun foreach_constraint (env : environment) 
  = let val cons = #constraints (curPermission' env);      
			fun env_from_list_item c ={ model = #model env,
									    curPermissionSet = #curPermissionSet env,
										curPermission = #curPermission env,
										curRole       = NONE ,
										curConstraint = SOME c,
										extension = #extension env } : environment
	in 
		List.map env_from_list_item cons
	end
			     		
			     
fun foreach "role_list"       env = foreach_role env 
  | foreach "constraint_list" env = foreach_constraint env
  | foreach listType          env = map (pack env) 
										(SuperCart.foreach listType (unpack env))
  
  
end
