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


functor SecureUML_Cartridge(SuperCart : BASE_CARTRIDGE) : SECUREUML_CARTRIDGE =
 struct
 open Rep_SecureUML_ComponentUML.Security
 open ComponentUML
 open Gcg_Helper
 open Ocl2String

 
 type environment = { curPermissionSet: Permission list,
 		      curPermission : Permission,
		      curRole : string,
		      curConstraint : Rep_OclTerm.OclTerm,	
 		      extension : SuperCart.environment }
 
 
 (* service functions for other cartridges to have access to the current
  * list items
  *)
 fun curPermissionSet (env : environment) = #curPermissionSet env
 fun curPermission (env : environment) = #curPermission env
 fun curRole (env : environment) = #curRole env
 fun curConstraint (env : environment)  = #curConstraint env

 
 fun initEnv model = { curPermissionSet = [],
		       curPermission = emptyPermission,
 		       curRole       = emptyRole,
 		       curConstraint = emptyConstraint,
 		       extension = SuperCart.initEnv model } : environment

(* unpack : environment -> SuperCart.environment *)
fun unpack (env : environment) = #extension env

(* pack : environment -> SuperCart.environment -> environment *)
fun pack (env: environment) (new_env : SuperCart.environment) 
		= { curPermissionSet = #curPermissionSet env,
		    curPermission = #curPermission env,
 		    curRole       = #curRole env,
 		    curConstraint = #curConstraint env,
                    extension=new_env}
                    
                    
(* Helper functions that get the SuperCartridge's needed environment values *)                    
fun getModel (env : environment) = SuperCart.model (unpack env)
fun getCurClassifier (env : environment) = SuperCart.curClassifier (unpack env)
fun getCurAttribute (env : environment) = SuperCart.curAttribute (unpack env)
fun getCurOperation (env : environment) = SuperCart.curOperation (unpack env)

type permissionContext = {permissions : Permission list,
	 		  setter_permissions : Permission list,
	 		  getter_permissions : Permission list,
	 		  constructor_permissions : Permission list,
	 		  destructor_permissions : Permission list}

fun permissionsForAction (e : environment) a 
		= List.filter (isInPermission a) (#permissions (#2 (getModel e)))
                       	    		
(* computePermissionContext: environment -> permissionContext
 * compute Permissions according to actual environment 
 *)
fun computePermissionContext (env : environment)=
      let 
        fun path_of_attr () = (Rep_Core.name_of (getCurClassifier env))@[#name (getCurAttribute env)]
	fun path_of_op () = (Rep_Core.name_of (getCurClassifier env))@[(name_of_op (getCurOperation env))]
        fun getAction "set" = SimpleAction ("update", ("EntityAttribute",(path_of_attr ())))
	 |  getAction "get" = SimpleAction ("read", ("EntityAttribute",(path_of_attr ())))
	 |  getAction "execute" = SimpleAction ("execute", ("EntityMethod",(path_of_op ())))
	 |  getAction "create" = SimpleAction ("create", ("Entity",(Rep_Core.name_of (getCurClassifier env))))
	 |  getAction "delete" = SimpleAction ("delete", ("Entity",(Rep_Core.name_of (getCurClassifier env))))
	 |  getAction s = gcg_error ("invalid action_type \""^s^"\" in secureUML_cartridge.computePermissionContext:getAction.") 
      in
	if not((getCurAttribute env) = emptyAttribute) then
	  {permissions = [],
	   setter_permissions = (permissionsForAction env (getAction "set")),
	   getter_permissions = (permissionsForAction env (getAction "get")),
	   constructor_permissions = [],
	   destructor_permissions = []
	  }
	else if not((getCurOperation env) = emptyOperation) then
	  {permissions = permissionsForAction env (getAction "execute"),
	   setter_permissions = [],
	   getter_permissions = [],
	   constructor_permissions = [],
	   destructor_permissions = []
	  }
	else if not((getCurClassifier env) = emptyClassifier) then
	  {permissions = [],
	   setter_permissions = [],
	   getter_permissions = [],
	   constructor_permissions = permissionsForAction env (getAction "create"),
	   destructor_permissions  = permissionsForAction env (getAction "delete")
	  }
	else
	  {permissions = #permissions (#2 (getModel env)),
	   setter_permissions = [],
	   getter_permissions = [],
	   constructor_permissions = [],
	   destructor_permissions = []
	  }
      end 
                       
fun name_of_role  r 	= r
                           
(********** ADDING/MODIFYING VARIABLE SUBSTITUTIONS *****************************************)
(*	lookup  environment -> string -> string			
 * might override some lookup entries of the base cartridge 
 *)
fun lookup (env : environment) "permission_name" = #name (#curPermission env)
  | lookup (env : environment) "role_name"	 = name_of_role (#curRole env)
  | lookup (env : environment) "constraint"	 = ocl2string false (#curConstraint env)
 (* pass the unknown variables to the Superior Cartridge *)
  | lookup (env : environment) s =  SuperCart.lookup (unpack env) s

(********** ADDING IF-CONDITION TYPE *****************************************)
fun evalCondition (env : environment) "first_permission" = (#curPermission env 	= hd (#curPermissionSet env))
  | evalCondition (env : environment) "first_role"       = (#curRole env   	= hd (#roles (#curPermission env)))
  | evalCondition (env : environment) "first_constraint" = (#curConstraint env 	= hd (#constraints (#curPermission env)))
  | evalCondition (env : environment) "last_permission"  = (#curPermission env 	= List.last (#curPermissionSet env))
  | evalCondition (env : environment) "last_role"        = (#curRole env      	= List.last (#roles (#curPermission env)))
  | evalCondition (env : environment) "last_constraint"  = (#curConstraint env	= List.last (#constraints (#curPermission env)))
 (* pass unknown condition types to Superior Cartridge *)
  | evalCondition (env : environment) s = SuperCart.evalCondition (unpack env) s


(********** ADDING FOREACH TYPE **********************************************)

(* fun foreach_<new_list_type>: environment -> environment list *)
fun foreach_permission (env : environment) 
			= let val plist = #permissions (computePermissionContext env);      
			      fun env_from_list_item c ={curPermissionSet = plist,
						        curPermission = c,
						        curRole       = emptyRole ,
						        curConstraint = emptyConstraint,
						        extension = #extension env 
						        } : environment
			  in 
			       List.map env_from_list_item plist
			     end
			     
fun foreach_readPermission (env : environment) 
			= let val plist = #getter_permissions (computePermissionContext env);      
			      fun env_from_list_item c ={curPermissionSet = plist,
						        curPermission = c,
						        curRole       = emptyRole ,
						        curConstraint = emptyConstraint,
						        extension = #extension env 
						        } : environment
			  in 
			       List.map env_from_list_item plist
			     end

fun foreach_updatePermission (env : environment) 
			= let val plist = #setter_permissions (computePermissionContext env);      
			      fun env_from_list_item c ={curPermissionSet = plist,
						        curPermission = c,
						        curRole       = emptyRole ,
						        curConstraint = emptyConstraint,
						        extension = #extension env 
						        } : environment
			  in 
			       List.map env_from_list_item plist
			     end
fun foreach_createPermission (env : environment) 
			= let val plist = #constructor_permissions (computePermissionContext env);      
			      fun env_from_list_item c ={curPermissionSet = plist,
						        curPermission = c,
						        curRole       = emptyRole ,
						        curConstraint = emptyConstraint,
						        extension = #extension env 
						        } : environment
			  in 
			       List.map env_from_list_item plist
			     end

fun foreach_deletePermission (env : environment) 
			= let val plist = #destructor_permissions (computePermissionContext env);      
			      fun env_from_list_item c ={curPermissionSet = plist,
						        curPermission = c,
						        curRole       = emptyRole ,
						        curConstraint = emptyConstraint,
						        extension = #extension env 
						        } : environment
			  in 
			       List.map env_from_list_item plist
			     end

fun foreach_role (env : environment) 
			= let val roles = #roles (#curPermission env);      
			      fun env_from_list_item r ={curPermissionSet = #curPermissionSet env,
						        curPermission = #curPermission env,
						        curRole       = r ,
						        curConstraint = emptyConstraint,
						        extension = #extension env 
						        } : environment
			  in 
			       List.map env_from_list_item roles
			     end
			     		
fun foreach_constraint (env : environment) 
			= let val cons = #constraints (#curPermission env);      
			      fun env_from_list_item c ={curPermissionSet = #curPermissionSet env,
						        curPermission = #curPermission env,
						        curRole       = emptyRole ,
						        curConstraint = c,
						        extension = #extension env 
						        } : environment
			  in 
			       List.map env_from_list_item cons
			     end
			     		
			     
fun foreach "permission_list" env = foreach_permission env
 |  foreach "readPermission_list" env = foreach_readPermission env
 |  foreach "updatePermission_list" env = foreach_updatePermission env
 |  foreach "createPermission_list" env = foreach_createPermission env
 |  foreach "deletePermission_list" env = foreach_deletePermission env
 |  foreach "role_list"  env  	  = foreach_role env 
 |  foreach "constraint_list" env = foreach_constraint env
  (* pass unknown list types to superior cartridge by unpacking environments, 
   * having SuperCart compute environment list, pack into native environment again*)
 | foreach listType env = map (pack env) (SuperCart.foreach listType (unpack env))
  
  
end