functor ComponentUML_Cartridge(SuperCart : SECUREUML_CARTRIDGE) : DESIGN_LANGUAGE_CARTRIDGE = 
struct

structure Design = ComponentUML

(* TODO: fill out *)
type environment = { extension: SuperCart.environment}
type Model = SuperCart.Model

(* unpack : environment -> SuperCart.environment *)
fun unpack (env : environment) = #extension env

(* pack : environment -> SuperCart.environment -> environment *)
fun pack (env: environment) (new_env : SuperCart.environment) = {extension = new_env} 

fun initEnv model = {extension = SuperCart.initEnv model}
fun getModel (env : environment) = SuperCart.getModel (unpack env)
fun curClassifier (env : environment) = SuperCart.curClassifier (unpack env)
fun curAttribute (env : environment) = SuperCart.curAttribute (unpack env)
fun curOperation (env : environment) = SuperCart.curOperation (unpack env)
fun curArgument (env : environment) = SuperCart.curArgument (unpack env)


(* FIX *)
fun permissionsForAction env _ = nil

(* computePermissionContext: environment -> permissionContext
 * compute Permissions according to actual environment 
 * FIX: move to ComponentUML cartridge...*)
fun computePermissionContext (env : environment)=
      let 
		  fun getAction "set" = ComponentUML.SimpleAction ("update", (ComponentUML.EntityAttribute (Option.valOf(curAttribute env))))
			|  getAction "get" = ComponentUML.SimpleAction ("read", (ComponentUML.EntityAttribute (Option.valOf(curAttribute env))))
			|  getAction "execute" = ComponentUML.SimpleAction ("execute", (ComponentUML.EntityMethod (Option.valOf(curOperation env))))
			|  getAction "create" = ComponentUML.SimpleAction ("create", (ComponentUML.Entity (Option.valOf(curClassifier env))))
			|  getAction "delete" = ComponentUML.SimpleAction ("delete", (ComponentUML.Entity (Option.valOf (curClassifier env))))
			|  getAction s = Gcg_Helper.gcg_error ("invalid action_type \""^s^"\" in secureUML_cartridge.computePermissionContext:getAction.") 
      in
	if Option.isSome(curAttribute env) then
	  {permissions = [],
	   setter_permissions = (permissionsForAction env (getAction "set")),
	   getter_permissions = (permissionsForAction env (getAction "get")),
	   constructor_permissions = [],
	   destructor_permissions = []
	  }
	else if Option.isSome(curOperation env) then
	  {permissions = permissionsForAction env (getAction "execute"),
	   setter_permissions = [],
	   getter_permissions = [],
	   constructor_permissions = [],
	   destructor_permissions = []
	  }
	else if Option.isSome(curClassifier env) then
	  {permissions = [],
	   setter_permissions = [],
	   getter_permissions = [],
	   constructor_permissions = permissionsForAction env (getAction "create"),
	   destructor_permissions  = permissionsForAction env (getAction "delete")
	  }
	else
	  {permissions = SuperCart.Security.getPermissions (#2 (getModel env)),
	   setter_permissions = [],
	   getter_permissions = [],
	   constructor_permissions = [],
	   destructor_permissions = []
	  }
      end 


(********** ADDING/MODIFYING VARIABLE SUBSTITUTIONS *****************************************)
(*	lookup  environment -> string -> string			
 * might override some lookup entries of the base cartridge 
 *)
fun lookup (env : environment) s =  SuperCart.lookup (unpack env) s
									 

(********** ADDING IF-CONDITION TYPE *****************************************)
fun evalCondition (env : environment) s = SuperCart.evalCondition (unpack env) s

(********** ADDING FOREACH TYPE **********************************************)
(*
fun foreach_readPermission (env : environment) 
			= let val plist = #getter_permissions (computePermissionContext env);      
			      fun env_from_list_item c ={curPermissionSet = SOME plist,
						        curPermission = SOME c,
						        curRole       = NONE ,
						        curConstraint = NONE,
						        extension = #extension env 
						        } : environment
			  in 
			       List.map env_from_list_item plist
			     end

fun foreach_updatePermission (env : environment) 
			= let val plist = #setter_permissions (computePermissionContext env);      
			      fun env_from_list_item c ={curPermissionSet = SOME plist,
						        curPermission = SOME c,
						        curRole       = NONE ,
						        curConstraint = NONE,
						        extension = #extension env 
						        } : environment
			  in 
			       List.map env_from_list_item plist
			     end
fun foreach_createPermission (env : environment) 
			= let val plist = #constructor_permissions (computePermissionContext env);      
			      fun env_from_list_item c ={curPermissionSet = SOME plist,
						        curPermission = SOME c,
						        curRole       = NONE ,
						        curConstraint = NONE,
						        extension = #extension env 
						        } : environment
			  in 
			       List.map env_from_list_item plist
			     end

fun foreach_deletePermission (env : environment) 
			= let val plist = #destructor_permissions (computePermissionContext env);      
			      fun env_from_list_item c ={curPermissionSet = SOME plist,
						        curPermission = SOME c,
						        curRole       = NONE ,
						        curConstraint = NONE,
						        extension = #extension env 
						        } : environment
			  in 
			       List.map env_from_list_item plist
			     end
*)


fun foreach listType env =  map (pack env) (SuperCart.foreach listType (unpack env))


end
