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

(**
 * A Cartridge that supports the basic SecureUML concepts: 
 * Permissions, Roles, and Constraints. 
 *)
signature SECUREUML_CARTRIDGE =
sig

(** the particular secureuml dialect used *)
structure Security:SECUREUML
		
include BASE_CARTRIDGE where
type Model = Rep.Classifier list * Security.Configuration

val PermissionSet: environment -> Security.Permission list
val curPermission : environment   -> Security.Permission option
val curRole : environment -> string option
val curConstraint : environment -> Rep_OclTerm.OclTerm option 
								   

end


(**
 * A Cartridge that supports the basic SecureUML concepts: 
 * Permissions, Roles, and Constraints. 
 *)
functor SecureUML_Cartridge(structure SuperCart : BASE_CARTRIDGE; 
                            structure D: DESIGN_LANGUAGE) : SECUREUML_CARTRIDGE =
struct

structure Security = SecureUML(structure Design = D)

type Model = Rep.Classifier list * Security.Configuration
	     
type environment = { model           : Model,
		     PermissionSet   : Security.Permission list,
		     curPermission   : Security.Permission option,
                     curSubject      : Security.Subject option,
		     curRole         : string option,
		     curConstraint   : Rep_OclTerm.OclTerm option,	
		     extension       : SuperCart.environment }
		   
fun PermissionSet    (env : environment) =  (#PermissionSet env)
fun curPermission    (env : environment) =  (#curPermission env)
fun curRole          (env : environment) =  (#curRole env)
fun curConstraint    (env : environment) =  (#curConstraint env)
fun curSubject       (env : environment) =  (#curSubject env)

fun curPermission'    (env : environment) = Option.valOf (#curPermission env)
fun curRole'          (env : environment) = Option.valOf (#curRole env)
fun curConstraint'    (env : environment) = Option.valOf (#curConstraint env)
											
fun initEnv model = let val m = Security.parse model 
		    in
			{ model = m, 
			  PermissionSet = (#permissions (#2 m)),
			  curPermission = NONE,
                          curSubject    = NONE,
			  curRole       = NONE,
			  curConstraint = NONE,
			  extension = SuperCart.initEnv (#1 m) } : environment
		    end
						
(* unpack : environment -> SuperCart.environment *)
fun unpack (env : environment) = #extension env

(* pack : environment -> SuperCart.environment -> environment *)
fun pack (env: environment) (new_env : SuperCart.environment) 
  = { model            = #model env,
      PermissionSet    = #PermissionSet env,
      curSubject       = #curSubject env,
      curPermission    = #curPermission env,
      curRole          = #curRole env,
      curConstraint    = #curConstraint env,
      extension        = new_env}
    
	
(* Helper functions that get the SuperCartridge's needed environment values *) 
fun getModel (env : environment)      = #model env
fun curClassifier (env : environment) = SuperCart.curClassifier (unpack env)
fun curAttribute (env : environment)  = SuperCart.curAttribute (unpack env)
fun curOperation (env : environment)  = SuperCart.curOperation (unpack env)
fun curArgument (env : environment)   = SuperCart.curArgument (unpack env)
fun curAssociationEnd env = SuperCart.curAssociationEnd (unpack env)

									  
                       
fun name_of_role  r 	= r
                           


(********** ADDING/MODIFYING VARIABLE SUBSTITUTIONS *************************)
(*	lookup  environment -> string -> string			
 * might override some lookup entries of the base cartridge 
 *)
fun lookup env "permission_name" = #name (curPermission' env)
  | lookup env "role_name"	 = name_of_role (curRole' env)
  | lookup env "constraint"	 = Ocl2String.ocl2string false (curConstraint' env)
  | lookup env "subject_name"    = (Security.subject_name_of o valOf o curSubject) env
  | lookup env s                 =  SuperCart.lookup (unpack env) s

(********** ADDING IF-CONDITION TYPE *****************************************)
fun test env "first_permission" = (curPermission' env = hd (PermissionSet env))
  | test env "first_role"       = (curRole' env = hd (#roles (curPermission' env)))
  | test env "first_constraint" = (curConstraint' env = hd (#constraints (curPermission' env)))
  | test env "last_permission"  = (curPermission' env = List.last (PermissionSet env))
  | test env "last_role"        = (curRole' env = List.last (#roles (curPermission' env)))
  | test env "last_constraint"  = (curConstraint' env 
                                   = List.last (#constraints (curPermission' env)))
  | test env s                  = SuperCart.test (unpack env) s


(********** ADDING FOREACH TYPE **********************************************)
(** iterates over roles, depending on the context.
 * in the context of a permission, iterate over all roles which have this permission.
 * in the context of a subject, iterate over all roles of that subject.
 * outside of these contextes, iterate over all roles.
 *)
fun foreach_role (env:environment) 
  = let val roles = case #curPermission env
                     of SOME p => #roles p
                      | NONE   => case #curSubject env
                                   of SOME s => (Security.subject_roles_of s o #2 o #model) env 
                                    | NONE   => (Security.all_roles o #2 o #model) env
	fun env_from_list_item r ={ model = #model env,
				    PermissionSet = #PermissionSet env,
				    curPermission = #curPermission env,
                                    curSubject    = #curSubject env,
				    curRole       = SOME r ,
				    curConstraint = NONE,
				    extension = #extension env } : environment
    in 
	List.map env_from_list_item roles
    end
		
(* FIXME: in the context of a permission, return the constraints of this permission. 
 * outside of such a context, return all constraints. *)
fun foreach_constraint (env:environment) 
  = let val cons = case #curPermission env
                    of SOME p => #constraints p
                     | NONE   => Security.all_constraints (#2 (#model env))     
	fun env_from_list_item c ={ model = #model env,
				    PermissionSet = #PermissionSet env,
				    curPermission = #curPermission env,
                                    curSubject    = NONE,
				    curRole       = #curRole env ,
				    curConstraint = SOME c,
				    extension = #extension env } : environment
    in 
	List.map env_from_list_item cons
    end
    
(* FIXME (when possible): in the context of a role, return the permissions of this role.
 * outside of such a context, return all permissions.*)
 
fun foreach_permission env 
  = let val perms = PermissionSet env
	fun env_from_list_item p ={ model = #model env,
				    PermissionSet = #PermissionSet env,
				    curPermission = SOME p,
                                    curSubject    = NONE,
				    curRole       = NONE ,
				    curConstraint = NONE ,
				    extension = #extension env } : environment
    in 
	List.map env_from_list_item perms
    end

fun foreach_subject (env:environment) =
    let val subjects = (Security.all_subjects o #2 o #model) env
        fun env_from_list_item s = { model = #model env,
				    PermissionSet = #PermissionSet env,
				    curPermission = NONE,
                                    curSubject    = SOME s,
				    curRole       = NONE,
				    curConstraint = NONE,
				    extension = #extension env } : environment
    in 
	List.map env_from_list_item subjects
    end
    
    
fun foreach "role_list"       env = foreach_role env 
  | foreach "constraint_list" env = foreach_constraint env
  | foreach "permission_list" env = foreach_permission env
  | foreach "subject_list"    env = foreach_subject env
  | foreach listType          env = map (pack env) (SuperCart.foreach listType (unpack env))
                                    
  
end
