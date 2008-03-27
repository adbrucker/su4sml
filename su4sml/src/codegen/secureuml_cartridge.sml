(*****************************************************************************
 * su4sml --- a SML repository for managing (Secure)UML/OCL models
 *             http://projects.brucker.ch/su4sml/
 *                                                                            
 * secureuml_cartridge.sml --- A cartridge for Access Control features of SecureUML
 * 		               transcribes a su4sml model according to a template 
 *                             tree into code specific to a target language  
 *                             cartridge 
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

(**
 * A Cartridge that supports the basic SecureUML concepts: 
 * Permissions, Roles, and Constraints. 
 *)
signature SECUREUML_CARTRIDGE =
sig

(** the particular secureuml dialect used *)
structure Security:SECUREUML
		
include BASE_CARTRIDGE where
(*type Model = Rep.Classifier list * Security.Configuration*)
type Model = Rep.Model * Security.Configuration

val PermissionSet : environment -> Security.Permission list
val curPermission : environment -> Security.Permission option
val curRole       : environment -> string option
val curSubject    : environment -> Security.Subject option
val curSuperrole  : environment -> string option
val curConstraint : environment -> Rep_OclTerm.OclTerm option 
								   

end


(**
 * A Cartridge that supports the basic SecureUML concepts: 
 * Permissions, Roles, and Constraints. 
 *)
functor SecureUML_Cartridge(structure SuperCart : BASE_CARTRIDGE; 
                            structure D: DESIGN_LANGUAGE) : SECUREUML_CARTRIDGE =
struct

open Rep_Logger
structure Security = SecureUML(structure Design = D)

(*type Model = Rep.Classifier list * Security.Configuration*)
type Model = Rep.Model * Security.Configuration
	     
type environment = { model           : Model,
		     PermissionSet   : Security.Permission list,
		     curPermission   : Security.Permission option,
                     curSubject      : Security.Subject option,
		     curRole         : string option,
                     curSuperrole    : string option,
		     curConstraint   : Rep_OclTerm.OclTerm option,	
		     extension       : SuperCart.environment }
		   
fun PermissionSet    (env : environment) =  (#PermissionSet env)
fun curPermission    (env : environment) =  (#curPermission env)
fun curRole          (env : environment) =  (#curRole env)
fun curSuperrole     (env : environment) =  (#curSuperrole env)
fun curConstraint    (env : environment) =  (#curConstraint env)
fun curSubject       (env : environment) =  (#curSubject env)

fun curPermission'    (env : environment) = Option.valOf (#curPermission env)
fun curRole'          (env : environment) = Option.valOf (#curRole env)
fun curConstraint'    (env : environment) = Option.valOf (#curConstraint env)

fun security_conf (env: environment) = #2 (#model env)
     
fun initEnv model = let val m = Security.parse model 
		    in
			{ model = m, 
			  PermissionSet = (#permissions (#2 m)),
			  curPermission = NONE,
                          curSubject    = NONE,
			  curRole       = NONE,
                          curSuperrole  = NONE,
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
      curSuperrole     = #curSuperrole env,
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
  | lookup env "superrole_name"  = (name_of_role o valOf o curSuperrole) env
  | lookup env s                 =  SuperCart.lookup (unpack env) s
    handle Option => error "variable outside of context"
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
	fun env_from_list_item r ={ model         = #model env,
				    PermissionSet = #PermissionSet env,
				    curPermission = #curPermission env,
                                    curSubject    = #curSubject env,
				    curRole       = SOME r ,
                                    curSuperrole  = NONE,
				    curConstraint = NONE,
				    extension     = #extension env } : environment
    in 
	List.map env_from_list_item roles
    end

(** iterate over all superroles in the context of a role *)
fun foreach_superrole (env:environment) =
    let val cur = valOf (curRole env )
                  handle Option => error ("no current role")
        val superroles = List.mapPartial (fn (r,s) => if r=cur then SOME s
                                                      else NONE) 
                                         (#rh (security_conf  env))
        fun env_from_list_item s = { model         = #model env,
				     PermissionSet = #PermissionSet env,
				     curPermission = #curPermission env,
                                     curSubject    = #curSubject env,
				     curRole       = #curRole env,
                                     curSuperrole  = SOME s,
				     curConstraint = NONE,
				     extension     = #extension env } : environment
    in 
        List.map env_from_list_item superroles
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
                                    curSuperrole  = NONE,
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
                                    curSuperrole  = NONE,
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
                                    curSuperrole  = NONE,
				    curRole       = NONE,
				    curConstraint = NONE,
				    extension = #extension env } : environment
    in 
	List.map env_from_list_item subjects
    end
    
    
fun foreach "role_list"       env = foreach_role env 
  | foreach "superrole_list"  env = foreach_superrole env
  | foreach "constraint_list" env = foreach_constraint env
  | foreach "permission_list" env = foreach_permission env
  | foreach "subject_list"    env = foreach_subject env
  | foreach listType          env = map (pack env) (SuperCart.foreach listType (unpack env))
                                    
  
end
