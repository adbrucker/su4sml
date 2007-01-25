functor ComponentUML_Cartridge(S : BASE_CARTRIDGE) : DESIGN_LANGUAGE_CARTRIDGE = 
struct

structure SuperCart = SecureUML_Cartridge(structure SuperCart=S; structure D=ComponentUML)

structure Design = SuperCart.Security.Design
open library

(* TODO: fill out *)
type environment = { curPermissionList: SuperCart.Security.Permission list option, 
                     curPermission: SuperCart.Security.Permission option,
                     curEntity: Rep.Classifier option,
                     extension: SuperCart.environment}
type Model = SuperCart.Model
             
(* unpack : environment -> SuperCart.environment *)
fun unpack (env : environment) = #extension env

(* pack : environment -> SuperCart.environment -> environment *)
fun pack (env: environment) (new_env : SuperCart.environment) = 
    { curPermissionList = #curPermissionList env,
      curPermission = #curPermission env,
      curEntity = #curEntity env,
      extension = new_env} 
    
fun initEnv model = { curPermissionList = NONE,
                      curPermission = NONE,
                      curEntity = NONE,
                      extension = SuperCart.initEnv model}
                    
(* fun getModel (env : environment) = SuperCart.getModel (unpack env) *)
fun curClassifier (env : environment) = SuperCart.curClassifier (unpack env)
fun curAttribute (env : environment) = SuperCart.curAttribute (unpack env)
fun curOperation (env : environment) = SuperCart.curOperation (unpack env)
fun curArgument (env : environment) = SuperCart.curArgument (unpack env)
									  
(** 
 * compute the atomic actions that are possible on the currently "active" 
 * resource.
 * Maybe sme of this should be moved to component_uml.sml... 
 *) 
fun atomic_actions_from_context env =
    if Option.isSome (curAttribute env) then
        let fun make_action s = 
                ComponentUML.SimpleAction (s, ComponentUMLResource.EntityAttribute 
                                                  (Option.valOf (curAttribute env)))
        in [make_action "read", make_action "update"] end
    else if Option.isSome (curOperation env) then
        let fun make_action s = 
                ComponentUML.SimpleAction (s, ComponentUMLResource.EntityMethod 
                                                  (Option.valOf (curOperation env)))
        in [make_action "execute"] end
    else if Option.isSome (curClassifier env) then
        let fun make_action s =
                ComponentUML.SimpleAction (s, ComponentUMLResource.Entity 
                                                  (Option.valOf (curClassifier env)))
        in [make_action "create", make_action "delete"] end
    else raise Fail "no current resource"
         
(* FIX *)
fun permissions_for_action env act = 
    List.filter (fn x => SuperCart.Security.permission_includes_action x act)
                (SuperCart.PermissionSet (unpack env))
                
(********** ADDING/MODIFYING VARIABLE SUBSTITUTIONS *****************************************)
(*	lookup  environment -> string -> string			
 * might override some lookup entries of the base cartridge 
 *)

fun lookup (env:environment) "permission_name" = 
    (case #curPermission env of 
         SOME x => #name x
       | NONE => SuperCart.lookup (unpack env) "permission_name")
  | lookup env "entity_name" =
    (case #curEntity env
      of SOME s => Rep.short_name_of s
       | NONE => SuperCart.lookup (unpack env) "entity_name") 
  | lookup env s =  SuperCart.lookup (unpack env) s 

(********** ADDING IF-CONDITION TYPE *****************************************)
(** no cartridge specific predicates are defined (yet). *)
fun test env "first_permission" = 
    (case #curPermission env of 
        SOME x => x = hd (Option.valOf (#curPermissionList env)) 
      | NONE   => SuperCart.test (unpack env) "first_permission" )
  | test env "last_permission" = 
    (case #curPermission env of 
        SOME x => x = List.last (Option.valOf (#curPermissionList env)) 
      | NONE   => SuperCart.test (unpack env) "first_permission" )
  | test env s = SuperCart.test (unpack env) s


(********** ADDING FOREACH TYPE **********************************************)

fun foreach_permission env name = 
    let val action = Option.valOf (List.find (fn  x => ComponentUML.action_type_of x = name) 
                                             (atomic_actions_from_context env))
        val permissions = permissions_for_action env action
        fun env_from_list_item c = { curPermissionList = SOME permissions,
                                     curPermission = SOME c,
                                     curEntity = #curEntity env,
                                     extension = #extension env} : environment
    in 
        List.map env_from_list_item permissions
    end
        
fun foreach_entity (env:environment)  =
    let val entities = List.filter (fn x => ListEq.includes (Rep.stereotypes_of x) "compuml.entity" )
                                        (#1 (#model (#extension env)))
        fun env_from_list_item c = { curPermissionList = #curPermissionList env,
                                     curPermission = #curPermission env,
                                     curEntity = SOME c,
                                     extension = #extension env}:environment
    in 
        List.map env_from_list_item entities
    end

fun foreach "readPermission_list" env = foreach_permission env "read"
  | foreach "updatePermission_list" env = foreach_permission env "update"
  | foreach "createPermission_list" env = foreach_permission env "create"
  | foreach "deletePermission_list" env = foreach_permission env "delete"
  | foreach "executePermission_list" env = foreach_permission env "execute"
  | foreach "entity_list" env = foreach_entity env
  | foreach listType env =  map (pack env) (SuperCart.foreach listType (unpack env))
                            
end
