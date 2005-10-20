structure Rep_SecureUML : REP_SECUREUML =
struct
 
datatype Subject = User of string
	         | Group of Group
withtype Group = string * Subject list

(* perhaps we find the need for a more elaborate type later *)
type Role = string 

type RoleAssignment = (Subject * Role) list


type RoleHierarchy = (Role * Role) list

(* computes the reflexiv and transitive closure of rh starting from *)
(* the given role                                                   *)
(* fun inherited_roles rh role = ... *)

(* Resources according to ComponentUML. This will have to be adapted when we *)
(* support something like ControllerUML.                                     *)
datatype Resource = Entity of Rep_OclType.Path
	          | EntityMethod of Rep_OclType.Path
                  | EntityAttribute of Rep_OclType.Path
(* | EntityAssociationEnd of Rep.Path ??? *)


(* fun contained_resources e = ... *)

datatype ActionName = Create | Read | Update | Delete | FullAccess | Execute

datatype ProtectedAction = SimpleAction of ActionName * Resource 
		         | CompositeAction of ActionName * Resource

fun subordinated_actions (SimpleAction _) = nil
  | subordinated_actions (CompositeAction (Read,Entity c)) = nil
    (* let val read_attributes = ...
	val read_methods = ...
    in 
	List.concat [read_attributes,read_methods]
    end *)
(*  | subordinated_actions (CompositeAction (_,_)) = ...*)
    


type Permission = { name:        string,
		    roles:       Role list,
		    constraints: Rep_OclTerm.OclTerm list,
		    actions:     ProtectedAction list
		    }

end
