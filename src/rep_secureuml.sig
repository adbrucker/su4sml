signature REP_SECUREUML =
sig

type Subject


type Role
type RoleAssignment = (Subject * Role) list
type RoleHierarchy = (Role * Role) list

type Resource 
type ActionName 
type ProtectedAction 
type Permission


end
