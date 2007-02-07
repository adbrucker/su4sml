(*****************************************************************************
 *          su4sml - a SecureUML repository for SML              
 *                                                                            
 * secure_uml.sml - a security language implementing mds.sig
 * Copyright (C) 2005 Achim D. Brucker <brucker@inf.ethz.ch>   
 *                    Juergen Doser    <doserj@inf.ethz.ch>
 *                    Burkhart Wolff   <bwolff@inf.ethz.ch>
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

(** 
 * SecureUML is a simple security language based on RBAC.
 * Permissions relate roles with actions and can be further constrained 
 * using OCL: 
 *) 
signature SECUREUML = 
sig 
    include    SECURITY_LANGUAGE
type Role
type Subject
val all_roles :      Configuration -> Role list 
val all_constraints: Configuration -> Rep_OclTerm.OclTerm list
val all_subjects:    Configuration -> Subject list
val constraints_of : Permission -> Rep_OclTerm.OclTerm list
val roles_of:        Permission -> Role list 
val subject_name_of: Subject -> string
val subject_roles_of:Subject -> Configuration -> Role list
     
end


(** 
 * SecureUML is a simple security language based on RBAC.
 * Permissions relate roles with actions and can be further constrained 
 * using OCL: 
 *) 
functor SecureUML(structure Design: DESIGN_LANGUAGE):SECUREUML =
struct
open library
structure Design : DESIGN_LANGUAGE = Design

type User = string
fun name_of (u:User) = u

datatype Subject = Group of string * (string list)
                 | User of User

fun subject_name_of (Group (g,_)) = g
  | subject_name_of (User u)      = u

type Role = string

type SubjectAssignment = (Subject * (Role list)) list

                         
type Permission = {name: string,
                   roles: Role list,
                   constraints: Rep_OclTerm.OclTerm list,
                   actions: Design.Action list }

type Config_Type = string

type 'a partial_order = ('a * 'a) list

type Configuration = { config_type: Config_Type,
                       permissions: Permission list,
                       subjects: Subject list,
                       (* groups: Group partial_order,*)
                       roles: Role list,
                       rh: Role partial_order,
                       sa: SubjectAssignment }

fun subject_roles_of (s:Subject) (c:Configuration) = 
    (snd o valOf o (List.find (fn x => (fst x) = s)) o #sa) c

fun constraints_of (x:Permission) = #constraints x
fun roles_of       (x:Permission) = #roles x
fun actions_of     (p:Permission) = #actions p
fun all_roles   (c:Configuration) = #roles c
fun all_subjects (c:Configuration)= #subjects c
fun all_constraints (c:Configuration) = List.concat (List.map constraints_of (#permissions c))

(** test whether a1 is (transitively) a subordinated_action of a2 *)
fun is_contained_in a1 a2 = (a1 = a2) orelse 
                            List.exists (is_contained_in a1) 
                                        (Design.subordinated_actions a2)

(** test whether the permission p covers the action a. *)
fun permission_includes_action (p:Permission) (a:Design.Action) = 
    List.exists (is_contained_in a) (#actions p)


(* unclear yet how this will look like: 
 fun domain_of (x:'a partial_order) = ...
 fun closure_of (x:'a partial_order) = ...
 *) 

fun type_of  (c:Configuration) = #config_type c

fun is_empty (c:Configuration) = List.null (#permissions c) andalso 
                                 List.null (#subjects c)

fun getPermissions (c:Configuration) = #permissions c

(* the following functions have yet to be implemented *)
fun users_of p = nil
fun check_permission (u,p) = false
fun permissions_of u = nil


(** checks whether the classifier c has the stereotype s.
 * (could be moved to rep_core?)
 *)
fun classifier_has_stereotype s c = ListEq.includes (Rep.stereotypes_of c) s

(** checks whether the classifier c has none of the given stereotypes *)
fun classifier_has_no_stereotype strings c = 
    ListEq.disjunct strings (Rep.stereotypes_of c)
    

(** checks whether the classifier c has a parent.
 * (could be moved to rep_core?)
 *)
fun classifier_has_parent (Rep.Class c)       = Option.isSome (#parent c)
  | classifier_has_parent (Rep.Interface c)   = not (List.null (#parents c))
  | classifier_has_parent (Rep.Enumeration c) = Option.isSome (#parent c)
  | classifier_has_parent (Rep.Primitive c)   = Option.isSome (#parent c)
  | classifier_has_parent (Rep.Template c)    = classifier_has_parent (#classifier c)

fun filter_permission cs = List.filter (classifier_has_stereotype 
                                            "secuml.permission") cs
(* FIXME: handle groups also *)
fun filter_subject cs = List.filter (classifier_has_stereotype "secuml.user") cs
fun filter_role cs = List.filter (classifier_has_stereotype "secuml.role") cs 

                     
fun mkRole (C as Rep.Class c) = Rep.string_of_path (Rep.name_of C)
  | mkRole _                  = error ("in mkRole: argument is not a class")

(* FIXME: handle groups also *)
fun mkSubject (C as Rep.Class c) = User (Rep.string_of_path (Rep.name_of C))
  | mkSubject _                  = error ("in mkSubject: argument is not a class")

fun mkPermission cs (c as Rep.Class _) = 
    let val classifiers = (Rep.connected_classifiers_of c cs)
        val role_classes = List.filter (classifier_has_stereotype "secuml.role") 
                                       classifiers
        val root_classes =   List.filter (fn x => ListEq.overlaps 
                                                      (Rep.stereotypes_of x)
                                                      Design.root_stereotypes)
                                         classifiers
        val root_resource = hd root_classes
            handle Empty => error ("in mkPermission: no root resource found "^
                                   "for permission "^Rep.string_of_path (Rep.name_of c))
        val action_attributes = 
            List.filter (fn x => ListEq.overlaps (#stereotypes x) (Design.action_stereotypes)) 
                        (Rep.attributes_of c)
            handle ex => (error_msg "could not parse permission attributes"; raise ex)
    in 
        { name  = (Rep.string_of_path (Rep.name_of c)),
          roles = (map (Rep.string_of_path o Rep.name_of) role_classes),
          (* FIXME: find attached constraints *)
          constraints = nil, 
          actions = if action_attributes = [] 
                    then error ("in mkPermission: Permission "^
                                (Rep.string_of_path (Rep.name_of c))^
                                "has no action attributes")
                    else map (Design.parse_action root_resource) action_attributes }
    end
  | mkPermission _ _ = error "in mkPermission: argument is not a class"
                       

fun mkSubjectAssignment cs (c as (Rep.Class _)) = 
    let (* FIXME: we just take all roles that are connected to the subject. *)
        (* in principle, we should check the stereotype of the association, *)
        (* but that does not exist in the rep datastructure...              *)  
        val classifiers = List.filter (classifier_has_stereotype "secuml.role")
                                      (Rep.connected_classifiers_of c cs)
    in 
        (mkSubject c, map mkRole classifiers)
    end
                       
(** parse a list of classifiers accoriding to the SecureUML profile.
 * removes the classes with SecureUML stereotypes. 
 *)
fun parse (cs:Rep_Core.Classifier list) = 
    let val _ = info "parsing security configuration"
    in 
        (List.filter (classifier_has_no_stereotype ["secuml.permission",
                                                    "secuml.role",
                                                    "secuml.subject",
                                                    "secuml.actiontype"]) 
                     cs,
         { config_type = "SecureUML",
           permissions = map (mkPermission cs) (filter_permission cs),
           subjects    = map mkSubject (filter_subject cs),
           roles       = map mkRole (filter_role cs),
           rh          = map (fn x => (Rep.string_of_path (Rep.name_of x),
                                       Rep.string_of_path (Rep.parent_name_of x)))
                             (List.filter classifier_has_parent (filter_role cs)),
           sa          = map (mkSubjectAssignment cs) (filter_subject cs)})
    end
    handle ex => (error_msg "in SecureUML.parse: security configuration \
                            \could not be parsed";
                  raise ex)


end



