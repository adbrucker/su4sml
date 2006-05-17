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

signature SECUREUML =
sig
    structure Design : DESIGN_LANGUAGE 

    
    type Configuration
    type Config_Type = string

    type Role = string
    type Permission	= {name: string,
                       roles: Role list,
                       constraints: Rep_OclTerm.OclTerm list,
                       actions: Design.Action list }

	val getPermissions : Configuration -> Permission list
    val type_of        : Configuration -> Config_Type
    val is_empty       : Configuration -> bool				   

    type User	
    val name_of         :              User -> string		
    
    (* a bit unclear, which of the following we really need *)
    val users_of        :        Permission -> User list	
    (* val permissions_of  :              User -> Permission list	*)
    val check_permission: User * Permission -> bool	  

    val actions_of      :        Permission -> Design.Action list
    val permissions_of  :     Design.Action -> Permission list

    val permission_includes_action : Permission -> Design.Action -> bool
    val parse: Rep_Core.Classifier list -> 
			   (Rep_Core.Classifier list * Configuration)

end

(** 
 * SecureUML is a simple security language based on RBAC.
 * Permissions relate roles with actions and can be further constrained 
 * using OCL: 
 *) 
functor SecureUML(structure Design: DESIGN_LANGUAGE):SECUREUML =
struct

structure Design : DESIGN_LANGUAGE = Design

type User = string
fun name_of (u:User) = u
 

datatype Subject = Group of string * (string list)
                 | User of User


type Role = string
type SubjectAssignment = (Subject * (Role list)) list

				 
type Permission = {name: string,
				   roles: Role list,
				   constraints: Rep_OclTerm.OclTerm list,
				   actions: Design.Action list }

fun actions_of (p:Permission) = #actions p

(** test whether a1 is (transitively) a subordinated_action of a2 *)
fun is_contained_in a1 a2 = (a1 = a2) orelse 
							List.exists (is_contained_in a1) 
                                                   (Design.subordinated_actions a2))) 

(** test whether the permission p covers the action a. *)
fun permission_includes_action (p:Permission) (a:Design.Action) = 
    List.exists (is_contained_in a) (#actions p)

type Config_Type = string

type 'a partial_order = ('a * 'a) list

(* unclear yet how this will look like: 
fun domain_of (x:'a partial_order) = ...
fun closure_of (x:'a partial_order) = ...
*) 

type Configuration = { config_type: Config_Type,
					   permissions: Permission list,
					   subjects: Subject list,
					   (* groups: Group partial_order,*)
					   roles: Role partial_order,
					   sa: SubjectAssignment }

fun type_of  (c:Configuration) = #config_type c

fun is_empty (c:Configuration) = List.null (#permissions c) andalso 
				 List.null (#subjects c)

fun getPermissions (c:Configuration) = #permissions c

(* the following functions have yet to be implemented *)
fun users_of p = nil
fun check_permission (u,p) = false
fun permissions_of u = nil


fun stereotypes_of (Rep.Class {stereotypes,...})       = stereotypes
  | stereotypes_of (Rep.Enumeration {stereotypes,...}) = stereotypes
  | stereotypes_of (Rep.Primitive {stereotypes,...})   = stereotypes
  | stereotypes_of (Rep.Interface {stereotypes,...})   = stereotypes


fun has_no_stereotype strings c = 
    not (List.exists (fn stereotype =>  List.exists (fn x => x = stereotype) 
						    strings) (stereotypes_of c))

fun has_stereotype string c = 
    List.exists (fn x => x=string) (stereotypes_of c)


fun filter_permission cs = List.filter (has_stereotype "secuml.permission") cs
(* FIXME: handle groups also *)
fun filter_subject cs = List.filter (has_stereotype "secuml.user") cs
fun filter_role cs = List.filter (has_stereotype "secuml.role") cs 

 
fun mkRole (Rep.Class c)  = Rep.string_of_path (#name c)
  | mkRole _ = library.error "mkRole called on something that is not a class"

(* FIXME: handle groups also *)
fun mkSubject (Rep.Class c) = User (Rep.string_of_path (#name c))
  | mkSubject _ = library.error "mkSubject called on something that is not a class"

fun classifier_has_stereotype s c = List.exists (fn x => x = s) 
												(Rep.stereotypes_of c)
fun mkPermission cs (Rep.Class c) =  (
	{ name  = (Rep.string_of_path (#name c)),
	  roles = (map (Rep.string_of_path o Rep.name_of)
				   (List.filter (classifier_has_stereotype "secuml.role") 
								(map (fn (Rep_OclType.Classifier p) => Rep.class_of p cs)
									 (List.filter Rep_OclType.is_Classifier
												  (map  #attr_type
														(Rep.attributes_of (Rep.Class c))))))),  
	  (* FIXME: find attached constraints *)
	  constraints = nil, 
	  actions = let 
		  val atts = Rep.attributes_of (Rep.Class c)
		  val root_resource = 
			  hd (List.filter (classifier_has_stereotype "compuml.entity") 
							  (map (fn (Rep_OclType.Classifier p) => 
									   Rep.class_of p cs)
								   (List.filter Rep_OclType.is_Classifier
												(map  #attr_type
													  atts))))
			  handle _ => library.error ("could not find root resource "^
										 "for class "^(Rep.string_of_path (#name c)))
		  val action_attributes = 
			  List.filter (fn x => List.exists 
									   (fn y => List.exists 
													(fn z => y= z) 
													(#stereotypes x)) 
									   Design.action_stereotypes) atts 
			  handle _ => library.error "could not parse permission attributes"
	  in 
          if action_attributes = [] 
          then library.error ("no action attributes found in permission "^
                              (Rep.string_of_path (#name c)))
          else map (Design.parse_action root_resource) action_attributes
	  end }
	handle _ => library.error "error in mkPermission" )
  | mkPermission _ _ = library.error "mkPermission called on something that is not a class"

(* FIXME *) 
fun mkPartialOrder xs = ListPair.zip (xs,xs)

fun parse (cs:Rep_Core.Classifier list) = 
	(List.filter (has_no_stereotype ["secuml.permission","secuml.role","secuml.subject"]) cs,
	 { config_type = "SecureUML",
	   permissions = map (mkPermission cs) (filter_permission cs),
	   subjects    = map mkSubject (filter_subject cs),
	   roles       = mkPartialOrder (map mkRole (filter_role cs)),
	   (* FIXME: find associations between Users and Roles. *)
	   sa = nil})
	handle _ => library.error ("Problem during parsing security configuration")


end



