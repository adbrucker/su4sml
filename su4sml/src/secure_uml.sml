(*****************************************************************************
 * su4sml --- a SML repository for managing (Secure)UML/OCL models
 *             http://projects.brucker.ch/su4sml/
 *                                                                            
 * secure_uml.sml --- a security language implementing mds.sig
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
  | classifier_has_parent (Rep.AssociationClass c) = Option.isSome (#parent c)
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
fun mkPermission (cs,ascs) (c as Rep.Class _) = 
    let val classifiers = (Rep.connected_classifiers_of ascs c cs)
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
                       

fun mkSubjectAssignment (cs,ascs) (c as (Rep.Class _)) = 
    let (* FIXME: we just take all roles that are connected to the subject. *)
        (* in principle, we should check the stereotype of the association, *)
        (* but that does not exist in the rep datastructure...              *)  
        val classifiers = List.filter (classifier_has_stereotype "secuml.role")
                                      (Rep.connected_classifiers_of ascs c cs)
    in 
        (mkSubject c, map mkRole classifiers)
    end
                       
(*                     
fun update_aends (Rep.Class { name, parent,attributes,operations,associations,
			      invariant, stereotypes,interfaces,thyname,activity_graphs}) aends = 
    let
	(* 
	 *)
	fun assoc_of_aend ({name,...}:Rep_Core.associationend) = List.take (name,List.length name -1 )
	val assocs = map assoc_of_aend aends
	fun is_member ({name,...}:Rep_Core.association) = List.exists (fn x => x=name) assocs
    in
	Rep.Class {name=name, parent=parent, attributes=attributes,
		   operations=operations(*, associations=map assoc_of_aend aends*),
		   associations = filter is_member associations,
									invariant=invariant, stereotypes=stereotypes,
									interfaces=interfaces, thyname=thyname,
									activity_graphs=activity_graphs}
    end
  | update_aends (Rep.AssociationClass { name, parent,attributes,operations,associations,association,
					 invariant, stereotypes,interfaces,thyname,activity_graphs}) aends = 
    let
	fun assoc_of_aend ({name,...}:Rep_Core.associationend) = List.take (name,List.length name -1 )
	val assocs = map assoc_of_aend aends
	fun is_member ({name,...}:Rep_Core.association) = List.exists (fn x => x=name) assocs
    in
	Rep.AssociationClass {name=name, parent=parent, attributes=attributes,
			      operations=operations(*, associations=map assoc_of_aend aends*),
			      associations = filter is_member associations,
			      association = association (* FIXME: proper handling? *),
			      invariant=invariant, stereotypes=stereotypes,
			      interfaces=interfaces, thyname=thyname,
			      activity_graphs=activity_graphs}
    end
*)
fun update_assocs (Rep.Class { name, parent,attributes,operations,associations,
			      invariant, stereotypes,interfaces,thyname,activity_graphs}) assocs = 
    Rep.Class {name=name, parent=parent, attributes=attributes,
	       operations=operations, associations=assocs,
	       invariant=invariant, stereotypes=stereotypes,
	       interfaces=interfaces, thyname=thyname,
	       activity_graphs=activity_graphs}
  | update_assocs (Rep.AssociationClass { name, parent,attributes,operations,associations,association,
					  invariant, stereotypes,interfaces,thyname,activity_graphs}) assocs = 
    Rep.AssociationClass {name=name, parent=parent, attributes=attributes,
			  operations=operations, associations=assocs,association=association,
			  invariant=invariant, stereotypes=stereotypes,
			  interfaces=interfaces, thyname=thyname,
			  activity_graphs=activity_graphs}
	
	
fun assocConnectsToSecureUml cs (a:Rep.associationend) = 
	classifier_has_no_stereotype ["secuml.permission",
								  "secuml.role",
								  "secuml.user"]
								 (Rep.class_of (Rep_OclType.path_of_OclType (#aend_type a)) cs)

(** remove aends from classifiers to permissions and roles.
 * Actual generation of permissions and roles is done in parse below.
 *)
fun removeSecureUmlAends (Rep.Class {name=class_name,...},(assocs,removed_assocs)):(Rep.association list * Rep.association list) = 
    let
	fun remove_aend ({name,aclass,aends}:Rep.association):Rep.association = 
	    {name = name,
	     aclass = aclass,
	     aends = filter (fn {aend_type,...} => not (aend_type = class_name)) aends
	    }
	fun non_emtpy ({aends,...}:Rep.association) = List.length aends >= 2 (* FIXME: reflexive association -> 2 aends? *)
	val reduced_assocs = map remove_aend assocs
	val (modified_assocs,newly_removed_assocs) = List.partition non_emtpy reduced_assocs
    in
	(modified_assocs,newly_removed_assocs @ removed_assocs)
    end
  | removeSecureUmlAends (Rep.AssociationClass {name=class_name,...},(assocs,removed_assocs)):(Rep.association list * Rep.association list) = 
    let
	fun remove_aend ({name,aclass,aends}:Rep.association):Rep.association = 
	    {name = name,
	     aclass = aclass,
	     aends = filter (fn {aend_type,...} => not (aend_type = class_name)) aends
	    }
	fun non_emtpy ({aends,...}:Rep.association) = List.length aends >= 2 (* FIXME: reflexive association -> 2 aends? *)
	val reduced_assocs = map remove_aend assocs
	val (modified_assocs,newly_removed_assocs) = List.partition non_emtpy reduced_assocs
	(* FIXME: proper handling for aclass? *)
    in
	(modified_assocs,newly_removed_assocs @ removed_assocs)
    end


(** parse a list of classifiers according to the SecureUML profile.
 * removes the classes with SecureUML stereotypes. 
 *)
fun parse (model as (cs,assocs):Rep.Model) = 
    let val _ = info "parsing  security configuration"
	val non_secureumlstereotypes = List.filter (classifier_has_no_stereotype ["secuml.permission",
										  "secuml.role",
										  "secuml.subject",
										  "secuml.actiontype"])  cs
	val secureumlstereotypes = List.filter (classifier_has_no_stereotype ["secuml.permission",
									      "secuml.role",
									      "secuml.user"]) cs 
	(* remove classes with SecureUML stereotypes from the association list
	 * and update affected classes if the association ceases to exist 
	 *)
	fun updateClassifierAssociations rem_assocs (Rep.Class {name, parent, attributes, operations,
								associations, invariant, stereotypes,
								interfaces, thyname, activity_graphs}) =
	    let
		val assoc_names = map (fn {name,aends,aclass} => name) rem_assocs
		fun non_emtpy path = not (List.exists (fn aname => aname = path) assoc_names)
    in 
		Rep.Class {name = name, 
			   parent = parent, 
			   attributes = attributes, 
			   operations = operations,
			   associations = filter non_emtpy associations, 
			   invariant = invariant, 
			   stereotypes = stereotypes,
			   interfaces = interfaces, 
			   thyname = thyname, 
			   activity_graphs = activity_graphs
			  }
	    end
	  | updateClassifierAssociations rem_assocs (Rep.AssociationClass {name, parent, attributes, 
									   operations, associations, 
									   association, invariant, 
									   stereotypes, interfaces, 
									   thyname, activity_graphs}) =
	    let
		val assoc_names = map (fn {name,aends,aclass} => name) rem_assocs
		fun non_emtpy path = not (List.exists (fn aname => aname = path) assoc_names)
	    in
		Rep.AssociationClass {name = name, 
				      parent = parent, 
				      attributes = attributes, 
				      operations = operations,
				      associations = filter non_emtpy associations, 
				      association = association (* FIXME: proper handling? *),
				      invariant = invariant, 
				      stereotypes = stereotypes,
				      interfaces = interfaces, 
				      thyname = thyname, 
				      activity_graphs = activity_graphs
			  }
	    end
	    
	val (modified_assocs,removed_assocs) = case secureumlstereotypes of [] => (assocs,[])
									  | xs => foldl removeSecureUmlAends (assocs,[]) xs
	val modified_classifiers = case removed_assocs of [] => non_secureumlstereotypes
							| xs => map (updateClassifierAssociations xs) non_secureumlstereotypes
    in 
        (
(*	 map (removeSecureUmlAends cs)
			 (List.filter (classifier_has_no_stereotype ["secuml.permission",
														 "secuml.role",
														 "secuml.subject",
														 "secuml.actiontype"]) 
						  cs),
 *)	 (modified_classifiers,modified_assocs),
         { config_type = "SecureUML",
           permissions = map (mkPermission model) (filter_permission cs),
           subjects    = map mkSubject (filter_subject cs),
           roles       = map mkRole (filter_role cs),
           rh          = map (fn x => (Rep.string_of_path (Rep.name_of x),
                                       Rep.string_of_path (Rep.parent_name_of x)))
                             (List.filter classifier_has_parent (filter_role cs)),
           sa          = map (mkSubjectAssignment model) (filter_subject cs)})
    end
    handle ex => (error_msg "in SecureUML.parse: security configuration \
                            \could not be parsed";
                  raise ex)


end



