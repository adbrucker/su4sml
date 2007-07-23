(*****************************************************************************
 * su4sml --- a SML repository for managing (Secure)UML/OCL models
 *             http://projects.brucker.ch/su4sml/
 *                                                                            
 * mds.sig --- signatures for design and security languages
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
 * a design language specifies what the protected resources and the 
 * possible actions on these resources are.                          
 *)
signature DESIGN_LANGUAGE =
sig	  
    
    (** the concrete resource types of the design modelling language. *)
    eqtype Resource

    (** 
     * the resource hierarchy.
     * give the list of resources the given resource contains.
     * only returns the immediatlye contained resources, not the transitive 
     * closure.
     *)
    val contained_resources : Resource -> Resource list
					  
    (** *)
    datatype Action = SimpleAction of string * Resource
                    | CompositeAction of string * Resource
					 
    (** list of allowed stereotype names on attributes in permission classes. *)
	val action_stereotypes : string list

    (** list of allowed stereotype names on classifiers to denote root resources. *)
    val root_stereotypes: string list

    (** 
     * the action hierarchy. 
     * give the list of actions the given composite action is composed of.
     * only returns the immediatly subordinated actions, not the transitive 
     * closure. If the given action is a simple action, returns an empty list
     *)
    val subordinated_actions:   Action -> Action list

    (** 
     * the list of actions that are possible on the given resource.
     * (actually not really needed currently, but might come in handy)
     *)
    val actions_of : Resource -> Action list
    
    (** the resource the given actions acts on *)
    val resource_of:   Action -> Resource

    (** the action type of the given action *)
    val action_type_of : Action -> string

	(** 
	 * parse a permission attribute into an action.
	 * Takes the root resource, and the attribute as argument.  
	 *)
	val parse_action: Rep.Classifier -> Rep.attribute -> Action
end 
    
    
(**
 * A security language speaks about users, and their permissions.
 * at this level, this is completeley independent of the access   
 * control model used in the application                          
 *)
signature SECURITY_LANGUAGE =
sig
    structure Design : DESIGN_LANGUAGE 

    
    type Configuration
    type Config_Type = string
    eqtype Permission	

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

    val is_contained_in : Design.Action -> Design.Action -> bool
    val permission_includes_action : Permission -> Design.Action -> bool

    (** 
     * parse a UML model and return a (modified) list of classes and the 
     * recognized security configuration. 
     *)
    val parse: Rep_Core.Classifier list -> 
			   (Rep_Core.Classifier list * Configuration)
end 	


