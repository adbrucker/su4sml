(*****************************************************************************
 *          su4sml - a SecureUML repository for SML              
 *                                                                            
 * mds.sig - signatures for design and security languages
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


