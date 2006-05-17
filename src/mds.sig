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
    
    eqtype Resource

    (* val resource_types: string list *)

    val contained_resources : Resource -> Resource list

					  
    datatype Action = SimpleAction of string * Resource
                    | CompositeAction of string * Resource
					 
    (** list of allowed stereotype names on attributes in permission classes. *)
	val action_stereotypes : string list

    (** list of allowed stereotype names on classifiers to denote root resources. *)
    val root_stereotypes: string list

    (* val action_names: string list *)

    val subordinated_actions:   Action -> Action list

    val actions_of : Resource -> Action list
    val resource_of:   Action -> Resource
    val action_type_of : Action -> string

	(** 
	 * parse a permission attribute into an action.
	 * Takes the root resource, and the attribute as argument  
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

    val parse: Rep_Core.Classifier list -> 
			   (Rep_Core.Classifier list * Configuration)
end 	


