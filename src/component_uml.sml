(*****************************************************************************
 *          su4sml - a SecureUML repository for SML              
 *                                                                            
 * component_uml.sml - a design language implementing mds.sig for 
 *                     component-based systems
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

signature COMPONENTUML = 
sig
	
	datatype Resource = Entity of Rep.Classifier
					  | EntityMethod of Rep.operation
					  | EntityAttribute of Rep.attribute

	val contained_resources : Resource -> Resource list
										  
										  
    datatype Action = SimpleAction of string * Resource
				    | CompositeAction of string * Resource

	val action_stereotypes : string list
    (* val action_names: string list *)
														 
    val subordinated_actions:   Action -> Action list
										  
    val actions_of : Resource -> Action list
    val resource_of:   Action -> Resource

	(** 
	 * parse a permission attribute into an action.
	 * Takes the root resource, the attribute's stereotype, 
	 * the attribute's name and the attribute's type as argument 
	 *)
	val parse_action: Rep.Classifier -> string -> string -> string -> Action
								 
end

(** ComponentUML is a simple language for component-based modeling. *)
structure ComponentUML : COMPONENTUML =
struct

(** The type of resource, plus a path name specifiying the resource. 
 * Resource types can be entities, methods, and attributes.
 * FIX: using Path for methods is unsafe, there can be severable  
 * methods with the same name, but different signature.           
 *)
datatype Resource = Entity of Rep.Classifier
	      		  | EntityMethod of Rep.operation
			      | EntityAttribute of Rep.attribute

(* val resource_types = ["Entity","EntityMethod","EntityAttribute"] *)

val action_stereotypes = ["EntityAction","EntityMethodAction","EntityAttributeAction"]

(** The resources that are contained in the given resource.
 * does nothing sensible yet, but perhaps you get the idea...
 * FIXME: do something sensible 
 *)
fun contained_resources (Entity (Rep.Class c)) = List.concat [map EntityMethod (#operations c),
												  map EntityAttribute (#attributes c)] 
(** The list of all attributes of an entity.
 *)
fun entity_contained_attributes (Entity c)     = nil

(** The list of all side-effect free methods of an entity.
 *)
fun entity_contained_read_methods (Entity c)   = nil

(** The list of all methods with side-effects of an entity *)
fun entity_contained_update_methods (Entity c) = nil

datatype Action = SimpleAction of string * Resource
                | CompositeAction of string * Resource

(* FIX: also parse method and attribute actions. *)
fun parse_action root "EntityAction" name "read" = CompositeAction ("read", (Entity root)) 
  | parse_action root "EntityAction" name "update" = CompositeAction ("update", (Entity root)) 
  | parse_action root "EntityAction" name "create" = SimpleAction ("create", (Entity root)) 
  | parse_action root "EntityAction" name "delete" = SimpleAction ("delete", (Entity root)) 

fun actionType_of (SimpleAction (t,_)) = t
 |  actionType_of (CompositeAction (t,_)) = t

(* val action_names = ["create","read","update","delete","full_access","execute"] *)

(** The actions possible on the given resource. *)
fun actions_of (e as (Entity c)) = [SimpleAction    ("create",    e),
				    CompositeAction ("read",      e),
				    CompositeAction ("update",    e),
				    SimpleAction    ("delete",    e),
				    CompositeAction ("full_access",e)]
  | actions_of (m as (EntityMethod p)) = [SimpleAction ("execute", m)]
  | actions_of (a as (EntityAttribute p)) = [SimpleAction    ("read",       a),
						SimpleAction    ("update",     a),
						CompositeAction ("full_access", a)]

(** The resource an action acts on. *)
fun resource_of (SimpleAction x)    = #2 x
  | resource_of (CompositeAction x) = #2 x

(** The list of actions a composite actions consists of. *)
fun subordinated_actions (SimpleAction _) = nil
  | subordinated_actions (CompositeAction ("read",         e as (Entity c))) = 
    let val read_attributes = List.map (fn x => SimpleAction ("read", x)) 
				       (entity_contained_attributes e)
	val read_methods = List.map (fn x => SimpleAction ("execute",x)) 
				    (entity_contained_read_methods e)
    in 
	List.concat [read_attributes,read_methods]
    end 
  | subordinated_actions (CompositeAction ("full_access",  e as (Entity c)))
    = [SimpleAction ("create",e), 
       CompositeAction ("read",e),
       CompositeAction ("update",e),
       SimpleAction ("delete",e)]
  | subordinated_actions (CompositeAction ("update",       e as (Entity c))) =
    let val update_attributes = List.map (fn x => SimpleAction ("update", x)) 
				       (entity_contained_attributes e)
	val update_methods = List.map (fn x => SimpleAction ("execute",x)) 
				    (entity_contained_update_methods e)
    in 
	List.concat [update_attributes,update_methods]
	end
  | subordinated_actions (CompositeAction ("full_access", a as (EntityAttribute ae)))
    = [SimpleAction ("read", a),
       SimpleAction ("update", a)]
end
