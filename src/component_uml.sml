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

(** Auxiliary structure to specialize the resource type for ComponentUML. *)
structure ComponentUMLResource = struct

(** The type of resource, plus a path name specifiying the resource. 
 * Resource types can be entities, methods, and attributes.
 * FIX: using Path for methods is unsafe, there can be severable  
 * methods with the same name, but different signature.           
 *)
 datatype Resource = Entity of Rep.Classifier
					  | EntityMethod of Rep.operation
					  | EntityAttribute of Rep.attribute
end

(** The signature for ComponentUML. *)
signature COMPONENTUML = DESIGN_LANGUAGE where 
type Resource = ComponentUMLResource.Resource

(** ComponentUML is a simple language for component-based modeling. *)
structure ComponentUML : COMPONENTUML =
struct

open ComponentUMLResource
(* val resource_types = ["Entity","EntityMethod","EntityAttribute"] *)

val action_stereotypes = ["dialect.entityaction",
                          "dialect.entitymethodaction",
                          "dialect.entityattributeaction"]

val root_stereotypes = ["compuml.entity"]

(** The list of all attributes of an entity. *)
fun entity_contained_attributes (Entity c) =
    map EntityAttribute (Rep.attributes_of c)
  | entity_contained_attributes _ = library.error "entity_contained_attributes called on something that is not an entity"

(** the list of all methods of an entity *)
fun entity_contained_methods (Entity c) = map EntityMethod (Rep.operations_of c)
  | entity_contained_methods _ = library.error "entity_contained_methods called on something that is not an entity"

(** The list of all side-effect free methods of an entity. *)
fun entity_contained_read_methods (Entity c) =
    map EntityMethod (List.filter #isQuery (Rep.operations_of c))
  | entity_contained_read_methods _ = library.error "entity_contained_read_methods called on something that is not an entity"
		
(** The list of all methods with side-effects of an entity *)
fun entity_contained_update_methods (Entity c) =
    map EntityMethod (List.filter (not o #isQuery) (Rep.operations_of c))
  | entity_contained_update_methods _ = library.error "entity_contained_update_methods called on something that is not an entity"

(** The resources that are contained in the given resource. *)
fun contained_resources x = 
	List.concat [entity_contained_attributes x, entity_contained_methods x] 
							

datatype Action = SimpleAction of string * Resource
                | CompositeAction of string * Resource


(** parses an entity action permission attribute. *)
fun parse_entity_action root att_name "create"     = 
	SimpleAction ("create", (Entity root)) 
  | parse_entity_action root att_name "read"       = 
	CompositeAction ("read", (Entity root)) 
  | parse_entity_action root att_name "update"     =
	CompositeAction ("update", (Entity root)) 
  | parse_entity_action root att_name "delete"     =
	SimpleAction ("delete", (Entity root)) 
  | parse_entity_action root att_name "fullaccess" =
	CompositeAction ("fullaccess", (Entity root)) 	
  | parse_entity_action root att_name s = library.error ("unknown action type "^s^
                                                         " for entity action")
	
(** parses an entity attribute action permission attribute. *)
fun parse_attribute_action root name "read"       =
	(SimpleAction ("read", 
                   (EntityAttribute ((hd o List.filter (fn x => #name x = name)) 
										(Rep.attributes_of root))))
     handle Empty => library.error "did not find attribute")
  | parse_attribute_action root name "update"     =
    ( SimpleAction ("update", 
                    (EntityAttribute ((hd o List.filter (fn x => #name x = name)) 
                                          (Rep.attributes_of root))))
      handle Empty => library.error "did not find attribute")
  | parse_attribute_action root name "fullaccess" =
    ( CompositeAction ("fullaccess", 
                       (EntityAttribute ((hd o List.filter (fn x => #name x = name)) 
                                             (Rep.attributes_of root))))
      handle Empty => library.error "did not find attribute")
  | parse_attribute_action root name s = library.error ("unknown action type "^s^
                                                        "for attribute action")

(** parses an entity method action permission attribute. *)
fun parse_method_action root name "execute" 
  = (SimpleAction ("execute", 
                   (EntityMethod ((hd o List.filter (fn x => #name x = name)) 
                                      (Rep.operations_of root))))
     handle Empty => library.error "did not find method")
  | parse_method_action roor name s = library.error ("unknown action type "^s^
                                                     "for method action")

(**
 * parses a permission attribute according to the ComponentUML 
 * dialect for SecureUML. 
 *)
fun parse_action root (att:Rep.attribute) =
	let val att_name = #name att
		val att_type = #attr_type att
        val cls_path = case att_type of Rep_OclType.Classifier x => x
                                      | _ => library.error "permission attribute type is not a classifier"
		val action_name = hd (rev cls_path) 
        fun resource_path name = (hd o List.tl) (String.tokens (fn x => x= #".") name)
	in case hd (#stereotypes att) 
		of "dialect.entityaction" => 
           parse_entity_action root att_name action_name
		 | "dialect.entitymethodaction" => 
           parse_method_action root (resource_path att_name) action_name
		 | "dialect.entityattributeaction" => 
           parse_attribute_action root (resource_path att_name) action_name 
		 | s => library.error ("in ComponentUML.parse_action: "^
							   "found unexpected stereotype "^s^
							   " for permission attribute")
	end
		handle _ => library.error "in ComponentUML.parse_action: could not parse attribute"

fun action_type_of (SimpleAction (t,_)) = t
  | action_type_of (CompositeAction (t,_)) = t
                                              
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
  | subordinated_actions (CompositeAction _) = library.error "encountered unknown composite action type in subordinated_actions"
end
