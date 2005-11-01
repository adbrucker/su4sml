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

structure ComponentUML : DESIGN_LANGUAGE =
struct

(* ComponentUML is a simple language for component-based modeling *)
(* It speaks about entities, methods, and attributes:             *)
(* FIX: using Path for methods is unsafe, there can be severable  *)
(* methods with the same name, but different signature.           *)
type Resource = string * Rep_OclType.Path

val resource_types = ["Entity","EntityMethod","EntityAttribute"]

(* does nothing sensible, but perhaps you get the idea...*)
fun contained_resources ("Entity",c) = nil



datatype Action = SimpleAction of string * Resource
                | CompositeAction of string * Resource

val action_names = ["create","read","update","delete","full_access","execute"]

(* not yet complete: *)
fun actions_of (e as ("Entity", c)) = [SimpleAction    ("create",    e),
				    CompositeAction ("read",      e),
				    CompositeAction ("update",    e),
				    SimpleAction    ("delete",    e),
				    CompositeAction ("full_access",e)]
  | actions_of (m as ("EntityMethod", p)) = [SimpleAction ("execute", m)]
  | actions_of (a as ("EntityAttribute", p)) = [SimpleAction    ("read",       a),
						SimpleAction    ("update",     a),
						CompositeAction ("full_access", a)]

fun resource_of (SimpleAction x)    = #2 x
  | resource_of (CompositeAction x) = #2 x

(* does nothing sensible, but perhaps you get the idea...*)
fun subordinated_actions (SimpleAction _) = nil
  | subordinated_actions (CompositeAction ("read",("Entity", c))) = 
    let val read_attributes = nil
	val read_methods = nil
    in 
	List.concat [read_attributes,read_methods]
    end 
  | subordinated_actions (CompositeAction _) = nil 
end
