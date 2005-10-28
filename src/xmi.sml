(*****************************************************************************
 *          su4sml - a SecureUML repository for SML              
 *                                                                            
 * xmi_umlcore.sig - XMI-UML-Core datatypes for the import interface for su4sml
 * Copyright (C) 2005  Achim D. Brucker <brucker@inf.ethz.ch>
 *                     Jürgen Doser <doserj@inf.ethz.ch>
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


(* ---------------------------------------------------------------------------
 * The types in these structures are supposed to define a representation of 
 * the XML elements found in UML-XMI files. It is reasonably close to the UML 
 * metamodel and the XMI representation of it, while simplifying some kinds 
 * of references.
 * --------------------------------------------------------------------------*)

structure XMI_ModelManagement =
(* from UML 1.5 Model Management Overview: ------------------------------------
 * The Model Management package is dependent on the Foundation package. It 
 * defines Model, Package, and Subsystem, which all serve as grouping units 
 * for other ModelElements.
 * Models are used to capture different views of a physical system. Packages 
 * are used within a Model to group ModelElements. A Subsystem represents a 
 * behavioral unit in the physical system. UML Profiles are packages 
 * dedicated to group UML extensions.
 * --------------------------------------------------------------------------*)
struct
open XMI_Core XMI_ActivityGraphs
(* from UML 1.5 Model Management: --------------------------------------------
 * A package is a grouping of model elements.
 * [...]
 * A Package may only own or reference Packages, Classifiers, Associations,
 * Generalizations, Dependencies, Comments, Constraints, Collaborations, 
 * StateMachines, Stereotypes, and TaggedValues.
 * --------------------------------------------------------------------------*)
(* We treat "Model" the same way as a "Package".                             *)
datatype Package = Package of { xmiid          : string,
				name           : string,
				visibility     : VisibilityKind,
				packages       : Package list,
				classifiers    : Classifier list,
                                state_machines : StateMachine list,
                                activity_graphs: ActivityGraph list,
				associations   : Association list,
				generalizations: Generalization list,
				constraints    : Constraint list,
				stereotypes    : Stereotype list, (* contained stereotype definitions *)
				dependencies   : Dependency list,
				tag_definitions: TagDefinition list,
				stereotype     : string list, (* idref to stereotype of this package *)
				taggedValue    : TaggedValue list,
				events: Event list}
		  
end



structure XMI =
struct
open XMI_Core XMI_ActivityGraphs XMI_ModelManagement

(* There may be (are) model elements outside of the UML model,
   due to errors in the Dresden Package.
   The only relevant Xmi Content is the head of the
   package list. *)
type XmiContent = {classifiers:           Classifier list,
		   constraints:           Constraint list,
		   packages   :           Package list,
		   stereotypes:           Stereotype list,
		   variable_declarations: VariableDeclaration list,
                   state_machines  :      StateMachine list,
                   activity_graphs :      ActivityGraph list
                  }

end
