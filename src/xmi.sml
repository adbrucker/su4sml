(*****************************************************************************
 * su4sml --- an SML repository for managing (Secure)UML/OCL models
 *             http://projects.brucker.ch/su4sml/
 *                                                                            
 * xmi.sml --- 
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
				events         : Event list}
		  
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
