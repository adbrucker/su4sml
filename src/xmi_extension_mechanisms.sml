(*****************************************************************************
 *          su4sml - a SecureUML repository for SML              
 *                                                                            
 * xmi_extensionmechanisms.sml - XMI-UML Extension mechanisms datatypes for 
 *                               the import interface for su4sml
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


structure XMI_ExtensionMechanisms =
(* from UML 1.5 Extension Mechanisms Overview:--------------------------------
 * The Extension Mechanisms package is the subpackage that specifies how 
 * specific UML model elements are customized and extended with new semantics 
 * by using stereotypes, constraints, tag definitions, and tagged values. 
 * A coherent set of such extensions, defined for specific purposes, 
 * constitutes a UML profile.
 * --------------------------------------------------------------------------*)
struct
open XMI_DataTypes XMI_OCL
                    
(* from UML 1.5 Extension Mechanisms:-----------------------------------------
 * The stereotype concept provides a way of branding (classifying) model 
 * elements so that they behave in some respects as if they were instances of 
 * new virtual metamodel constructs. These model elements have the same 
 * structure (attributes, associations, operations) as similar non-stereotyped 
 * model elements of the same kind. The stereotype may specify additional 
 * constraints and tag definitions that apply to model elements. In addition, 
 * a stereotype may be used to indicate a difference in meaning or usage 
 * between two model elements with identical structure.
 * --------------------------------------------------------------------------*)
type Stereotype = {xmiid: string, 
		   name: string,
		   (* extendedElement: string list        *)
		   (* definedTag: string list             *)
		   stereotypeConstraint: Constraint option,
		   baseClass: string option}

(* from UML 1.5 Extension Mechanisms:-----------------------------------------
 * A tag definition specifies the tagged values that can be attached to a kind 
 * of model element.
 * --------------------------------------------------------------------------*)
type TagDefinition = {xmiid: string,
		      name: string,
		      multiplicity: Multiplicity}

(* from UML 1.5 Extension Mechanisms:-----------------------------------------
 * A tagged value allows information to be attached to any model element in 
 * conformance with its tag definition. Although a tagged value, being an 
 * instance of a kind of ModelElement, automatically inherits the name 
 * attribute, the name that is actually used in the tagged value is the name 
 * of the associated tag definition. 
 * --------------------------------------------------------------------------*)
type TaggedValue = {xmiid: string,
		    dataValue: string, (* the value of the tag *)
		    tag_type: string   (* xmi.idref to TagDefinition *)
		    }
end



