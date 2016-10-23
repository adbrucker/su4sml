(*****************************************************************************
 * su4sml --- an SML repository for managing (Secure)UML/OCL models
 *             http://projects.brucker.ch/su4sml/
 *                                                                            
 * xmi_extensionmechanisms.sml --- XMI-UML Extension mechanisms datatypes for 
 *                                 the import interface for su4sml
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



