(*****************************************************************************
 * su4sml --- an SML repository for managing (Secure)UML/OCL models
 *             http://projects.brucker.ch/su4sml/
 *                                                                            
 * xmi_datatypes.sml --- XMI-UML basic datatypes for the import interface 
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

structure XMI_DataTypes =
(* from UML 1.5 Core Overview: ----------------------------------------------
 * The Data Types package is the subpackage that specifies the different data 
 * types that are used to define UML. 
 * 
 * the following constructs are currently not represented: ArgListsExpression,
 * Boolean, BooleanExpression, CallConcurrencyKind, Expression, Geometry, 
 * Integer, LocationReference, Mapping, MappingExpression, Name, 
 * ProcedureExpression, PseudostateKind, ScopeKind, String, TimeExpression, 
 * TypeExpression, UnlimitedInteger
 * --------------------------------------------------------------------------*)
struct

datatype AggregationKind        = NoAggregation | Aggregate | Composite

datatype ScopeKind = InstanceScope | ClassifierScope

(* from UML 1.5 Core: --------------------------------------------------------
 * ChangeableKind defines an enumeration that denotes how an AttributeLink or 
 * LinkEnd may be modified.
 * --------------------------------------------------------------------------*)
datatype ChangeableKind = Changeable (* No restrictions on modification.     *)
			| Frozen     (* The value may not be changed from the*)
			             (* source end after the creation and    *)
			             (* initialization of the source object. *)
			             (* Operations on the other end may      *)
			             (* change a value.                      *)
			| AddOnly    (* If the multiplicity is not fixed,    *)
                                     (* values may be added at any time from *)
                                     (* the source object, but once created a*)
                                     (* value may not be removed from the    *)
                                     (* source end. Operations on the other  *)
                                     (* end may change a value.              *)

(* from UML 1.5 Data Types: --------------------------------------------------
 * a Multiplicity [consists of a list of MultiplicityRanges and] defines a 
 * non-empty set of non-negative integers. 
 * a MultiplicityRange defines a range of integers. The upper bound of the 
 * range cannot be below the lower bound. The lower bound must be a 
 * nonnegative integer. The upper bound must be a nonnegative integer or the 
 * special value unlimited, which indicates there is no upper bound on the 
 * range. 
 * --------------------------------------------------------------------------*)
(* provisionally, we denote the upper bound 'unlimited' by "-1"              *)
type Multiplicity = (int * int) list

datatype OrderingKind           = Unordered | Ordered 
datatype ParameterDirectionKind = In | Out | Inout | Return


(* from UML 1.5 Core: --------------------------------------------------------
 * VisibilityKind defines an enumeration that denotes how the element to which 
 * it refers is seen outside the enclosing name space.
 * --------------------------------------------------------------------------*)
datatype VisibilityKind = public (* Other elements may see and use the target*)
				(* element.                                  *)
                    | private   (* Only the source element may see and use   *)
                                (* the target element.                       *)
                    | protected (* Descendants of the source element may see *)
                                (* and use the target element.               *)
                    | package   (* Elements declared in the same package as  *)
                                (* the target element may see and use the    *)
                                (* target                                    *)


(* from UML 1.5 Core: --------------------------------------------------------
 * A parameter is an unbound variable that can be changed, passed, or 
 * returned. A parameter may include a name, type, and direction of 
 * communication. Parameters are used in the specification of operations, 
 * messages and events, templates, etc. 
 * not supported: attribute defaultValue 
 * --------------------------------------------------------------------------*)
type Parameter = { xmiid : string,
		   name : string,
		   kind : ParameterDirectionKind,
		   (* defaultValue : ..., *)
		   type_id : string (* xmi.idref to type *)}


end
