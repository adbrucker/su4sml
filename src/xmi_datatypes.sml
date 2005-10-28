(*****************************************************************************
 *          su4sml - a SecureUML repository for SML              
 *                                                                            
 * xmi_datatypes.sml - XMI-UML basic datatypes for the import interface for su4sml
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
