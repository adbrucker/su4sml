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
open XMI_OCL

datatype AggregationKind        = NoAggregation | Aggregate | Composite


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


end


structure XMI_ExtensionMechanisms =
(* from UML 1.5 Extension Mechanisms Overview:--------------------------------
 * The Extension Mechanisms package is the subpackage that specifies how 
 * specific UML model elements are customized and extended with new semantics 
 * by using stereotypes, constraints, tag definitions, and tagged values. 
 * A coherent set of such extensions, defined for specific purposes, 
 * constitutes a UML profile.
 * --------------------------------------------------------------------------*)
struct
open XMI_DataTypes
                    
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



structure XMI_Core = 
(* from UML 1.5 Core Overview: ----------------------------------------------
 * The Core package is the most fundamental of the subpackages that compose 
 * the UML Foundation package. It defines the basic abstract and concrete 
 * metamodel constructs needed for the development of object models. 
 * Abstract constructs are not instantiable and are commonly used to reify 
 * key constructs, share structure, and organize the UML metamodel. Concrete
 * metamodel constructs are instantiable and reflect the modeling constructs 
 * used by object modelers (cf. metamodelers). Abstract constructs defined 
 * in the Core include ModelElement, GeneralizableElement, and Classifier. 
 * Concrete constructs specified in the Core include Class, Attribute, 
 * Operation, and Association.                                               
 * --------------------------------------------------------------------------*
 * represented constructs are: Association, AssociationClass?, AssociationEnd, 
 *    Attribute, Class, Classifier?, Constraint, DataType, Generalization, 
 *    Interface, Operation, Parameter
 * 
 * the following constructs are not represented: Abstraction, Artifact, 
 *    BehavioralFeature, Binding, Comment, Component, Dependency, Element,
 *    ElementOwnership, ElementResidence, Enumeration?, EnumerationLiteral?
 *    Feature, Flow, GeneralizableElement, Method, ModelElement, Namespace, 
 *    Node, Permission, PresentationElement, Primitive?, 
 *    ProgrammingLanguageDataType, Relationship, StructuralFeature, 
 *    TemplateArgument, TemplateParameter, Usage
 * --------------------------------------------------------------------------*)
struct
open XMI_ExtensionMechanisms


(* UML distinguishes between different kinds of dependencies:         *)
(* Abstraction, Binding, Permission, and Usage. WE do not distinguish *)
(* these at the moment                                                *)

type Dependency = { xmiid: string,
		    client: string,
		    supplier: string,
		    stereotype: string}

(* from UML 1.5 Core: --------------------------------------------------------
 * A generalization is a taxonomic relationship between a more general 
 * element and a more specific element. The more specific element is 
 * fully consistent with the more general element (it has all of its 
 * properties, members, and relationships) and may contain additional 
 * information.
 * not supported : association powertype
 *                 stereotype <<implementation>>
 *                 constraints 'complete', 'disjoint', 
 *                             'incomplete', 'overlapping'
 * --------------------------------------------------------------------------*)
type Generalization = { xmiid: string,
			child_id : string, 
			parent_id : string }


(* from UML 1.5 Core: --------------------------------------------------------
 * An attribute is a named slot within a classifier that describes a range
 * of values that instances of the classifier may hold. 
 * not supported: association associationEnd
 * --------------------------------------------------------------------------*)
type Attribute = { xmiid : string,
		   name : string,
		   type_id : string, (* xmi.idref to type *)
		   (* initialValue : ...,               *)
		   (* inherited from StructuralFeature: *)
		   multiplicity : Multiplicity, 
		   ordering : OrderingKind,           
		   (* targetScope : ScopeKind           *)
		   changeability : ChangeableKind,
		   (* inherited from Feature:           *)
		   (* ownerScope : ... ,                *)
		   visibility : VisibilityKind,
		   taggedValue : TaggedValue list
				}


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
		 
(* fom UML 1.5 Core: ---------------------------------------------------------	    
 * An operation is a service that can be requested from an object to effect 
 * behavior. An operation has a signature, which describes the actual 
 * parameters that are possible (including possible return values).
 * not supported: taggedValue "semantics"
 * --------------------------------------------------------------------------*)
type Operation = { xmiid : string,
		   name : string,
		   (* concurrency : CallConcurrencyKind, *)
		   (* isRoot : bool,                     *)
		   (* isLeaf : bool,                     *)
		   (* isAbstract : bool,                 *)
		   (* specification : string,            *)
		   (* methods: UMLMethod list,           *)
		   (* inherited from BehavioralFeature:  *)
		   isQuery : bool,
		   parameter : Parameter list,
		   (* inherited from Feature:            *)
		   (* ownerScope : ScopeKind,            *)
		   visibility : VisibilityKind,
		   (* inherited from ModelElemt:         *)
		   (* xmi.idref to UMLConstraint         *)
		   constraints : string list }
				   
		    
(* from UML 1.5 Core: --------------------------------------------------------
 * A class is a description of a set of objects that share the same 
 * attributes, operations, methods, relationships, and semantics. A class 
 * may use a set of interfaces to specify collections of operations it 
 * provides to its environment. 
 * not supported: stereotypes <<auxiliary>>, <<focus>>, 
 *                            <<implementation>>, <<type>> 
 * --------------------------------------------------------------------------*)
type Class = { xmiid : string,
	       name : string,
	       isActive : bool,
	       visibility : VisibilityKind,
	       (* inherited from GeneralizableElement: *)
	       (* isRoot : bool,                       *)
	       (* isAbstract : bool,                   *)
	       isLeaf : bool,
	       (* xmi.idref to Generalization *)
	       generalizations: string list,
	       (* inherited from Classifier: *)
	       attributes : Attribute list,
	       operations: Operation list,
	       (* inherited from ModelElement: *)
	       (* xmi.idref to Constraint *)
	       invariant: string list ,
	       stereotype: string list,
	       taggedValue: TaggedValue list,
	       clientDependency: string list,
	       supplierDependency: string list,
	       (* xmi.id's of contained ClassifierInStates: *)
	       classifierInState: string list }

(* from UML 1.5 Core: --------------------------------------------------------
 * A data type is a type whose values have no identity (i.e., they are 
 * pure values). Data types include primitive built-in types (such as 
 * integer and string) as well as definable enumeration types (such as 
 * the predefined enumeration type boolean whose literals are false and 
 * true). 
 * --------------------------------------------------------------------------*)
(* type UMLDataType = UMLPrimitive | UMLEnumeration *)


(* from UML 1.5 Core: --------------------------------------------------------
 * A Primitive defines a predefined DataType, without any relevant UML 
 * substructure (i.e., it has no UML parts). A primitive datatype may have an 
 * algebra and operations defined outside of UML, for example, mathematically. 
 * Primitive datatypes used in UML itself include Integer, UnlimitedInteger, 
 * and String. 
 * --------------------------------------------------------------------------*)
type Primitive = { xmiid:           string,
		   name:            string,
		   operations:      Operation list,
		   generalizations: string list,
		   invariant:       string list}


(* from UML 1.5 Core: --------------------------------------------------------
 * Enumeration defines a kind of DataType whose range is a list of predefined 
 * values, called enumeration literals.
 * --------------------------------------------------------------------------*)
(*type Enumeration = { xmiid : string,
			name: string,
			literal : string list,
			(* inherited from GeneralizableElement: *)
			(* isRoot : bool,                       *)
			(* isAbstract : bool,                   *)
			(* isLeaf : bool, *)
			(* inherited from Feature:              *)
			operations: Operation list } *)

(* from UML 1.5 Core: --------------------------------------------------------
 * An interface is a named set of operations that characterize the 
 * behavior of an element.
 * --------------------------------------------------------------------------*)
type Interface = { xmiid : string,
		   name: string,
		   generalizations: string list,
		   operations : Operation list,
		   invariant: string list,
		   clientDependency: string list,
		   supplierDependency: string list}  


type Collection = { xmiid : string,
		    name : string,
		    operations: Operation list,
		    generalizations: string list,
		    elementtype: string (* xmi.idref to classifier *)}

type Sequence   = Collection
type Set        = Collection
type Bag        = Collection
type OrderedSet = Collection

type Enumeration = { xmiid : string,
		     name : string,
		     operations: Operation list,
		     generalizations: string list,
		     literals: string list, (* names of literals *)
		     invariant: string list}
		   
type Void = {xmiid: string,
	     name : string }

(* from UML 1.5 Core: --------------------------------------------------------
 * An association end is an endpoint of an association, which connects 
 * the association to a classifier. Each association end is part of one
 * association. The association-ends of each association are ordered.
 * not supported: associations qualifier, specification,
 *                stereotypes <<association>>, <<global>>, <<local>>, 
 *                            <<parameter>>, <<self>>
 * --------------------------------------------------------------------------*)
type AssociationEnd = { xmiid : string,
			name : string option,
			isNavigable: bool,
			ordering : OrderingKind,
			aggregation : AggregationKind,
			(* targetScope: ScopeKind, *)
			multiplicity : Multiplicity,
			changeability: ChangeableKind,
			visibility : VisibilityKind,
			participant_id : string (* xmi.idref to class *) }
			 
(* from UML 1.5 Core: --------------------------------------------------------
 * An association defines a semantic relationship between classifiers. 
 * The instances of an association are a set of tuples relating instances 
 * of the classifiers. Each tuple value may appear at most once.
 * not supported: stereotype <<implicit>>, 
 *	          constraint "xor", 
 *                tagged value "persistence"
 *                generalization of associations
 * --------------------------------------------------------------------------*)
type Association = { xmiid : string,
		     name : string option,
		     connection: AssociationEnd list }
		   
(* from UML 1.5 Core: --------------------------------------------------------
 * An association class is an association that is also a class. It not 
 * only connects a set of classifiers but also defines a set of features 
 * that belong to the relationship itself and not any of the classifiers.
 * not supported....
 * --------------------------------------------------------------------------*)
type AssociationClass = { xmiid : string,
			  name : string,
			  isActive : bool,
			  visibility : VisibilityKind,
			  (* inherited from GeneralizableElement: *)
			  (* isRoot : bool,                       *)
			  (* isAbstract : bool,                   *)
			  isLeaf : bool,
			  (* xmi.idref to Generalization *)
			  generalizations: string list,
			  (* inherited from Classifier: *)
			  attributes : Attribute list,
			  operations: Operation list,
			  (* inherited from ModelElement: *)
			  (* xmi.idref to Constraint *)
			  invariant: string list ,
			  stereotype: string list,
			  taggedValue: TaggedValue list,
			  clientDependency: string list,
			  supplierDependency: string list,
			  connection: AssociationEnd list}


(* from UML 1.5 Core: --------------------------------------------------------	    
 * A classifier is an element that describes behavioral and structural 
 * features; it comes in several specific forms, including class, 
 * data type, interface, component, artifact, and others that are defined 
 * in other metamodel packages.
 * not supported: associations powertypeRane, specifiedEnd
 *                stereotypes <<metaclass>>, <<powertype>>, <<process>>, 
 *                            <<thread>>, <<utility>>
 *                taggedValues persistence, semantics
 * --------------------------------------------------------------------------*)
datatype Classifier = Primitive   of Primitive
		    | Class       of Class     
		    | AssociationClass of AssociationClass
		    | Interface   of Interface 
		    | Enumeration of Enumeration 
		    | Collection  of Collection 
		    | Sequence    of Sequence
                    | Set         of Set
                    | Bag         of Bag
                    | OrderedSet  of OrderedSet
		    | Void        of Void

exception IllFormed of string

fun classifier_stereotype_of (Class{stereotype,...}) = stereotype
  | classifier_stereotype_of (AssociationClass{stereotype,...}) = stereotype
  | classifier_stereotype_of _  = nil


fun classifier_name_of (Primitive{name,...}) = name
  | classifier_name_of (Class{name,...}) = name
  | classifier_name_of (AssociationClass{name,...}) = name
  | classifier_name_of (Interface{name,...}) = name
  | classifier_name_of (Enumeration{name,...}) = name
  | classifier_name_of (Collection{name,...}) = name
  | classifier_name_of (Sequence{name,...}) = name
  | classifier_name_of (Set{name,...}) = name
  | classifier_name_of (Bag{name,...}) = name
  | classifier_name_of (OrderedSet{name,...}) = name
  | classifier_name_of (Void{name,...}) = name

fun classifier_xmiid_of (Primitive{xmiid,...}) = xmiid
  | classifier_xmiid_of (Class{xmiid,...}) = xmiid
  | classifier_xmiid_of (AssociationClass{xmiid,...}) = xmiid
  | classifier_xmiid_of (Interface{xmiid,...}) = xmiid
  | classifier_xmiid_of (Enumeration{xmiid,...}) = xmiid
  | classifier_xmiid_of (Collection{xmiid,...}) = xmiid
  | classifier_xmiid_of (Sequence{xmiid,...}) = xmiid
  | classifier_xmiid_of (Set{xmiid,...}) = xmiid
  | classifier_xmiid_of (Bag{xmiid,...}) = xmiid
  | classifier_xmiid_of (OrderedSet{xmiid,...}) = xmiid
  | classifier_xmiid_of (Void{xmiid,...}) = xmiid

fun classifier_generalizations_of (Primitive{generalizations,...}) = generalizations
  | classifier_generalizations_of (Class{generalizations,...}) = generalizations
  | classifier_generalizations_of (AssociationClass{generalizations,...}) = generalizations
  | classifier_generalizations_of (Interface{generalizations,...}) = generalizations
  | classifier_generalizations_of (Enumeration{generalizations,...}) = generalizations
  | classifier_generalizations_of (Collection{generalizations,...}) = generalizations
  | classifier_generalizations_of (Sequence{generalizations,...}) = generalizations
  | classifier_generalizations_of (Set{generalizations,...}) = generalizations
  | classifier_generalizations_of (Bag{generalizations,...}) = generalizations
  | classifier_generalizations_of (OrderedSet{generalizations,...}) = generalizations
  | classifier_generalizations_of _ = nil
  

fun classifier_elementtype_of (Collection{elementtype,...}) = elementtype
  | classifier_elementtype_of (Sequence{elementtype,...}) = elementtype
  | classifier_elementtype_of (Set{elementtype,...}) = elementtype
  | classifier_elementtype_of (Bag{elementtype,...}) = elementtype
  | classifier_elementtype_of (OrderedSet{elementtype,...}) = elementtype
  | classifier_elementtype_of _ = raise IllFormed "classifier_elementtype_of called on a non-collection value"

end


