(*****************************************************************************
 * su4sml --- a SML repository for managing (Secure)UML/OCL models
 *             http://projects.brucker.ch/su4sml/
 *                                                                            
 * xmi_core.sml --- 
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
open XMI_ExtensionMechanisms XMI_ActivityGraphs


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
		   initialValue : OCLExpression option,               
		   (* inherited from StructuralFeature: *)
		   multiplicity : Multiplicity, 
		   ordering : OrderingKind,           
		   (* targetScope : ScopeKind           *)
		   changeability : ChangeableKind,
		   (* inherited from Feature:           *)
		   ownerScope : ScopeKind,               
		   targetScope : ScopeKind,
		   visibility : VisibilityKind,
		   taggedValue : TaggedValue list,
		   stereotype : string list
				}



		 
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
		   ownerScope : ScopeKind,            
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
	       classifierInState: string list,
	       activity_graphs: ActivityGraph list,
	       state_machines: StateMachine list}

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
		   invariant:       string list,
		   taggedValue: TaggedValue list
		   }


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

type Enumeration = {
     xmiid : string,
		 name : string,
		 operations: Operation list,
		 generalizations: string list,
		 literals: string list (* names of literals *),
		 invariant: string list
}
		   
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
type AssociationEnd = { 
     xmiid : string,
		 name : string option,
		 association: string (* xmiid of enclosing association *),
		 isNavigable: bool,
		 ordering : OrderingKind,
		 aggregation : AggregationKind,
		 targetScope: ScopeKind,
		 multiplicity : Multiplicity,
		 changeability: ChangeableKind,
     qualifier: Attribute list,
		 visibility : VisibilityKind,
		 participant_id : string (* xmi.idref to class *)
}
			 
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

fun classifier_operations_of (Primitive{operations,...}) = operations
  | classifier_operations_of (Class{operations,...}) = operations
  | classifier_operations_of (AssociationClass{operations,...}) = operations
  | classifier_operations_of (Interface{operations,...}) = operations
  | classifier_operations_of (Enumeration{operations,...}) = operations
  | classifier_operations_of (Collection{operations,...}) = operations
  | classifier_operations_of (Sequence{operations,...}) = operations
  | classifier_operations_of (Set{operations,...}) = operations
  | classifier_operations_of (Bag{operations,...}) = operations
  | classifier_operations_of (OrderedSet{operations,...}) = operations
  | classifier_operations_of (Void{...}) = []

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
  | classifier_elementtype_of _ = Rep_Logger.error "in classifier_elementtype_of: \
                                                \argument is not a collection value"

end


