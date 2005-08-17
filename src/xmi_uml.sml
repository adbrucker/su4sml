(*****************************************************************************
 *          su4sml - a shallow embedding of OCL in Isabelle/HOL              
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


structure XMI_OCL = 
struct
(* from OCL 2.0 Expressions: -------------------------------------------------
 * A VariableDeclaration declares a variable name and binds it to a type. The 
 * variable can be used in expressions where the variable is in scope. This 
 * metaclass represents amongst others the variables self and result and the 
 * variables defined using the Let expression.
 * not supported: initExpression 
 * --------------------------------------------------------------------------*)
type VariableDeclaration = { xmiid: string,
			     name: string,
			     declaration_type: string }
			   
			   
datatype OCLExpression = LiteralExp of { symbol          : string,
					 expression_type : string }
		       | IfExp    of { condition       : OCLExpression,
				      thenExpression  : OCLExpression,
				      elseExpression  : OCLExpression,
				      expression_type : string }
       | AssociationEndCallExp   of { source                   : OCLExpression,
				       referredAssociationEnd   : string,
				       expression_type          : string }
       | AssociationClassCallExp of { source                   : OCLExpression, 
				       referredAssociationClass : string,
				       expression_type          : string }
       | AttributeCallExp        of { source            : OCLExpression,
				       referredAttribute : string,
				       expression_type   : string }
       | OperationCallExp        of { source            : OCLExpression,
				       arguments         : OCLExpression list,
				       referredOperation : string,
				       expression_type   : string }
       | VariableExp             of { referredVariable: string,
				       expression_type : string }
       | LetExp                  of { variable        : VariableDeclaration,
				       initExpression  : OCLExpression,
				       inExpression    : OCLExpression,
				       expression_type : string }
       | IterateExp              of { iterators       : VariableDeclaration list, 
				       result          : VariableDeclaration ,
				       body            : OCLExpression,
				       source          : OCLExpression,
				       expression_type : string}
       | IteratorExp             of { name            : string, 
				       iterators       : VariableDeclaration list,
				       body            : OCLExpression,
				       source          : OCLExpression,
				       expression_type : string}

fun expression_type_of (LiteralExp{expression_type,...})           = expression_type
  | expression_type_of (IfExp{expression_type,...})                = expression_type
  | expression_type_of (AssociationEndCallExp{expression_type,...}) = expression_type
  | expression_type_of (AssociationClassCallExp{expression_type,...}) = expression_type
  | expression_type_of (AttributeCallExp{expression_type,...}) = expression_type
  | expression_type_of (OperationCallExp{expression_type,...}) = expression_type
  | expression_type_of (VariableExp{expression_type,...}) = expression_type
  | expression_type_of (LetExp{expression_type,...})           = expression_type
  | expression_type_of (IterateExp{expression_type,...})       = expression_type
  | expression_type_of (IteratorExp{expression_type,...})      = expression_type

(* from UML 1.5 Core: --------------------------------------------------------
 * A constraint is a semantic condition or restriction expressed in text.
 * not supported: 
 * --------------------------------------------------------------------------*)

datatype ConstraintType = Inv | Pre | Post | Def | Body

type OCLConstraint = { xmiid           : string,
		       name            : string option,
		       constraint_type : string, (*xmi.idref*)
		       body            : OCLExpression }


end
 
(* ---------------------------------------------------------------------------
 * The types in this signature are supposed to define a representation of the 
 * XML elements found in UML-XMI files for UML-Core constructs. It is 
 * reasonably close to the UML metamodel and the XMI representation of it, 
 * while simplifying some kinds of references.
 * 
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

structure XMI_UML = 
struct
open XMI_OCL

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
 * --------------------------------------------------------------------------*)

(* ---------------------------------------------------------------------------
 * First some "simple" types that are used as attributes for the modeling 
 * constructs below.
 * --------------------------------------------------------------------------*)
datatype ParameterDirectionKind = In | Out | Inout | Return
datatype OrderingKind           = Unordered | Ordered 
datatype AggregationKind        = NoAggregation | Aggregate | Composite

(* from UML 1.5 Core: --------------------------------------------------------
 * ChangeableKind defines an enumeration that denotes how an AttributeLink or 
 * LinkEnd may be modified.
 * --------------------------------------------------------------------------*)
datatype ChangeabilityKind = Changeable (* No restrictions on modification.  *)
                      | Frozen     (* The value may not be changed from the  *)
			           (* source end after the creation and      *)
                                   (* initialization of the source object.   *)
                                   (* Operations on the other end may change *)
                                   (* a value.                               *)
                      | AddOnly    (* If the multiplicity is not fixed,      *)
                                   (* values may be added at any time from   *)
                                   (* the source object, but once created a  *)
                                   (* value may not be removed from the      *)
                                   (* source end. Operations on the other    *)
                                   (* end may change a value.                *)
(* from UML 1.5 Core: --------------------------------------------------------
 * VisibilityKind defines an enumeration that denotes how the element to which 
 * it refers is seen outside the enclosing name space.
 * --------------------------------------------------------------------------*)
datatype VisibilityKind = Public (* Other elements may see and use the target*)
				(* element.                                  *)
                    | Private   (* Only the source element may see and use   *)
                                (* the target element.                       *)
                    | Protected (* Descendants of the source element may see *)
                                (* and use the target element.               *)
                    | Package   (* Elements declared in the same package as  *)
                                (* the target element may see and use the    *)
                                (* target                                    *)

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
type UMLGeneralization = { xmiid: string,
			   child_id : string, 
			   parent_id : string }


(* from UML 1.5 Core: --------------------------------------------------------
 * An attribute is a named slot within a classifier that describes a range
 * of values that instances of the classifier may hold. 
 * not supported: association associationEnd
 * --------------------------------------------------------------------------*)
type UMLAttribute = { xmiid : string,
		      name : string,
		      type_id : string, (* xmi.idref to type *)
		      (* initialValue : ...,               *)
		      (* inherited from StructuralFeature: *)
		      (* multiplicity : (int * int) list,  *)
		      (* ordering : OrderingKind           *)
		      (* targetScope : ScopeKind           *)
		      changeability : ChangeabilityKind,
		      (* inherited from Feature:           *)
		      (* ownerScope : ... ,                *)
		      visibility : VisibilityKind
		      }


(* from UML 1.5 Core: --------------------------------------------------------
 * A parameter is an unbound variable that can be changed, passed, or 
 * returned. A parameter may include a name, type, and direction of 
 * communication. Parameters are used in the specification of operations, 
 * messages and events, templates, etc. 
 * not supported: attribute defaultValue 
 * --------------------------------------------------------------------------*)
type UMLParameter = { xmiid : string,
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
type UMLOperation = { xmiid : string,
		      name : string,
		      (* concurrency : CallConcurrencyKind, *)
		      (* isRoot : bool,                     *)
		      (* isLeaf : bool,                     *)
		      (* isAbstract : bool,                 *)
		      (* specification : string,            *)
		      (* methods: UMLMethod list,           *)
		      (* inherited from BehavioralFeature:  *)
		      isQuery : bool,
		      parameter : UMLParameter list,
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
type UMLClass = { xmiid : string,
		  name : string,
		  isActive : bool,
		  visibility : VisibilityKind,
		  (* inherited from GeneralizableElement: *)
		  (* isRoot : bool,                       *)
		  (* isAbstract : bool,                   *)
		  isLeaf : bool,
		  (* xmi.idref to UMLGeneralization *)
		  generalizations: string list,
		  (* inherited from Classifier: *)
		  attributes : UMLAttribute list,
		  operations: UMLOperation list,
		  (* inherited from ModelElement: *)
		  (* xmi.idref to UMLConstraint *)
		  invariant: string list }

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
type UMLPrimitive = { xmiid : string,
		      name : string,
		      operations: UMLOperation list,
		      generalizations: string list,
		      invariant: string list}


(* from UML 1.5 Core: --------------------------------------------------------
 * Enumeration defines a kind of DataType whose range is a list of predefined 
 * values, called enumeration literals.
 * --------------------------------------------------------------------------*)
(*type UMLEnumeration = { xmiid : string,
			name: string,
			literal : string list,
			(* inherited from GeneralizableElement: *)
			(* isRoot : bool,                       *)
			(* isAbstract : bool,                   *)
			(* isLeaf : bool, *)
			(* inherited from Feature:              *)
			operations: UMLOperation list } *)

(* from UML 1.5 Core: --------------------------------------------------------
 * An interface is a named set of operations that characterize the 
 * behavior of an element.
 * --------------------------------------------------------------------------*)
type UMLInterface = { xmiid : string,
		      name: string,
		      generalizations: string list,
		      operations : UMLOperation list,
		      invariant: string list}  


type UMLCollection = { xmiid : string,
		       name : string,
		       operations: UMLOperation list,
		       generalizations: string list,
		       elementtype: string (* xmi.idref to classifier *)}

type UMLSequence = UMLCollection
type UMLSet = UMLCollection
type UMLBag = UMLCollection
type UMLOrderedSet = UMLCollection

type UMLEnumeration = { xmiid : string,
			name : string,
			operations: UMLOperation list,
			generalizations: string list,
			literals: string list, (* names of literals *)
			invariant: string list}

type UMLVoid = {xmiid: string,
		name : string }

(* from UML 1.5 Core: --------------------------------------------------------
 * An association end is an endpoint of an association, which connects 
 * the association to a classifier. Each association end is part of one
 * association. The association-ends of each association are ordered.
 * not supported: associations qualifier, specification,
 *                stereotypes <<association>>, <<global>>, <<local>>, 
 *                            <<parameter>>, <<self>>
 * --------------------------------------------------------------------------*)
type UMLAssociationEnd = { xmiid : string,
			   name : string,
			   isNavigable: bool,
			   ordering : OrderingKind,
			   aggregation : AggregationKind,
			   (* targetScope: ScopeKind, *)
			   multiplicity : Multiplicity,
			   changeability: ChangeabilityKind,
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
type UMLAssociation = { xmiid : string,
			name : string,
			connection: UMLAssociationEnd list }

(* from UML 1.5 Core: --------------------------------------------------------
 * An association class is an association that is also a class. It not 
 * only connects a set of classifiers but also defines a set of features 
 * that belong to the relationship itself and not any of the classifiers.
 * not supported....
 * --------------------------------------------------------------------------*)
(* type UMLAssociationClass = ... *)


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
datatype UMLClassifier = Primitive   of UMLPrimitive
		       | Class       of UMLClass     
		       | Interface   of UMLInterface 
		       | Enumeration of UMLEnumeration 
		       | Collection  of UMLCollection 
		       | Sequence    of UMLSequence
                       | Set         of UMLSet
                       | Bag         of UMLBag
                       | OrderedSet  of UMLOrderedSet
		       | Void        of UMLVoid

fun classifier_name_of (Primitive{name,...}) = name
  | classifier_name_of (Class{name,...}) = name
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
  | classifier_generalizations_of (Interface{generalizations,...}) = generalizations
  | classifier_generalizations_of (Enumeration{generalizations,...}) = generalizations
  | classifier_generalizations_of (Collection{generalizations,...}) = generalizations
  | classifier_generalizations_of (Sequence{generalizations,...}) = generalizations
  | classifier_generalizations_of (Set{generalizations,...}) = generalizations
  | classifier_generalizations_of (Bag{generalizations,...}) = generalizations
  | classifier_generalizations_of (OrderedSet{generalizations,...}) = generalizations
  

fun classifier_elementtype_of (Collection{elementtype,...}) = elementtype
  | classifier_elementtype_of (Sequence{elementtype,...}) = elementtype
  | classifier_elementtype_of (Set{elementtype,...}) = elementtype
  | classifier_elementtype_of (Bag{elementtype,...}) = elementtype
  | classifier_elementtype_of (OrderedSet{elementtype,...}) = elementtype

type UMLStereotype = {xmiid: string, name: string}
                    
(* from UML 1.5 Model Management: --------------------------------------------
 * A package is a grouping of model elements.
 * [...]
 * A Package may only own or reference Packages, Classifiers, Associations,
 * Generalizations, Dependencies, Comments, Constraints, Collaborations, 
 * StateMachines, Stereotypes, and TaggedValues.
 * --------------------------------------------------------------------------*)
(* We treat "Model" the same way as a "Package".                             *)
datatype UMLPackage = UMLPackage of { xmiid: string,
				      name: string,
				      visibility: VisibilityKind,
				      packages: UMLPackage list,
				      classifiers: UMLClassifier list,
				      associations: UMLAssociation list,
				      generalizations: UMLGeneralization list,
				      constraints: OCLConstraint list }
		  

(* There may be (are) model elements outside of the UML model *)
type XmiContent = {classifiers: UMLClassifier list,
		   constraints: OCLConstraint list,
		   packages: UMLPackage list,
		   stereotypes: UMLStereotype list,
		   variable_declarations: VariableDeclaration list
					      }

end
