(*****************************************************************************
 * su4sml --- a SML repository for managing (Secure)UML/OCL models
 *             http://projects.brucker.ch/su4sml/
 *                                                                            
 * rep_core.sml --- core repository datastructures for su4sml
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

(** Repository datatypes and helper functions for classifiers. *)
signature REP_CORE = 
sig
type Scope
type Visibility
type operation = { 
     name          : string,	
     precondition  : (string option * Rep_OclTerm.OclTerm) list,
     postcondition : (string option * Rep_OclTerm.OclTerm) list,
     body          : (string option * Rep_OclTerm.OclTerm) list,
     arguments     : (string * Rep_OclType.OclType) list,
     result        : Rep_OclType.OclType,
     isQuery       : bool,
     scope         : Scope,
		   stereotypes    : string list,
     visibility    : Visibility	
}     	

type associationend= {
     name: Rep_OclType.Path (* pathOfAssociation@[role]*),
     aend_type : Rep_OclType.OclType (* participant type *),
     multiplicity: (int * int) list,
     ordered: bool,
     visibility: Visibility,
     init: Rep_OclTerm.OclTerm option
}		
     
type attribute = {
     name : string,
     attr_type : Rep_OclType.OclType,
     visibility : Visibility,
     scope: Scope,
     stereotypes: string list,
     init : Rep_OclTerm.OclTerm option
}

type association = { 
     name: Rep_OclType.Path (* pathOfPackage@[assocName] *),
     aends: associationend list,
     qualifiers: (string * attribute list) list (*(aend name, qualifiers)*),
     aclass: Rep_OclType.Path option
}
                   
type constraint = (string option * Rep_OclTerm.OclTerm)
                  
datatype Classifier =  
	 Class of 
         { name        : Rep_OclType.OclType, 
	   parent      : Rep_OclType.OclType option,
	   attributes  : attribute list,
	   operations  : operation list,
	   associations: Rep_OclType.Path list (* associations *),
	   invariant   : (string option * Rep_OclTerm.OclTerm) list,
	   stereotypes : string list,
	   interfaces  : Rep_OclType.OclType list,
	   thyname     : string option,
	   visibility  : Visibility,
           activity_graphs : Rep_ActivityGraph.ActivityGraph list
	 }
       | AssociationClass of
	 { name        : Rep_OclType.OclType, 
	   parent      : Rep_OclType.OclType option,
	   attributes  : attribute list,
	   operations  : operation list,
	   invariant   : (string option * Rep_OclTerm.OclTerm) list,
	   stereotypes : string list,
	   interfaces  : Rep_OclType.OclType list,
	   thyname     : string option,
     activity_graphs    : Rep_ActivityGraph.ActivityGraph list,
     associations: Rep_OclType.Path list,
	   visibility  : Visibility,
	   association: Rep_OclType.Path
	 } 
       | Interface (* not supported yet *) of
	 { name        : Rep_OclType.OclType,
	   parents     : Rep_OclType.OclType list, 
	   operations  : operation list,
	   stereotypes : string list,
	   invariant   : (string option * Rep_OclTerm.OclTerm) list,
	   thyname     : string option
	 }
       | Enumeration (* not really supported yet? *) of
	 { name        : Rep_OclType.OclType,
	   parent      : Rep_OclType.OclType option,
	   operations  : operation list,
	   literals    : string list,
	   invariant   : (string option * Rep_OclTerm.OclTerm) list,
	   stereotypes : string list,
	   interfaces  : Rep_OclType.OclType list,
	   thyname     : string option
	 }
       | Primitive (* not really supported yet *) of
	 { name        : Rep_OclType.OclType,
	   parent      : Rep_OclType.OclType option,
	   operations  : operation list,
	   associations: Rep_OclType.Path list,
	   invariant   : (string option * Rep_OclTerm.OclTerm) list,
	   stereotypes : string list,
	   interfaces  : Rep_OclType.OclType list,
	   thyname     : string option
	 }
       | Template of 
	 { parameter   : Rep_OclType.OclType,
	   classifier  : Classifier
	 }

type transform_model = (Classifier list * association list)


(***************************************** 
 *                 MODEL                 *
 *****************************************)
(**
 * TODO: Description 
 *)
val joinModel      : transform_model -> transform_model -> transform_model

(** 
 * TODO: Description 
 *)
val normalize_ext  : transform_model -> transform_model

(**
 * TODO: Description
 *)
val normalize      : association list -> Classifier -> Classifier

(**
 * TODO: Description
 *)
val normalize_init : Classifier -> Classifier

(** 
 * Update the thy_name of a classifier.
 *)
val update_thyname      : string -> Classifier -> Classifier

(**
 * Update the classifier wiht new invariants.
 *)
val update_invariant    : (string option * Rep_OclTerm.OclTerm) list -> 
                          Classifier -> Classifier

(** 
 * Update the classifier with new operations.
 *)
val update_operations   : operation list -> Classifier -> Classifier 

(** 
 * Update the classifier with a new invariant.
 *)
val addInvariant : constraint -> Classifier -> Classifier

(**
 * Update the classifier with new invariants.
 *)
val addInvariants: constraint list -> Classifier -> Classifier

(** 
 * Update the classifier with a new operations
 *)
val addOperation : operation  -> Classifier -> Classifier

(**
 * Sort the classifier according to its names.
 *)
val topsort_cl          : Classifier list -> Classifier list

(**
 * TODO: Description
 *)
val connected_classifiers_of : association list -> Classifier -> Classifier list -> Classifier list


(*****************************************
 *              CLASSIFIERS              *
 *****************************************)

(**
 * Ocl Classifier OclAny.
 *)
val OclAnyC : Classifier

(**
 * Returns the classifier of a given path.
 *)
val class_of            : Rep_OclType.Path -> transform_model -> Classifier

(**
 * Is the classifier originally of the design model.
 * Classes of the design models are classes which are wether templates
 * nor oclLib classes.
 *)
val class_of_design_model : Rep_OclType.Path -> transform_model -> Classifier 


(** 
 * Returns the classifier of a given type.
 *)
val class_of_type           : Rep_OclType.OclType -> transform_model -> Classifier

(**
 * Returns the classifier of a given term.
 *) 
val class_of_term           : Rep_OclTerm.OclTerm -> transform_model -> Classifier

(**
 * Returns the classifier of the parent of a classifier.
 *)
(*
val class_of_parent         : Classifier -> transform_model -> Classifier
*)
(**
 * Returns all classifiers of a given package.
 *) 
val classes_of_package      : Rep_OclType.Path -> transform_model -> Classifier list

(** 
 * Returns paths of the associations of a classifier.
 *)
val associations_of   : Classifier -> Rep_OclType.Path list

(**
 * Returns the invariants of an operation.
 *)
val invariant_of        : Classifier -> (string option * Rep_OclTerm.OclTerm) list

(**
 * Get the stereotypes of a classifier.
 *)
val stereotypes_of      : Classifier -> string list

(**
 * Is the classifier visible?
 *)
val is_visible_cl       : Classifier -> bool

(**
 * Visibility of classifier 
 *)
val visibility_of : Classifier -> Visibility

(*****************************************
 *             TYPES                     *
 *****************************************)


val string_to_type : string -> Rep_OclType.OclType

(**
 * Returns the type of a classifier.
 *)
val type_of       : Classifier -> Rep_OclType.OclType 

(** 
 * Returns the type of the parent from a 
 * classifier.
 *)
val type_of_parent       : Classifier -> Rep_OclType.OclType

(** 
 * Returns the type of all parents of a classifier.
 *)
val type_of_parents         : Classifier -> transform_model -> Rep_OclType.OclType list    

(** 
 * Returns the type of a given term.
 *)
val type_of_term  : Rep_OclTerm.OclTerm -> Rep_OclType.OclType

(**
 * Returns the type of a given CollectionPart.
 *)
val type_of_CollPart : Rep_OclTerm.CollectionPart -> Rep_OclType.OclType

(** 
 * Returns the type of a given simple Path.
 * DO NOT USE: just for parsing where the model is not available.
 *)
val simple_type_of_path     : Rep_OclType.Path -> Rep_OclType.OclType

(** 
 * Returns the type of a given Path
 *)
val type_of_path            : Rep_OclType.Path -> transform_model -> Rep_OclType.OclType

(**
 * returns the type of the classifier this association end belongs to.
 * params {aend}
 * @param aend association end
 * @return type of the classifier at the association end
 *)
val type_of_aend   : associationend -> Rep_OclType.OclType

(**
 * Returns the type of the attribute.
 *)
val type_of_att    : attribute -> Rep_OclType.OclType

(**
 * Returns the type of a not yet instantiate template
 *)
val type_of_template : Classifier -> Rep_OclType.OclType

(**
 * Get type of template parameter.
 *)
val type_of_template_parameter      : Rep_OclType.OclType -> Rep_OclType.OclType

(** 
 * Replace the template parameter with another type.
 *)
val substitute_templ_para      : Rep_OclType.OclType -> Rep_OclType.OclType -> Rep_OclType.OclType


(** 
 * Substitute a package name of a path. 
 *)
val substitute_package        : Rep_OclType.Path -> Rep_OclType.Path -> Rep_OclType.Path -> Rep_OclType.Path

(**
 * Prefixes a type with a given package name.
 *)
val prefix_type             : string list -> Rep_OclType.OclType -> Rep_OclType.OclType

(** 
 * Prefix a given path with a prefix.
 *)
val prefix_path             : string list -> string list -> string list

(**
 * Collections of Collections are flattened according to Ocl 2.0 Standard.
 *)
val flatten_type  : Rep_OclType.OclType -> Rep_OclType.OclType

(** 
 * Dispatch a collection.
 *)
val create_set     : (string * Rep_OclType.OclType) -> Rep_OclType.OclType

(** 
 * Get the type if the associationend would be a attribute.
 *)
val convert_aend_type : associationend -> Rep_OclType.OclType

(**
 * Are two types equal?
 *)
val type_equals         : Rep_OclType.OclType -> Rep_OclType.OclType -> bool

(** 
 * Is first type conform to the second type? 
 *)
val conforms_to             : Rep_OclType.OclType -> Rep_OclType.OclType -> transform_model -> bool

(**
 * OBSOLETE 
 *)
val correct_type_for_CollLiteral : Rep_OclType.OclType -> Rep_OclTerm.CollectionPart -> bool

(** 
 * Type a collection type?
 *)
val isColl_Type      : Rep_OclType.OclType -> bool

(** 
 * Type a value (basic) type?
 *)
val isValueType : Rep_OclType.OclType -> bool


(*****************************************
 *             TERMS/EXPRESSIONS         *
 *****************************************)

(** 
 * Upcast an OclTerm.
 *)
val upcast                  : (Rep_OclTerm.OclTerm * Rep_OclType.OclType) -> Rep_OclTerm.OclTerm

(** 
 * Upcast the types of an operation according to another operation, if possible.
 *)
val upcast_op       : (Classifier * operation) list -> Rep_OclTerm.OclTerm 
			      -> (Rep_OclTerm.OclTerm * Rep_OclType.OclType) list -> transform_model -> Rep_OclTerm.OclTerm
(** 
 *
 *)
val upcastable_args      : (string * Rep_OclType.OclType) list -> (Rep_OclTerm.OclTerm * Rep_OclType.OclType) list 
			   -> transform_model -> bool
(**
 * Interfere the types of the arguments of an operation according to 
 * a given signature, if possible.
 *)
val upcast_args          : (string * Rep_OclType.OclType) list -> (Rep_OclTerm.OclTerm * Rep_OclType.OclType) list 
			   -> transform_model -> (Rep_OclTerm.OclTerm * Rep_OclType.OclType) list
(** 
 * 
 *)
val upcast_type  : Rep_OclType.OclType -> Rep_OclType.OclType -> transform_model -> Rep_OclType.OclType

(** 
 * Upcast the types of an attribute to an other attribute, if possible.
 *)
val upcast_att       : (Classifier * attribute) -> Rep_OclTerm.OclTerm -> transform_model -> Rep_OclTerm.OclTerm option

(** 
 * Upcast the types of an association to an other association, if possible.
 *)
val upcast_aend      : (Classifier * associationend) -> Rep_OclTerm.OclTerm -> transform_model -> Rep_OclTerm.OclTerm option


(** 
 * Upcast the types of an assoc/attribute to an other assoc/attribute, if possible.
 *)
val upcast_att_aend   : (Classifier * attribute option * associationend option) list 
				  -> Rep_OclTerm.OclTerm -> transform_model -> Rep_OclTerm.OclTerm
(**
 * Prefixes all types in a term with the 
 * given string list. 
 *)
val prefix_expression       : string list -> Rep_OclTerm.OclTerm -> Rep_OclTerm.OclTerm

(**
 * Prefix all types in a CollectionPart with
 * the given string list.
 *)
val prefix_collectionpart   : string list -> Rep_OclTerm.CollectionPart -> Rep_OclTerm.CollectionPart


(*****************************************
 *            OPERATIONS                 *
 *****************************************)

val operation_of        : transform_model -> Rep_OclType.Path -> operation option

(**
 * Find an operation in a list of operations.
 *)
val get_operation : string -> Classifier -> transform_model -> operation
(** Get the local operations of a classifier.*)
val local_operations_of          : Classifier -> operation list
(** Get the redefined/refined operations of a classifier.*)
val modified_operations_of    : Classifier -> transform_model -> operation list
(** Get all the inherited (without the redefined ones) operations of a classifier.*)
val inherited_operations_of   : Classifier -> transform_model -> operation list
(** Get all operations of a classifier (for redefined ones the more special is choosen).*)
val all_operations_of         : Classifier -> transform_model -> operation list
(** Get all creators of a classifier.*)
val creation_operations_of    : Classifier -> transform_model -> operation list
(** Get all destroying operations of a classifier.*)
val destruction_operations_of : Classifier -> transform_model -> operation list
(** Get all public operations of a classifier.*)
val public_operations_of      : Classifier -> transform_model -> operation list
(** Get all private operations of a classifier.*)
val private_operations_of     : Classifier -> transform_model -> operation list
(** Get all package operations of a classifier.*)
val package_operations_of     : Classifier -> transform_model -> operation list
(** Get all protected operations of a classifier.*)
val protected_operations_of   : Classifier -> transform_model -> operation list
(** Get all query operations of a classifier.*)
val query_operations_of       : Classifier -> transform_model -> operation list
(** Get all command operations of a classifier.*)
val command_operations_of     : Classifier -> transform_model -> operation list							   
(** Get the local invariants of a classifier.*)
val local_invariants_of       : Classifier -> (string option * Rep_OclTerm.OclTerm) list
(** Get the inherited invarinats of a classifier.*)
val inherited_invariants_of   : Classifier -> transform_model -> (string option * Rep_OclTerm.OclTerm) list
(** Get all invariants of a classifier.*)
val all_invariants_of         : Classifier -> transform_model -> (string option * Rep_OclTerm.OclTerm) list
(** OBSOLETE **)
val operations_of                : Classifier -> operation list
(** OBSOLETE **)
val get_overloaded_methods    : Classifier -> string -> transform_model -> (Classifier * operation) list
(** OBSOLETE **)
val get_meth                  : Rep_OclTerm.OclTerm -> string -> (Rep_OclTerm.OclTerm * Rep_OclType.OclType) list 
				-> transform_model -> Rep_OclTerm.OclTerm
(** 
 * Return the next parent which is implementing the operation.
 *)
val last_implementation_of_op : Classifier -> string -> transform_model -> (Classifier * operation)

(**
 * Returns the preconditions of an operation.
 *)
val precondition_of_op  : operation -> (string option * Rep_OclTerm.OclTerm) list

(**
 * Returns the postconditions of an operation.
 *)
val postcondition_of_op : operation -> (string option * Rep_OclTerm.OclTerm) list

(**
 * Returns the body of an operation.
 *)
val body_of_op          : operation -> (string option * Rep_OclTerm.OclTerm) list

(**
 * Returns the arguments of an operation.
 *)
val arguments_of_op     : operation -> (string * Rep_OclType.OclType) list

(**
 * Returns the result type of an operation.
 *)
val result_of_op        : operation -> Rep_OclType.OclType

(**
 * Returns the name (string) of an operation.
 *)
val name_of_op          : operation -> string

(** 
 * Is the operation visible?
 *)
val is_visible_op       : operation -> bool

(**
 * TODO: Description
 *)
val mangled_name_of_op  : operation -> string

(**
 * Update an operation with new preconditions.
 *)
val update_precondition   : (string option * Rep_OclTerm.OclTerm) list -> 
                            operation ->  operation

(** 
 * Update an operation with new postconditions.
 *)
val update_postcondition  : (string option * Rep_OclTerm.OclTerm) list -> 
                            operation ->  operation

(*****************************************
 *            ATTRIBUTES                 *
 *****************************************)

(** 
 * Get name of attribute
 *)
val name_of_att  : attribute -> string


(** 
 * Get the local attributes of a classifier.
 *)
val local_attributes_of : Classifier -> attribute list

(**
 * Get the inherited attributes of a classifier.
 *)
val inherited_attributes_of : Classifier -> transform_model -> attribute list

(**
 * Get all the attributes of a Classifier (including the inherited ones).
 *)
val all_attributes_of : Classifier -> transform_model -> attribute list

(** 
 * Find an attribute in a list of attributes. 
 *)
val get_attribute : string -> attribute list -> attribute option
(** OBSOLETE **)
val attributes_of     : Classifier -> attribute list
(** OBSOLETE **)
val get_overloaded_attrs_or_assocends    : Classifier -> string -> transform_model 
					   -> (Classifier * attribute option * associationend option) list
(** OBSOLETE **)					      
val get_attr_or_assoc       : Rep_OclTerm.OclTerm -> string -> transform_model -> Rep_OclTerm.OclTerm
(**
 * convert an associationend into an attribute.
 *)
val convert_aend      : string -> associationend -> attribute

(**
 * Is the attribute visible?
 *)
val is_visible_attr     : attribute -> bool

(*****************************************
 *       ASSOCIATIONENDS                 *
 *****************************************)

(**
 * TODO: Description
 *)
val associationends_of: association list -> Classifier -> associationend list 


val inherited_associationends_of : Classifier -> transform_model -> associationend list

val local_associationends_of  : association list -> Classifier -> associationend list

val all_associationends_of : Classifier -> transform_model -> associationend list 

(** 
 * Returns ends of an association.
 *)
val aends_of_association: association -> associationend list

(**
 * returns all associationends belonging to associationPath, excluding the 
 * associationend at source.
 * @params {source,associations,associationPath}
 *)
val oppositeAendsOfAssociation: Rep_OclType.OclType -> association list -> 
                                Rep_OclType.Path -> associationend list
(**
 * Does the opposite of oppositeAendsOfAssociation above. Returns only the 
 * associationends belonging to source.
 * @params {source,associations,associationPath}
 *)
val incomingAendsOfAssociation:  Rep_OclType.OclType -> association list -> 
                                 Rep_OclType.Path -> associationend list

(*****************************************
 *            PARENTS [ OK ]             *
 *****************************************)

(**
 * Returns the classifier of the parent of classifier.
 *) 
val parent_of           : Classifier -> transform_model -> Classifier

(**
 * Returns all parents of a classifier. 
 *)
val parents_of          : Classifier -> transform_model -> Classifier list

(** 
 * Returns one of the parents from the classifier.
 *)
val parent_name_of       : Classifier -> Rep_OclType.Path  

(** 
 * Returns the name of the package from the
 * parent class.
 *)
val parent_package_of    : Classifier -> Rep_OclType.Path  

(**
 * Returns the last part (last string in path) of the name
 * of the parent of the classifier.
 *)
val parent_short_name_of : Classifier -> string 

(**
 * Returns the types of the interfaces from 
 * the classifier.
 *)
val parent_interfaces_of : Classifier -> Rep_OclType.OclType list

(**
 * Returns the names of the interfaces from the
 * parents.
 *)
val parent_interface_names_of : Classifier -> Rep_OclType.Path list

(*****************************************
 *         SIGNATURES                    *
 *****************************************)

(** 
 * Prefixes all types in the signature with a 
 * given string list.
 *)
val prefix_signature        : string list -> (string * Rep_OclType.OclType) list -> (string * Rep_OclType.OclType) list

(** 
 * Test wheter the signatures are type consistent.
 *)
val sig_conforms_to           : (string * Rep_OclType.OclType) list -> (string * Rep_OclType.OclType) list -> transform_model -> bool


(*****************************************
 * RETURN Path/string                    *
 *****************************************)

(**
 * Returns the name of the classifier.
 *)
val name_of              : Classifier -> Rep_OclType.Path 

(** 
 * Returns the last part (last string in path) of 
 * the name of the classifier.
 *)
val short_name_of : Classifier -> string 

(** 
 * Returns the thy_name of a classifer.
 *)
val thy_name_of       : Classifier -> string

(**
 * Returns the name of the package.
 *)
val package_of           : Classifier -> Rep_OclType.Path 

(**
 * Returns all the packages of a model
 *)
val all_packages_of_model : transform_model -> Rep_OclType.Path list

(** 
 * Returns the package name of the template parameter.
 *)
val package_of_template_parameter : Rep_OclType.OclType -> string list

(** 
 * Path of the association.
 *)
val path_of_association: association -> Rep_OclType.Path

(** 
 * Name of the association.
 *)
val name_of_association: association -> Rep_OclType.Path

(**
 * returns the association this association end belongs to.
 * @params {aend}
 * @param aend association end
 * @return the path of the enclosing association
 *)
val association_of_aend : associationend -> Rep_OclType.Path

(**
 * returns the name of the association end.  The name of the association
 * end is the last part of the association end's path.
 * @params {aend}
 * @param aend association end
 * @return name of the association end as string.
 *)
val name_of_aend   : associationend -> string

(** 
 * Return the associationend as path
 *)
val name_of_ae          : associationend -> Rep_OclType.Path

(**
 * returns the path of an association end. The path of an association end
 * is <path_of_association>@[<name_of_aend>].
 * @params {aend}
 * @param aend association end
 * @return path of association end
 *)
val path_of_aend: associationend -> Rep_OclType.Path

(**
 * TODO: Description
 *)
val role_of_aend   : associationend -> string

(** 
 * Convert Path(string list) into a string.
 *)
val string_of_path      : Rep_OclType.Path -> string    

(**
 * TODO: Description
 *)
val short_name_of_path  : Rep_OclType.Path -> string    


(*****************************************
 * RETURN activity_graphs                *
 *****************************************)

(** 
 * TODO: Description
 *)
val activity_graphs_of: Classifier -> Rep_ActivityGraph.ActivityGraph list 


(*****************************************
 * TEMPORARLY SIGNATURE,
 * PLEASE DELETE IF NOT DELETED YET.
 * JUST FOR DEBUGGING PROPOSES.
 *****************************************)

val parse_string  : char -> char list -> (char list * char list)

exception InvalidArguments of string
exception TemplateError of string 
exception TemplateInstantiationError of string
exception GetClassifierError of string
exception UpcastingError of string
exception OperationNotFoundError of string
exception AttributeNotFoundError of string
exception NoParentForDatatype of string
exception NoModelReferenced of string
exception NoCollectionTypeError of Rep_OclType.OclType
exception AttributeAssocEndNameClash of string
exception ParentsOfError of string
exception Rep_CoreError of string
end

structure Rep_Core  :  REP_CORE = 
struct
open Rep_Logger
open Rep_Help_Functions
open Rep_OclTerm
open Rep_OclType
open XMI_DataTypes

type Visibility = XMI_DataTypes.VisibilityKind
type Scope      = XMI_DataTypes.ScopeKind

type operation = { 
     name          : string,	
     precondition  : (string option * Rep_OclTerm.OclTerm) list,
     postcondition : (string option * Rep_OclTerm.OclTerm) list,
     body          : (string option * Rep_OclTerm.OclTerm) list,
     arguments     : (string * Rep_OclType.OclType) list,
     result        : Rep_OclType.OclType,
     isQuery       : bool,
     scope         : Scope,
		   stereotypes    : string list,
     visibility    : Visibility	
}     	

type associationend = {
     name         : Rep_OclType.Path,
     aend_type    : Rep_OclType.OclType,
     multiplicity : (int*int) list,
     visibility   : Visibility,
     ordered      : bool,
     init         : Rep_OclTerm.OclTerm option
}

type attribute = {
     name        : string,
     attr_type   : Rep_OclType.OclType,
     visibility  : Visibility,
     scope       : Scope,
     stereotypes : string list,
     init        : Rep_OclTerm.OclTerm option
}


type association = { 
     name: Rep_OclType.Path,
     aends: associationend list,
     qualifiers: (string * attribute list) list,
     aclass: Rep_OclType.Path option
}

type constraint = (string option * Rep_OclTerm.OclTerm)

datatype Classifier =  
	 Class of 
	 { name        : Rep_OclType.OclType, 
	   parent      : Rep_OclType.OclType option,
	   attributes  : attribute list,
	   operations  : operation list,
	   associations: Rep_OclType.Path list,
	   invariant   : (string option * Rep_OclTerm.OclTerm) list,
	   stereotypes : string list,
	   interfaces  : Rep_OclType.OclType list,
	   thyname     : string option,
	   visibility  : Visibility,
           activity_graphs : Rep_ActivityGraph.ActivityGraph list
	  }
      | AssociationClass of
	 { name        : Rep_OclType.OclType, 
	   parent      : Rep_OclType.OclType option,
	   attributes  : attribute list,
	   operations  : operation list,
	   invariant   : (string option * Rep_OclTerm.OclTerm) list,
	   stereotypes : string list,
	   interfaces  : Rep_OclType.OclType list,
	   thyname     : string option,
           activity_graphs    : Rep_ActivityGraph.ActivityGraph list,
	   associations: Rep_OclType.Path list,
	   visibility  : Visibility,
	   association: Rep_OclType.Path
	 }
       | Interface of               (* not supported yet *)
	 { name        : Rep_OclType.OclType,
	   parents     : Rep_OclType.OclType list, 
	   operations  : operation list,
	   stereotypes : string list,
	   invariant   : (string option * Rep_OclTerm.OclTerm) list,
	   thyname     : string option
	  }
       | Enumeration of (* not really supported yet? *)
	 { name        : Rep_OclType.OclType,
	   parent      : Rep_OclType.OclType option,
	   operations  : operation list,
	   literals    : string list,
	   invariant   : (string option * Rep_OclTerm.OclTerm) list,
	   stereotypes : string list,
	   interfaces  : Rep_OclType.OclType list,
	   thyname     : string option
	  }
       | Primitive of (* not really supported yet *)
	 { name        : Rep_OclType.OclType,
	   parent      : Rep_OclType.OclType option,
	   operations  : operation list,
	   associations: Rep_OclType.Path list,
	   invariant   : (string option * Rep_OclTerm.OclTerm) list,
	   stereotypes : string list,
	   interfaces  : Rep_OclType.OclType list,
	   thyname     : string option
	  } 
       | Template of 
	 { parameter   : Rep_OclType.OclType,
	   classifier  : Classifier
	 }

type transform_model = (Classifier list * association list)

exception InvalidArguments of string
exception TemplateError of string 
exception TemplateInstantiationError of string
exception GetClassifierError of string
exception UpcastingError of string
exception NoParentForDatatype of string
exception NoModelReferenced of string
exception NoCollectionTypeError of Rep_OclType.OclType
exception OperationNotFoundError of string
exception AttributeNotFoundError of string
exception AttributeAssocEndNameClash of string
exception ParentsOfError of string
exception Rep_CoreError of string


val OclLibPackage = "oclLib"
val OclAnyC = Class{name=Rep_OclType.OclAny,parent=NONE,attributes=[],
		    operations=[], interfaces=[],
		    invariant=[],stereotypes=[], associations=[],
		    thyname=NONE,
		    visibility = public,
                    activity_graphs=nil}
              
val OclAnyAC = AssociationClass{name=Rep_OclType.OclAny,parent=NONE,
                                attributes=[],operations=[], interfaces=[],
			        invariant=[],stereotypes=[], associations=[],
				visibility = public,
			        association= []:Path (*FIXME: sensible dummy*),
			        thyname=NONE, activity_graphs=nil}

fun type_of (Class{name,...})            = name  
  | type_of (AssociationClass{name,...}) = name
  | type_of (Interface{name,...})        = name
  | type_of (Enumeration{name,...})      = name
  | type_of (Primitive{name,...})        = name
  | type_of (Template{classifier,...})   = type_of classifier 

fun type_of_term (Literal (s,typ)) = typ
  | type_of_term (TupleLiteral(fst,ftype,snd,stype)) = Tuple(ftype,stype)
  | type_of_term (AttributeCall (t,typ,p,res_typ)) = res_typ
  | type_of_term (AssociationEndCall (t,typ,p,res_typ)) = res_typ
  | type_of_term (OperationCall (t,typ,p,l,res_typ)) = res_typ
  | type_of_term (Variable (s,typ)) = typ
  | type_of_term (CollectionLiteral (set,typ)) = typ
  | type_of_term (Iterator (_,_,_,_,_,_,res_typ)) = res_typ
  | type_of_term (If(_,_,_,_,_,_,res_typ)) = res_typ
  | type_of_term (OperationWithType (_,_,_,_,res_typ)) = res_typ
  | type_of_term (Let (_,_,_,_,_,res_typ)) = res_typ
  | type_of_term (Iterate (_,_,_,_,_,_,_,_,res_typ)) = res_typ
  | type_of_term (Predicate (_,_,_,_)) = Boolean


fun name_of (Class{name,...})            = path_of_OclType name  
  | name_of (AssociationClass{name,...}) = path_of_OclType name
  | name_of (Interface{name,...})        = path_of_OclType name
  | name_of (Enumeration{name,...})      = path_of_OclType name
  | name_of (Primitive{name,...})        = path_of_OclType name
  | name_of (Template{classifier,...})   = name_of classifier

fun short_name_of_path p = (hd o rev) p

fun name_of_op ({name,...}:operation) = name

fun name_of_att ({name,...}:attribute) = name

fun type_of_aend ({name,aend_type,...}:associationend) = aend_type

fun name_of_aend ({name,aend_type,...}:associationend) = 
    short_name_of_path name


fun mangled_name_of_op ({name,arguments,result,...}:operation) = 
    let
      val arg_typestrs = map (fn a => (Rep_OclType.string_of_OclType o #2 )a )
                             arguments
    in 
      foldr1 (fn (a,b) =>(a^"_"^b)) 
             ((name::arg_typestrs)@[Rep_OclType.string_of_OclType result])
    end
    
                                                 
fun result_of_op ({result,...}:operation) = result
                                            
fun arguments_of_op ({arguments,...}:operation) = arguments



fun local_operations_of (Class{operations,...}) = operations
  | local_operations_of (AssociationClass{operations,...}) = operations
  | local_operations_of (Interface{operations,...})      = operations
  | local_operations_of (Enumeration{operations,...})    = operations
  | local_operations_of (Primitive{operations,...})      = operations  
  | local_operations_of (Template{parameter,classifier}) = raise OperationNotFoundError ("..._operations_of a template not possible.\n")


fun local_attributes_of (Class{attributes,...}) = attributes
  | local_attributes_of (AssociationClass{attributes,...}) = attributes					    
  | local_attributes_of (Interface{...})        = 
         error "in Rep.local_attributes_of: argument is Interface"
  | local_attributes_of (Enumeration{...})      = 
         error "in Rep.local_attributes_of: argument is Enumeration"  
  | local_attributes_of (Primitive{...})         = []  
  | local_attributes_of (Template{parameter,classifier}) = raise AttributeNotFoundError ("..._attributes_of a template not possible.\n")

fun operations_of class = local_operations_of class

fun attributes_of class = local_attributes_of class

fun class_of_design_model path (model as (clist,alist)) = 
    let
	val _ = trace rep_core ("path of class = " ^ (String.concat (path)) ^ "\n")
    in
	case (List.find (fn a => (type_of a) = Classifier (path)) clist) of
	    NONE => raise GetClassifierError (String.concat path)
	  | SOME(x) => x
    end

fun type_of_parent (Class {parent,...}) = 
    let
	val _ = trace development ("type_of_parent : Class{parent,...} \n")
    in
	Option.valOf(parent)
	handle Option.Option => OclAny
    end
  | type_of_parent (AssociationClass {parent,...}) = 
    let
	val _ = trace development ("type_of_parent : AssociationClass{parent,...} \n")
    in
	Option.valOf(parent) 
	handle Option.Option => OclAny
    end
  | type_of_parent (Primitive {parent, ...}) = 	Option.valOf(parent)
  | type_of_parent (Interface {parents, ...}) = (List.hd parents)
  | type_of_parent (Template{classifier,...}) = raise TemplateError ("Parent of a class can never be of type template(x).\n")

(* RETURN: OclType list *)

fun parse_string c ([]) = ([],[])
	      | parse_string c (h::tail) =
		if (c = h) then
		    ([],h::tail)
		else
 		    (h::(#1 (parse_string c tail)),(#2 (parse_string c tail)))



fun simple_type_of_path ["Integer"] = Integer
  | simple_type_of_path ["Boolean"] = Boolean
  | simple_type_of_path ["Real"] = Real
  | simple_type_of_path ["OclAny"] = OclAny
  | simple_type_of_path ["DummyT"] = DummyT
  | simple_type_of_path ["String"] = String
  | simple_type_of_path ["OclVoid"] = OclVoid
  | simple_type_of_path (("oclLib")::tail) = simple_type_of_path tail 
  | simple_type_of_path [set] = 
    if (List.exists (fn a => if (a = (#"(")) then true else false) (String.explode set)) then
	(* set *)
	let
	    fun string_to_cons "Set" typ = Set(typ)
	      | string_to_cons "Bag" typ = Bag(typ)
	      | string_to_cons "Collection" typ = Collection (typ)
	      | string_to_cons "OrderedSet" typ = OrderedSet (typ)
	      | string_to_cons "Sequence" typ = Sequence (typ)	
	    val tokens = parse_string (#"(") (String.explode set)
	    val cons = (#1 tokens)
	    (* delete first "(" and last ")" element *)
	    val tail = List.tl (real_path (#2 tokens))
	    val _ = TextIO.output(TextIO.stdOut,"tail "^ (String.implode tail) ^ "\n")

	in
	    string_to_cons (String.implode cons) (simple_type_of_path ([String.implode tail]))
	end
    else 
	Classifier([set])
  | simple_type_of_path (list:Path) = Classifier (list)



fun type_of_path ["Integer"] (model:transform_model) = Integer
  | type_of_path ["Boolean"] (model:transform_model) = Boolean
  | type_of_path ["Real"] (model:transform_model) = Real
  | type_of_path ["OclAny"] (model:transform_model) = OclAny
  | type_of_path ["DummyT"] (model:transform_model) = DummyT
  | type_of_path ["String"] (model:transform_model) = String
  | type_of_path ["OclVoid"] (model:transform_model) = OclVoid
  | type_of_path (("oclLib")::tail) (model:transform_model) = type_of_path tail model
  | type_of_path [set] (model:transform_model) = 
    if (List.exists (fn a => if (a = (#"(")) then true else false) (String.explode set)) then
	(* set *)
	let
	    fun string_to_cons "Set" typ = Set(typ)
	      | string_to_cons "Bag" typ = Bag(typ)
	      | string_to_cons "Collection" typ = Collection (typ)
	      | string_to_cons "OrderedSet" typ = OrderedSet (typ)
	      | string_to_cons "Sequence" typ = Sequence (typ)	
	    val tokens = parse_string (#"(") (String.explode set)
	    val cons = (#1 tokens)
	    (* delete first "(" and last ")" element *)
	    val tail = List.tl (real_path (#2 tokens))
	    val _ = TextIO.output(TextIO.stdOut,"tail "^ (String.implode tail) ^ "\n")

	in
	    string_to_cons (String.implode cons) (type_of_path ([String.implode tail]) model)
	end
    else 
	let
	    val cl = class_of_design_model [set] model
	in
	    type_of cl
	end
  | type_of_path (list:Path) (model:transform_model) = 
    let
	val cl = class_of_design_model list model 
    in
	type_of cl
    end

and class_of_term source (c:Classifier list, a:association list) =
    let
	val typ = type_of_term (source)
	val _ = trace rep_core ("type_of_term term = " ^ (string_of_OclType typ) ^ "\n")
	fun class_of_t typ m = 
	    hd (List.filter (fn a => if ((type_of a) = typ) then true else false) m)
	fun substitute_classifier typ classifier =
	    let
		fun substitute_args typ [] = []
		  | substitute_args typ ((s,t)::tail) =
		    let 
			val _ = trace low ("substitute argument : " ^ (string_of_OclType typ) ^ " template parameter of " ^ (string_of_OclType t) ^ " \n")
		    in
			(s,substitute_typ typ t)::(substitute_args typ tail)
		    end
		and substitute_parent (Collection(t)) typ = NONE
		  | substitute_parent (Set(t)) typ = SOME(Collection(typ))
		  | substitute_parent (OrderedSet(t)) typ = SOME(Set(typ))
		  | substitute_parent (Bag(t)) typ = SOME(Collection(typ))
		  | substitute_parent (Sequence(t)) typ = SOME(Collection(typ))
		  | substitute_parent x typ = raise TemplateInstantiationError ("Parent tmpl type must be a collection.\n")

		and substitute_operations typ [] = []
		  | substitute_operations typ ((oper:operation)::tail) =
		    let 
			val _ = trace low ("substitute operation : " ^ (#name oper) ^ " ... \n")
			val args = substitute_args typ (#arguments oper)
			val res = substitute_typ typ (#result oper)
			val _ = trace 100 ("check\n")
		    in
			({
			 name = #name oper,
			 postcondition = #postcondition oper,
			 precondition = #precondition oper,
			 body = #body oper,
			 arguments = args,
			 result = res,
			 visibility = #visibility oper,
			 isQuery = #isQuery oper,
			 stereotypes = #stereotypes oper, 
			 scope = #scope oper
			 }:operation)::(substitute_operations typ tail)
		    end
		and substitute_typ typ templ_type =
		    let 
			val _ = trace low ("substitute type : " ^ (string_of_OclType typ) ^ " instead of " ^ (string_of_OclType templ_type) ^ " \n")
		    in    
			case templ_type of  
			    (* innerst type *)
			    Sequence(TemplateParameter "T") => Sequence (typ)
			  | Set(TemplateParameter "T") => Set(typ)
			  | OrderedSet(TemplateParameter "T") => OrderedSet (typ)
			  | Collection(TemplateParameter "T") => Collection(typ)
			  | Bag(TemplateParameter "T") => Bag(typ)
			  (* nested types *)
			  | Sequence (t) => Sequence (substitute_typ typ t)
			  | Set (t) => Set (substitute_typ typ t)
			  | OrderedSet (t) => OrderedSet (substitute_typ typ t)
			  | Collection (t) => Collection (substitute_typ typ t)
			  | Bag (t) => Bag (substitute_typ typ t)
			  (* only parameter  *)
			  | TemplateParameter "T" => typ
			  (* basic types *)
			  | OclAny => OclAny
			  | OclVoid => OclVoid
			  | Integer => Integer
			  | Real => Real
			  | String => String
			  | Boolean => Boolean
			  | DummyT => DummyT
			  | Classifier (path) => Classifier (path)
			  (* else error *)
			  | _ => raise TemplateInstantiationError ("Template type not of type: Sequence, Set, OrderedSet, Collection or Bag")
		    end
		val _ = trace rep_core ("substitute classifier: parameter type: " ^ string_of_OclType typ ^ " template type: " ^ string_of_OclType (type_of classifier) ^ "\n") 
                (* val typ = parameter type *)
		val styp = substitute_typ typ (type_of classifier)
		val _ = trace rep_core ("substitute_classifier: end substitute_type \n")
		val ops = substitute_operations typ (local_operations_of classifier)
		val _ = trace 100 ("substitute parent.\n")
			
		val sparent = substitute_parent (type_of classifier) typ
		val _ = trace 100 ("end substitute parent.\n")
	    in
		(Class
		     {
		      name = styp,
		      (* take the parent of the template parameter *)
		      parent = sparent,
		      (* a template has no attributes *)
		      attributes = [],
		      operations = ops,
		      (* a template has no associationends *)
		      associations = [],
		      (* a template has no invariants *)
		      invariant = [],
		      (* a template has no stereotypes *)
		      stereotypes = [],
		      (* a template has no interfaces *)
		      interfaces = [],
		      (* a template's thyname is NONE *)
		      thyname = NONE,
		      (* a template's visibility is public *)
		      visibility = public:Visibility,
		      (* a template has no activity_graphs *)
		      activity_graphs = []
		})
	    end
	fun  templ_of temp_typ para_typ [] = raise TemplateInstantiationError ("Error during instantiating a template" ^ "\n")
	   | templ_of temp_typ para_typ (Template{parameter,classifier}::tail) =
	     let
		 val _ = trace low ("Instantiate Template for classifier: " ^ (string_of_OclType (type_of classifier)) ^ "\n")
	     in
		 if ((type_of classifier) = temp_typ) then
		     substitute_classifier para_typ classifier
		 else
		     templ_of temp_typ para_typ tail
	     end
	   | templ_of temp_typ para_typ (h::tail) = 
	     let
		 val _ = trace development ("shit")
	     in
		 templ_of temp_typ para_typ tail
	     end  
    in
	case typ of
	    (* Primitive types of lib *)
	    Boolean => class_of_t Boolean c
	  | Integer => class_of_t Integer c
	  | Real => class_of_t Real c
	  | String => class_of_t String c
	  (* Template types of lib *)
	  | Sequence (T) => templ_of (Sequence(TemplateParameter "T")) T c
	  | Set (T) => templ_of (Set(TemplateParameter "T")) T c
	  | OrderedSet (T) => templ_of (OrderedSet(TemplateParameter "T")) T c
	  | Bag (T) => templ_of (Bag(TemplateParameter "T")) T c
	  | Collection (T) => templ_of (Collection(TemplateParameter "T")) T c
	  (* Class types of lib *)
	  | OclVoid => class_of_t OclVoid c
	  | OclAny => 
	    let 
		val _ = trace rep_core ("type is OclAny")
	    in
		class_of_t OclAny c
	    end
	  (* Model types *)
	  | Classifier (path) =>
	    let
		val _ = trace development ("class_of_term: Classifier ("^(string_of_path path)^")\n")
		val res = class_of_t (Classifier (path)) c
		val _ = trace development ("found: "^(string_of_path (name_of res)) ^"\n")
	    in
		(*class_of_t (Classifier (path)) model*)
		res
	    end
	  | DummyT => 
	    let
		val _ = trace development ("GetClassifierError: DummyT \n")
	    in
		raise GetClassifierError ("No classifier of type: 'DummyT' \n")
	    end
	  | TemplateParameter (string) => 
	    let
		val _ = trace development ("GetClassifierError: TemplateParameter ("^ string ^") \n")
	    in
		raise GetClassifierError ("No classifier of type: 'TemplateParameter (string)' \n")
	    end
    end

and class_of (name:Path) (model as (clist,alist)) = 
     let
	 val _ = trace rep_core ("top level package: " ^ (List.hd (name)) ^ "\n")
	 val _ = trace rep_core ("remaining package: " ^ (String.concat (List.tl name)) ^ "\n") 
     in
	 class_of_term (Variable("x",type_of_path name model)) model
	 handle TemplateInstantiationError s =>
		let
		    val _ = trace rep_core ("The path of the template parameter is not in the desing model.\n")
		    val _ = trace rep_core ("Path = " ^ s ^ "\n")
		in
		    raise TemplateError ("shit\n")
		end
     end

and class_of_type (typ:OclType) (model:transform_model) = 
    class_of_term (Variable ("x",typ)) model

and type_of_parents (Primitive {parent,...}) (model:transform_model) =
    (    
     case parent of 
	 NONE => [OclAny]
       | SOME (OclAny) => [OclAny]
       | SOME (t) => (t)::(type_of_parents (class_of_type t model) model)
    )
  | type_of_parents (Class {parent,...}) model =
    (
     case parent of
	 NONE => [OclAny]
       | SOME (OclAny) => [OclAny]
       | SOME (t) => (t)::(type_of_parents (class_of_type t model) model)
    )
  | type_of_parents (AssociationClass {parent,...}) model =
    (
     case parent of
	 NONE => [OclAny]
       | SOME (OclAny) => [OclAny]
       | SOME (t) => (t)::(type_of_parents (class_of_type t model) model)
    )
  | type_of_parents (Interface {parents,...}) model = parents
  | type_of_parents (Template {classifier,...}) model =
    raise TemplateInstantiationError ("During Instantiation of template parent needn't to be accessed")


fun type_equals Integer (Classifier ([OclLibPackage,"Real"])) = true
  | type_equals (Classifier ([OclLibPackage,"Integer"])) Real = true
  | type_equals _ OclAny = true
  | type_equals _ (Classifier ([OclLibPackage,"OclAny"])) = true
  | type_equals x y = 
    if (x = y) then
	true
    else
	false

fun conforms_to_up _ OclAny (_:transform_model) = true
  | conforms_to_up (Set(T1)) (Collection(T2)) model =
    let
	val _ = trace low ("conforms_to_up: set -> collection \n")
    in
	if (conforms_to T1 T2 model) then 
	    true
	else 
	    false
    end
  | conforms_to_up (Bag(T1)) (Collection(T2)) model =
    let
	val _ = trace low ("conforms_to_up: bag -> collection \n")
    in    
	if (conforms_to T1 T2 model) then 
	    true
	else 
	    false
    end
  | conforms_to_up (Sequence(T1)) (Collection(T2)) model =
    let
	val _ = trace low ("conforms_to_up: sequence -> collection \n")
    in
	if (conforms_to T1 T2 model) then 
	    true
	else 
	    false
    end
  | conforms_to_up (OrderedSet(T1)) (Collection(T2)) model =
    let
	val _ = trace low ("conforms_to_up:  orderedset -> collection \n")
    in
	if (conforms_to T1 T2 model) then 
	    true
	else 
	    false
    end
  | conforms_to_up typ1 typ2  (model as(classifiers,associations)) = 
    let
	val class = class_of_type typ1 model
	val parents_types = type_of_parents (class) model
	val _ = trace low ("conforms_to_up:  ... \n")
    in
	member (typ2) (parents_types)
    end

and
(* RETRUN: Boolean *)
conforms_to x y (model:transform_model) =
    let
	val _ = trace low ("conforms_to: " ^ string_of_OclType x ^ " -> " ^ string_of_OclType y ^ " ? \n")
    in
	if (x = y) then 
	    true
	else
	    if (type_equals x y) then
		true
	    else
		conforms_to_up x y model
    end

(* RETURN: OclTerm *)
and upcast (term,typ) =  
    if (type_equals (type_of_term term) typ) then
	term
    else
	OperationWithType (term,type_of_term term,"oclIsTypeOf",typ,typ)


fun parent_name_of (C as Class{parent,...}) = 
    (case parent  of NONE   => name_of OclAnyC
		   | SOME p => path_of_OclType p ) 
  | parent_name_of (AC as AssociationClass{parent,...}) = 
    (case parent  of NONE   => name_of OclAnyAC
		   | SOME p => path_of_OclType p )
  | parent_name_of (Interface{...}) =
    error "in Rep.parent_name_of: unsupported argument type Interface"
  | parent_name_of (E as Enumeration{parent,...}) = 
    (case parent  of NONE => error ("in Rep.parent_name_of: Enumeration "^
                                    ((string_of_path o name_of) E)
                                    ^" has no parent")
		   | SOME p  => path_of_OclType p )  
  | parent_name_of (D as Primitive{parent,...})    = 
    (case parent  of NONE => name_of OclAnyC
    (* error ("Primitive "^((string_of_path o name_of) D)^" has no parent") *)
		   | SOME p  => path_of_OclType p )
  | parent_name_of (Template _) = 
    error "in Rep.parent_name_of: unsupported argument type Template"

fun sig_conforms_to [] [] model = true
  | sig_conforms_to [] list model = 
    let
	val result = false
    in
	result
    end
  | sig_conforms_to list [] model = 
    let
	val result = false 
    in
	result
    end 
  | sig_conforms_to [(s1:string,sig_sub:OclType)] [(s2:string,sig_super:OclType)] model = 
    let
	val result = if (conforms_to (sig_sub) (sig_super) model) then 
			 true 
		     else 
			 false    
    in
	result
    end
  | sig_conforms_to ((s1:string,sig_sub:OclType)::tail1)  ((s2:string,sig_super:OclType)::tail2) model =
    let
	val result = if (s2 = s1 andalso (conforms_to (sig_sub) (sig_super) model)) then 
			 sig_conforms_to tail1 tail2 model
		     else 
			 false   
    in
	result 
    end


fun operation_of cl fq_name = 
    let 
      val classname   = (rev o  tl o rev) fq_name
      val operations  = operations_of (class_of classname cl)
      val name        = (hd o rev) fq_name	
    in
      SOME(hd (filter (fn a => if ((name_of_op a) = name)
			       then true else false ) operations ))
    end	


fun type_of_att ({name,attr_type,...}:attribute) = attr_type

fun same_op (sub_op:operation) (super_op:operation) (model:transform_model) =
    if ((name_of_op sub_op = name_of_op super_op ) andalso (sig_conforms_to (arguments_of_op sub_op) (arguments_of_op super_op) model))
    then true
    else false

fun same_att (sub_att:attribute) (super_att:attribute) (model:transform_model) = 
    if ((name_of_att sub_att = name_of_att super_att) andalso (conforms_to (type_of_att sub_att) (type_of_att super_att) model))
    then true
    else false

fun same_ae (sub_ae:associationend) (super_ae:associationend) (model:transform_model) = 
    if ((name_of_aend sub_ae = name_of_aend super_ae) andalso (conforms_to (type_of_aend sub_ae) (type_of_aend super_ae) model))
    then true
    else false

(* embed local operations to the inherited operations *)
fun embed_local_operations [] iops model = iops
  | embed_local_operations x [] model = x
  | embed_local_operations (h::tail) iops model = 
    let
	fun embed_local_operation oper [] model = [oper]
	  | embed_local_operation lop ((oper:operation)::iops) model = 
	    if (same_op lop oper model)
	    then (lop::iops)
	    else (oper)::(embed_local_operation lop iops model) 
	val tmp = embed_local_operation h iops model
    in
	(embed_local_operations tail tmp model)
    end

fun embed_local_attributes [] iatts model = iatts
  | embed_local_attributes x [] model = x
  | embed_local_attributes (h::tail) iatts model = 
    let
	fun embed_local_attribute att [] model = [att]
	  | embed_local_attribute latt ((att:attribute)::iatts) model = 
	    if (same_att latt att model)
	    then (latt::iatts)
	    else (att)::(embed_local_attribute latt iatts model)
	val tmp = embed_local_attribute h iatts model
    in
	(embed_local_attributes tail tmp model)
    end

fun embed_local_assocEnds [] iassE model = iassE
  | embed_local_assocEnds x [] model = x
  | embed_local_assocEnds (h::tail) iassEs model = 
    let
	fun embed_local_assocEnd assE [] model = [assE]
	  | embed_local_assocEnd lassE ((assE:associationend)::iassEs) model =
	    if (same_ae lassE assE model)
	    then (lassE::iassEs)
	    else (assE)::(embed_local_assocEnd lassE iassEs model)
	val tmp = embed_local_assocEnd h iassEs model
    in
	(embed_local_assocEnds tail tmp model)
    end

fun isColl_Type (Set(x)) = true
  | isColl_Type (Sequence(x)) = true
  | isColl_Type (OrderedSet(x)) = true
  | isColl_Type (Bag(x)) = true
  | isColl_Type (Collection(x)) = true
  | isColl_Type _ = false


fun isValueType Integer         = true
  | isValueType String          = true
  | isValueType Real            = true
  | isValueType Boolean         = true
  | isValueType (Set(t))        = false
  | isValueType (OrderedSet(t)) = false
  | isValueType (Bag(t))        = false
  | isValueType (Sequence(t))   = false
  | isValueType OclAny          = false
  | isValueType (Classifier s)  = false
  | isValueType DummyT          = false
  | isValueType OclVoid         = false
  | isValueType t               = error ("Error in isValueType(_,"^(string_of_OclType t)^")")





fun parent_of_template (cl as Class{parent,...}:Classifier) (model:transform_model) = 
    (case parent of 
	 NONE => ( case (isColl_Type (type_of cl)) of
		       false => class_of_type OclAny model
		     | true => class_of_type (Collection(OclAny)) model
		 )
       | SOME(x) => class_of_type x model
    )
  | parent_of_template (asso as AssociationClass{parent,...}:Classifier) (model:transform_model) = 
    (case parent of
	 NONE => ( case (isColl_Type (type_of asso)) of
		       false => class_of_type OclAny model
		     | true => class_of_type (Collection(OclAny)) model
		 ) 
      | SOME(x) => class_of_type x model)
  | parent_of_template (enum as Enumeration{parent,...}:Classifier) (model:transform_model) = 
        (case parent of
	 NONE => ( case (isColl_Type (type_of enum)) of
		       false => class_of_type OclAny model
		     | true => class_of_type (Collection(OclAny)) model
		 ) 
      | SOME(x) => class_of_type x model)
  | parent_of_template (primi as Primitive{parent,...}:Classifier) (model:transform_model) = 
        (case parent of
	 NONE => ( case (isColl_Type (type_of primi)) of
		       false => class_of_type OclAny model
		     | true => class_of_type (Collection(OclAny)) model
		 ) 
      | SOME(x) => class_of_type x model)
  | parent_of_template (inf as Interface{parents,...}:Classifier) (model:transform_model) =
    (class_of_type (List.hd (parents)) model
    handle List.Empty => (case (isColl_Type (type_of inf)) of 
					       false => class_of_type OclAny model
					     | true => class_of_type (Collection(OclAny)) model
			   )
    )		   
  | parent_of_template (tmp as Template{classifier,...}) (model:transform_model) = raise TemplateError ("parent_of_template should never be used during Instantiation of a template.\n")

    
fun parents_of_help (C:Classifier) (model:transform_model) = 
    let
	val this_type = type_of C
	val _ = trace rep_core ("type of C = " ^ (string_of_OclType this_type) ^ "\n")
    in
	case this_type of
	    OclAny => []
	  | Collection(OclAny) => []
	  | Set(OclAny) => []
	  | Bag(OclAny) => []
	  | Sequence(OclAny) => []
	  | OrderedSet(OclAny) => []
	  | Collection(T) =>
	    let
		val class_T = class_of_type (T) model
		val class_T' = parent_of_template class_T model
		val T' = type_of class_T'
		val col_T' = class_of_type (Collection(T')) model
	    in
		[col_T']@(parents_of_help col_T' model)
	    end
	  | Set(T) => 
	    let
		val col_T = class_of_type (Collection(T)) model
		val class_T = class_of_type (T) model
		val class_T' = parent_of_template class_T model
		val T' = type_of class_T'
		val set_T' = class_of_type (Set(T')) model
	    in
		[col_T]@(parents_of_help col_T model)@[set_T']@(parents_of_help set_T' model)
	    end
	  | OrderedSet(T) => 
	      let
		  val set_T = class_of_type (Set(T)) model
		  val col_T = class_of_type (Collection(T)) model
		  val class_T = class_of_type (T) model
		  val class_T' = parent_of_template class_T model
		  val T' = type_of class_T'
		  val ordset_T' = class_of_type (OrderedSet(T')) model
	      in
		 [set_T]@(parents_of_help set_T model)@[col_T]@(parents_of_help col_T model)@[ordset_T']@(parents_of_help ordset_T' model)
	      end
	    | Bag(T) =>
	      let
		  val col_T = class_of_type (Collection(T)) model
		  val class_T = class_of_type (T) model
		  val class_T' = parent_of_template class_T model
		  val T' = type_of class_T'
		  val bag_T' = class_of_type (Bag(T')) model
	      in
		  [col_T]@(parents_of_help col_T model)@[bag_T']@(parents_of_help bag_T' model)
	      end
	    | Sequence(T) => 
	      let
		  val col_T = class_of_type (Collection(T)) model
		  val class_T = class_of_type (T) model
		  val class_T' = parent_of_template class_T model
		  val T' = type_of class_T'
		  val seq_T' = class_of_type (Sequence(T')) model
	      in
		  [col_T]@(parents_of_help col_T model)@[seq_T']@(parents_of_help seq_T' model)
	      end
	    | some_type => 
	      let 
		  val _ = trace rep_core ("parent_of_template \n")
		  val parent = parent_of_template C model
		  val _ = trace rep_core ("parent_of_template end, classifier = " ^ (String.concat (name_of parent)) ^ "\n")
	      in
		  [parent]@(parents_of_help parent model)
	      end
	      handle Empty => [OclAnyC]
    end

fun parent_of (C:Classifier) model = parent_of_template C model
				     handle Empty => OclAnyC

fun parents_of (C:Classifier) model = 
    let
	val _ = trace rep_core ("parents_of ... \n")
    in
	(if (parents_of_help C model = [])
	then
	    (
	     if (isColl_Type (type_of C)) 
	     then [(class_of_type (Collection(OclAny)) model)]
	     else [(class_of_type (OclAny) model)]
	    )
	else
	    remove_dup (parents_of_help C model)
	)
	handle Empty => [OclAnyC]
    end



fun name_of_association ({name,aends,qualifiers,aclass}:association) = name

fun path_of_association assoc = name_of_association assoc

fun aends_of_association {name,aends,qualifiers,aclass} = aends

fun associations_of (Class{name,associations,...}) = associations
  | associations_of (AssociationClass{name,associations,association,...}) = associations  
  | associations_of (Primitive{name,associations,...}) = associations
                                                             
fun oppositeAendsOfAssociation name allAssociations associationPath =
    let
      val [association] = List.filter (fn assoc => path_of_association assoc = associationPath) allAssociations
    in
      List.filter (fn aend => type_of_aend aend <> name) (aends_of_association association)
    end

fun incomingAendsOfAssociation name allAssociations associationPath =
    let
      val [association] = List.filter (fn assoc => path_of_association assoc = associationPath) allAssociations
    in
      List.filter (fn aend => type_of_aend aend = name) (aends_of_association association)
    end

(** find the associationends belonging to a classifier.
 * This mean all other associationends from all associations the
 * classifer is part of. For association classes, the belonging 
 * association also needs to be checked.
 * If the association is reflexiv, all aends will be returned.
 *)


fun local_associationends_of (all_associations:association list) (Class{name,associations,...}):associationend list = 
    let 
	val _ = trace rep_core ("local_associationends_of 1 ... \n")
	val _ = trace rep_core ("classifier = " ^ (string_of_OclType name) ^ "\n")
	val oppAends = List.concat (List.map (fn a => 
						 let
						     val _ = trace rep_core ("Association path = ")
						     val _ = trace rep_core (string_of_path a ^ "\n")
						 in
						     (oppositeAendsOfAssociation name all_associations a)
						 end
					     ) associations)
	val _ = trace rep_core ("local_associationends_of 2 ... \n")
	val selfAends = map (incomingAendsOfAssociation name all_associations) associations
	val _ = trace rep_core ("local_associationends_of 3 ... \n")
	val filteredSelfAends = List.concat (List.filter (fn x => length x >= 2) selfAends)
	val _ = trace rep_core ("local_associationends_of 4 ... \n")
    in
        oppAends@filteredSelfAends
    end
  | local_associationends_of all_associations (AssociationClass{name,associations,association,...}) = 
    (* association only contains endpoints to the other, pure classes *)
    let
	val _ = trace rep_core ("local_associationends_of 1 AssoCl ... \n")
	val assocs = if List.exists (fn x => x = association ) associations 
                     then associations
		     else association::associations
	val _ = trace rep_core ("local_associationends_of 2 AssoCl ... \n")
	val oppAends = List.concat (map (oppositeAendsOfAssociation name all_associations) assocs)
	val _ = trace rep_core ("local_associationends_of 3 AssoCl ... \n")
	val selfAends = map (incomingAendsOfAssociation name all_associations) associations
	val _ = trace rep_core ("local_associationends_of 4 AssoCl ... \n")
	val filteredSelfAends = List.concat (List.filter (fn x => length x >= 2) selfAends)
	val _ = trace rep_core ("local_associationends_of 5 AssoCl ... \n")
    in
        oppAends@filteredSelfAends
    end
  | local_associationends_of all_associations (Primitive{name,associations,...}) = []
    (* let 
	val _ = trace rep_core ("local_associationends_of 1 Primi... \n")
	val oppAends = List.concat (map (oppositeAendsOfAssociation name all_associations) associations)
	val _ = trace rep_core ("local_associationends_of 2 primi ... \n")
	val selfAends = map (incomingAendsOfAssociation name all_associations) associations
	val _ = trace rep_core ("local_associationends_of 3 primi ... \n")
	val filteredSelfAends = List.concat (List.filter (fn x => length x >= 2) selfAends)
	val _ = trace rep_core ("local_associationends_of 4 primi ... \n")
    in
        oppAends@filteredSelfAends
    end *)
  | local_associationends_of _ _ = error ("in local_associationends_of: This classifier has no associationends") (*FIXME: or rather []? *)
fun associationends_of assocs classes = local_associationends_of assocs classes                             
    
(* get all inherited operations of a classifier, without the local operations *)
fun inherited_operations_of class (model as (clist,alist)) =
    let
	val _ = trace rep_core ("inh ops 0\n")
	val c_parents = parents_of class model
	val _ = trace 50 ("inh ops: parents = " ^ (String.concat (List.map (fn a => (string_of_path (name_of a))) c_parents)) ^ " \n")
	val ops_of_par = (List.map (operations_of) c_parents)
	val _ = trace 50 ("inh ops 2\n")
    in
	List.foldr (fn (a,b) => embed_local_operations a b model) (List.last (ops_of_par)) ops_of_par
    end

fun inherited_attributes_of class (model as (clist,alist)) = 
    let
	val _ = trace rep_core ("inh att 0\n")
	val c_parents = parents_of class model
	val _ = trace rep_core ("inh att 0\n")
	val atts_of_par = (List.map (attributes_of) c_parents)
	val _ = trace rep_core ("inh att 0\n")
    in
	if (List.length(atts_of_par) = 0)
	then []
	else List.foldr (fn (a,b) => embed_local_attributes a b model) (List.last (atts_of_par)) atts_of_par
    end
	
fun inherited_associationends_of class (model as (clist,alist)) =
    let
	val _ = trace rep_core ("inh assoEnd 0\n")
	val c_parents = parents_of class model
	val _ = trace rep_core ("inh assoEnd 1: parents = " ^ (String.concat (List.map (fn a => string_of_path (name_of a)) (c_parents))) ^ "\n")
	val assE_of_par = (List.map (associationends_of alist) c_parents)
	val _ = trace rep_core ("inh assoEnd 2\n")
	val _ = trace rep_core ("inh assoEnd 3:  assocEnds of parents: " ^ (String.concat (List.map (fn a => (name_of_aend a)) (List.concat assE_of_par))) ^ "\n")
    in
	if (List.length(assE_of_par) = 0)
	then []
	else List.foldr (fn (a,b) => embed_local_assocEnds a b model) (List.last (assE_of_par)) assE_of_par
    end

(* get absolutelly all operations of a classifier. In case of a functions which occurs serveral times in the inheritance tree, the most specified function is listed. *)
fun all_operations_of class model =
    let 
	val lo = local_operations_of class
	val _ = trace 50 ("all ops of classifier : "^ (string_of_path (name_of class)) ^ "\n")
	val io = inherited_operations_of class model
	val _ = trace 50 ("all ops 2\n")
    in
	embed_local_operations lo io model
    end

fun all_attributes_of class model = 
    let
	val la = local_attributes_of class
	val _ = trace 50 ("all atts of classifier : "^ (string_of_path (name_of class)) ^ "\n")
	val ia = inherited_attributes_of class model
	val _ = trace 50 ("all atts 2\n")
    in
	embed_local_attributes la ia model
    end

fun all_associationends_of class (model as (clist,alist)) = 
    let
	val la = local_associationends_of alist class
	val _ = trace rep_core ("all assocEnds of classifier : " ^ (String.concat (name_of class)) ^ "\n")
	val _ = trace rep_core ("name of loacal assends: " ^ (String.concat (List.map (fn a => (name_of_aend a)) la)) ^ "\n")
	val ia = inherited_associationends_of class model
	val _ = trace rep_core ("name of inherited assends: " ^ (String.concat (List.map (fn a => (name_of_aend a)) ia)) ^ "\n")
	val _ = trace rep_core ("all assocEnds \n")
    in
	embed_local_assocEnds la ia model
    end

		 
(* get all local operations, which occurs in one of the parent classes at least each time also *)
fun modified_operations_of class model = 
    let
	val io = inherited_operations_of class model
	val lo = local_operations_of class
	fun op_exists (oper:operation) [] = false
	  | op_exists (oper:operation) ((h:operation)::tail) = if (oper=h) 
							       then true
							       else op_exists oper tail
    in
	optlist2list (List.map (fn a => if (op_exists a io)
					then NONE
					else (if  (List.exists (fn b => same_op a b model) io)
					      then SOME(a)
					      else NONE
			       )) lo) 
    (* List.concat (List.map (fn a => List.filter (fn b => if (same_op a b model) then false else true) io) lo ) *)
    end

fun modified_attributes_of class model = 
    let
	val ia = inherited_attributes_of class model
	val la = local_attributes_of class
	fun att_exists (att:attribute) [] = false
	  | att_exists (att:attribute) ((h:attribute)::tail) = if (att = h)
								then true
								else att_exists att tail
    in
	optlist2list (List.map (fn a => if (att_exists a ia)
					then NONE
					else (if (List.exists (fn b => same_att a b model) ia)
					      then SOME(a)
					      else NONE
			       )) la)
    end


fun creation_operations_of class (model:transform_model) = 
    let
	val oper = all_operations_of class model
	val creators = List.filter (fn a => List.exists (fn b => b = "create") (#stereotypes a)) (oper)
    in
	creators
    end   

fun destruction_operations_of class (model:transform_model) = 
    let
	val oper = all_operations_of class model
	val creators = List.filter (fn a => List.exists (fn b => b = "destroy") (#stereotypes a)) (oper)
    in
	creators
    end   

fun public_operations_of class (model:transform_model) = 
    let
	val ops = all_operations_of class model
    in
	List.filter (fn a => (#visibility a) = public) ops
    end

fun private_operations_of class (model:transform_model) = 
    let
	val ops = all_operations_of class model
    in
	List.filter (fn a => (#visibility a) = private) ops
    end

fun package_operations_of class (model:transform_model) = 
    let
	val ops = all_operations_of class model
    in
	List.filter (fn a => (#visibility a) = package) ops
    end

fun protected_operations_of class (model:transform_model) = 
    let
	val ops = all_operations_of class model
    in
	List.filter (fn a => (#visibility a) = protected) ops
    end

fun query_operations_of class (model:transform_model) =
    let
	val ops = all_operations_of class model
    in
	List.filter (fn a => (#isQuery a)) ops
    end

fun  command_operations_of class (model:transform_model) = 
     let
	 val ops = all_operations_of class model
     in
	 List.filter (fn a => not (#isQuery a)) ops
     end


(* convert an association end into the corresponding collection type *)
fun convert_aend_type ({name,aend_type,multiplicity,
                        ordered,visibility,init}:associationend) =
    (case multiplicity of 
       [(0,1)] => aend_type
     | [(1,1)] => aend_type
     | _ =>if ordered then Rep_OclType.Sequence aend_type (* OrderedSet? *)
           else Rep_OclType.Set aend_type)
    

fun convert_aend (cls_name:string) (aend:associationend):attribute = 
    {name = List.last (#name aend),
     attr_type = convert_aend_type aend,
     visibility = #visibility aend,
     scope = XMI.InstanceScope,
     stereotypes = nil,
     init = #init aend}



(* convert a multiplicity range into an invariant of the form *)
(* size > lowerBound and size < upperBound )                 *)
fun range_to_inv cls_name aend (a,b) = 
    let val cls       = Rep_OclType.Classifier cls_name
      val attr_type = convert_aend_type aend
      val attr_name = cls_name@[List.last (#name aend)]
      val literal_a = Rep_OclTerm.Literal (Int.toString a, Rep_OclType.Integer)
      val literal_b = Rep_OclTerm.Literal (Int.toString b, Rep_OclType.Integer)
      val self      = Rep_OclTerm.Variable ("self",cls)
      val attribute = Rep_OclTerm.AttributeCall (self,cls,attr_name,attr_type)
      val attribute_size = 
	  Rep_OclTerm.OperationCall (attribute,attr_type,
				     ["oclLib","Collection","size"],[],
				     Rep_OclType.Integer)
      val lower_bound = 
	  Rep_OclTerm.OperationCall (attribute_size,Rep_OclType.Integer,
				     ["oclLib","Real",">="],
				     [(literal_a,Rep_OclType.Integer)],
                                     Rep_OclType.Boolean)
      val upper_bound = 
	  Rep_OclTerm.OperationCall (attribute_size,Rep_OclType.Integer,
				     ["oclLib","Real","<="],
				     [(literal_b,Rep_OclType.Integer)],
                                     Rep_OclType.Boolean)
      val equal = 
	  Rep_OclTerm.OperationCall (attribute_size,Rep_OclType.Integer,
				     ["oclLib","OclAny","="],
				     [(literal_a,Rep_OclType.Integer)],
                                     Rep_OclType.Boolean)
    in
      if a = b then equal
      else if b = ~1 then lower_bound
      else Rep_OclTerm.OperationCall (lower_bound,Rep_OclType.Boolean,
				      ["oclLib","Boolean","and"],
				      [(upper_bound,Rep_OclType.Boolean)],
				      Rep_OclType.Boolean)
    end
    
fun path_of_aend ({name,aend_type,...}:associationend) = name 

fun substitute_templ_para (Collection(tt)) t = Collection (t)
  | substitute_templ_para (Set (tt)) t = Set (t)
  | substitute_templ_para (OrderedSet (tt)) t = OrderedSet (t)
  | substitute_templ_para (Sequence (tt)) t = Sequence (t)
  | substitute_templ_para (Bag (tt)) t = Bag (t)
  | substitute_templ_para t1 t2 = raise TemplateError ("Not possible to replace template parameter of a basic type. Type is: " ^ string_of_OclType t1 ^ " \n")

fun type_of_template_parameter typ =
    case typ of
	Set(t) => t
      | Sequence(t) => t
      | Bag(t) => t
      | Collection(t) => t
      | OrderedSet(t) => t
      | t => raise NoCollectionTypeError t

fun consistency_constraint cls_name (aend,revAend) = 
    let 
        fun aendIsSet (aend:associationend) = case #multiplicity aend
                                               of [(0,1)] => false
                                                | [(1,1)] => false
                                                | M       => true
	fun mkCollection (a:associationend) = if (#ordered a) 
			     then (Sequence (#aend_type a)) 
			     else (Set (#aend_type a))

	fun path_of_classifier (Classifier p) = p
        val cons_inv_name = ("consistencyconstraint_for_aend_"^
                             (short_name_of_path (#name aend)))
        val revPath = 
	    (path_of_classifier (#aend_type aend))@[List.last (#name revAend)]  
        val selfVar = Rep_OclHelper.self (Rep_OclType.Classifier cls_name)
        val attPath =  
	    (path_of_classifier (#aend_type revAend))@[List.last (#name aend)] 
        val targetType = if   aendIsSet aend 
                         then mkCollection aend 
                         else      #aend_type aend
        val targetVar = Rep_OclTerm.Variable ("x",#aend_type aend)
        val target = Rep_OclHelper.ocl_attcall selfVar attPath 
                                               targetType
        val revType = if   aendIsSet revAend 
                      then mkCollection revAend
                      else  (#aend_type revAend)
        val sources = Rep_OclHelper.ocl_attcall targetVar revPath
                                                       (revType)
        val back = Rep_OclHelper.ocl_attcall target revPath revType 
	val oclTrue = Rep_OclTerm.Literal ("true",Rep_OclType.Boolean)

        val body = 
	    case (aendIsSet aend, aendIsSet revAend)
             of (false,false) =>  Rep_OclHelper.ocl_eq  back selfVar 
              | (false,true)  =>  Rep_OclHelper.ocl_includes back selfVar 
              | (true,false)  =>  Rep_OclHelper.ocl_forAll target [targetVar]
							  (Rep_OclHelper.ocl_eq sources selfVar) 
              | (true,true)   => Rep_OclHelper.ocl_forAll target [targetVar]
				(Rep_OclHelper.ocl_includes sources selfVar) 
    in
      (SOME cons_inv_name, body)
    end
                                      

fun multiplicity_constraint cls_name (aend:associationend) = 
    let 
        val mult_inv_name = ("multconstraint_for_aend_"^
                             (short_name_of_path (#name aend)))
        val range_constraints = 
            (case (#multiplicity aend) of
	         [(0,1)] => []
	       | [(1,1)] => let
	             val attr_name = cls_name@[List.last (#name aend)]
	             val attr_type = convert_aend_type aend
	             val cls       = Rep_OclType.Classifier cls_name
	             val self      = Rep_OclTerm.Variable ("self",cls)
	             val attribute = Rep_OclTerm.AttributeCall (self,cls,
                                                                attr_name,attr_type)
	         in
	             [Rep_OclTerm.OperationCall (attribute,attr_type,
					         ["oclIsDefined"],[],
					         Rep_OclType.Boolean)]
	         end
	       | _ =>  map (range_to_inv cls_name aend) 
		           (#multiplicity aend))
    in 
        if range_constraints = [] 
        then (SOME mult_inv_name, Rep_OclTerm.Literal ("true",
                                                       Rep_OclType.Boolean)) 
        else (SOME mult_inv_name, Rep_OclHelper.ocl_or_all range_constraints)
    end

(** calculate the invariants of an association end:
 * 1. multiplicity constraints
 * 2. consistency constraints between opposing association ends
 *    i.e., A.b.a->includes(A)                              
 * params {cls_name,(aend,revPath)}
 * @param cls_name Path of source classifier
 * @param aend aend to be converted
 * @param revAend the reverse navigation of aend   
 *)
fun aend_to_inv cls_name (aend:associationend,revAend:associationend) =
      [ consistency_constraint cls_name (aend,revAend), 
       multiplicity_constraint cls_name aend]
    

fun association_of_aend ({name,aend_type,...}:associationend) =
	  List.take(name, (List.length name)-1)

fun role_of_aend ({name,aend_type,...}:associationend) = List.last name
			     
fun bidirectionalPairs name allAssociations associationPaths =
    let
	fun combine [] [] = []
          | combine (xs::xss) ([y]::ys) =
            map (fn x => (x,y)) xs @ (combine xss ys)
          | combine (xs::xss) ((b::bs)::ys) =
            map (fn x => (x,b)) xs @
            map (fn x => (x,b)) bs @ 
            map (fn x => (b,x)) bs (* need symmetry in this case *) @
            (combine (xs::xss) (bs::ys))
	    
	    
	val otherAends = map (oppositeAendsOfAssociation name allAssociations)
                             associationPaths
	val selfAends = map(incomingAendsOfAssociation name allAssociations)
                           associationPaths
    in
	combine otherAends selfAends
    end
    
fun aendToAttCall (name,oclTerm) =
    let
	fun aendToAtt (Rep_OclTerm.AssociationEndCall(source,sourceType,path,
                                                      resultType)) =
            (Rep_OclTerm.AttributeCall(source,sourceType,path,resultType))
          | aendToAtt x = x
    in
	(name,Rep_OclHelper.mapOclCalls aendToAtt oclTerm)
    end
    
(** convert association ends into attributes + invariants 
 * Associations belonging to an association class have not been modified to
 * include an additional aend to the association class.
 *)
fun normalize (all_associations:association list) 
              (C as (Class {name,parent,attributes,operations,associations,
                            invariant,stereotypes,interfaces,thyname,
                            visibility,activity_graphs})):Classifier =
    let
	val _ = trace function_calls ("Rep_Core:normalize: class\n")
	val _ = trace function_arguments 
                      ("number of associations: "^(Int.toString(List.length 
                                                                    associations)
                                                  )^"\n")
	val _ = map (trace function_arguments o (fn x => 
                                                    "association path: "^x^"\n")
                     o string_of_path) associations
	fun mapPath (aend1,aend2) = (aend1,path_of_aend aend2)
				    
	(*     val aendPathPairs = map mapPath (bidirectionalPairs name all_associations
								   associations)*)
	val aendPathPairs = bidirectionalPairs name all_associations associations
	val _ = trace function_ends ("Rep_Core: end normalize \n")
    in
	Class {name   = name,
	       parent = parent,
	       attributes = append (map (convert_aend (List.last(path_of_OclType
                                                                     name))) 
					(associationends_of all_associations C))
                                   attributes,
	       operations = operations,
	       associations = nil,
	       invariant = append (List.concat 
                                       (map (aend_to_inv (path_of_OclType name))
                                            aendPathPairs))
				  (map aendToAttCall invariant),
	       stereotypes = stereotypes,
               interfaces = interfaces,
	       thyname = thyname,
	       visibility = visibility,
               activity_graphs = activity_graphs}
    end
  | normalize all_associations (AC as (AssociationClass 
                                           {name,parent,attributes,association,
                                            associations,operations,invariant,
                                            stereotypes,interfaces,
                                            thyname,visibility, activity_graphs})) =
    (* FIXME: how to handle AssociationClass.association? *)
    let
	val _ = trace function_calls ("Rep_Core.normalize  AssociationClass\n")
	val _ = trace function_arguments 
                      ("number of associations: "^
                       (Int.toString (List.length associations ))^"\n")
	fun mapPath (aend1,aend2) = (aend1,path_of_aend aend2)
                                    
	val aendPathPairs = (bidirectionalPairs name all_associations
                                                associations)
	val res = 
	    AssociationClass {
	    name   = name,
	    parent = parent,
	    attributes = append (map (convert_aend (List.last (path_of_OclType 
								   name)))
				     (associationends_of all_associations AC))
				attributes,
	    operations = operations,
	    invariant = append (List.concat(
				map (aend_to_inv (path_of_OclType name)) 
				    aendPathPairs))
			       (map aendToAttCall invariant),
	    stereotypes = stereotypes,
	    interfaces = interfaces,
	    thyname = thyname,
	    activity_graphs = activity_graphs,
	    associations = [],
	    visibility=visibility,
	    association = [] (* FIXME? *)}
	val _ = trace function_ends ("Rep_Core.normalize")
    in
	res
    end
  | normalize all_associations (Primitive p) =
    (* Primitive's do not have attributes, so we have to convert *)
    (* them into Classes...                                      *)
    if (#associations p) = [] then Primitive p 
    else normalize all_associations (Class { name = (#name p),
                                             parent = (#parent p),
                                             attributes=[],
                                             operations=(#operations p),
                                             invariant = (#invariant p),
					     associations = (#associations p),
					     stereotypes = (#stereotypes p),
					     interfaces = (#interfaces p),
					     thyname = (#thyname p),
					     visibility = public,
					     activity_graphs=[]})
  | normalize all_associations c = c
				   
				   
fun rm_init_attr (attr:attribute) = {
    name = #name attr,
    attr_type = #attr_type attr,
    visibility = #visibility attr,
    scope = #scope attr,
    stereotypes = #stereotypes attr,
    init = NONE
}:attribute


fun joinModel ((a_cl,a_assoc):transform_model)
	      ((b_cl,b_assoc):transform_model)  = 
    (a_cl@b_cl,a_assoc@b_assoc)
    
fun init_to_inv cls_name (attr:attribute) = 
    (case (#init attr) of
       NONE => (SOME ("init_"^(#name attr)),
	        Rep_OclTerm.Literal ("true",Rep_OclType.Boolean))
     | SOME(init) => let
	 val attr_name = cls_name@[#name attr]
	 val attr_type = #attr_type attr
	 val cls       = Rep_OclType.Classifier cls_name
	 val self      = Rep_OclTerm.Variable ("self",cls)
	 val attribute = Rep_OclTerm.AttributeCall (self,cls,attr_name,
                                                    attr_type)
       in	
         (SOME ("init_"^(#name attr)),
	  Rep_OclTerm.OperationCall
	      (Rep_OclTerm.OperationCall
		   (self,cls,
		    ["oclLib","OclAny","oclIsNew"],[],Rep_OclType.Boolean),
               Rep_OclType.Boolean,
	       ["oclLib","Boolean","implies"],
	       [(Rep_OclTerm.OperationCall (attribute,
			                    attr_type,["oclLib","OclAny","="],
			                    [(init,attr_type)],
                                            Rep_OclType.Boolean),
                 Rep_OclType.Boolean)],
	       Rep_OclType.Boolean)
	 )
       end)
                    

fun normalize_init (Class {name,parent,attributes,operations,
                           associations,invariant,
		           stereotypes,interfaces,thyname,visibility,activity_graphs}) =
    Class {name   = name,
	   parent = parent,
	   attributes = (map rm_init_attr attributes),
	   operations = operations,
	   associations = nil,
	   invariant = append (map (init_to_inv  (path_of_OclType name)) 
                                   attributes)  
			      invariant,
	   stereotypes = stereotypes,
           interfaces = interfaces,
	   thyname = thyname,
	   visibility=visibility,
           activity_graphs=activity_graphs}
  | normalize_init (AssociationClass {name,parent,attributes,operations,
                                      associations,association,
				      invariant,stereotypes,interfaces,
                                      thyname,visibility,activity_graphs}) =
    AssociationClass {name   = name,
		      parent = parent,
		      attributes = (map rm_init_attr attributes),
		      operations = operations,
		      associations = nil,
		      association = []:Path (* FIXME: better dummy? *),
		      invariant = append (map (init_to_inv  (path_of_OclType
                                                                 name)) 
                                              attributes)  invariant,
		      stereotypes = stereotypes,
                      interfaces = interfaces,
		      visibility=visibility,
		      thyname = thyname,
                      activity_graphs=activity_graphs}
  | normalize_init c = c
                       
fun normalize_ext ((classifiers,associations):transform_model) =
    (* no distinguishing for valid binary associations *)
    (map (normalize associations) classifiers, [])		   
	       
fun string_of_path (path:Rep_OclType.Path) = 
    (case path of
       [] => ""
     | p  => foldr1 (fn (a,b) => a^"."^b) p)
             
fun update_thyname tname (Class{name,parent,attributes,operations,invariant,
                                stereotypes,interfaces,associations,
                                visibility,activity_graphs,...}) =
    Class{name=name,
          parent=parent,
          attributes=attributes,
          operations=operations,
          associations=associations,
          invariant=invariant,
          stereotypes=stereotypes,
          interfaces=interfaces,
          thyname=(SOME tname),
	  visibility=visibility,
          activity_graphs=activity_graphs }
  | update_thyname tname (AssociationClass{name,parent,attributes,operations,
                                           invariant,stereotypes,interfaces,
                                           associations,association,
                                           visibility,activity_graphs,...}) =
    AssociationClass{name=name,
                     parent=parent,
                     attributes=attributes,
                     operations=operations,
		     associations=associations,
                     association=association,
                     invariant=invariant,
                     stereotypes=stereotypes,
		     interfaces=interfaces,
                     thyname=(SOME tname),
		     visibility=visibility,
                     activity_graphs=activity_graphs }
  | update_thyname tname (Interface{name,parents,operations,stereotypes,
                                    invariant,...})  =
    Interface{name=name,
              parents=parents,
              operations=operations,
              stereotypes=stereotypes,
              invariant=invariant,
              thyname=(SOME tname)} 
  | update_thyname tname (Enumeration{name,parent,operations,literals,
                                      invariant,stereotypes,interfaces,...}) =
    Enumeration{name=name,
                parent=parent,
                operations=operations,
                literals=literals,
                invariant=invariant,
                stereotypes=stereotypes,
                interfaces=interfaces,
                thyname=(SOME tname)}
  | update_thyname tname (Primitive{name,parent,operations,associations,
                                    invariant,stereotypes,interfaces,...})  =
    Primitive{name=name,
              parent=parent,
              operations=operations,
              associations=associations,
              invariant=invariant,
              stereotypes=stereotypes,
              interfaces=interfaces,
              thyname=(SOME tname)} 
  | update_thyname _ (Template T) = 
    error ("in update_thyname: Template does not have a theory")

fun update_invariant invariant' (Class{name,parent,attributes,operations,
                                       invariant,stereotypes,interfaces,
                                       associations,visibility,activity_graphs,thyname}) =
    Class{name=name,
          parent=parent,
          attributes=attributes,
          operations=operations,
          associations=associations,
          invariant=invariant',
          stereotypes=stereotypes,
          interfaces=interfaces,
          thyname=thyname,
	  visibility=visibility,
          activity_graphs=activity_graphs }
  | update_invariant invariant' (AssociationClass{name,parent,attributes,
                                                  operations,invariant,
                                                  stereotypes,interfaces,
                                                  association,associations,
                                                  visibility,activity_graphs,thyname}) =
    AssociationClass{name=name,
                     parent=parent,
                     attributes=attributes,
                     operations=operations,
		     associations=associations,
                     association=association,
                     invariant=invariant',
		     stereotypes=stereotypes,
                     interfaces=interfaces,
                     thyname=thyname,
		     visibility=visibility,
                     activity_graphs=activity_graphs }
  | update_invariant invariant' (Interface{name,parents,operations,stereotypes,
                                           invariant,thyname})  =
    Interface{name=name,
              parents=parents,
              operations=operations,
              stereotypes=stereotypes,
              invariant=invariant',
              thyname=thyname} 
  | update_invariant invariant' (Enumeration{name,parent,operations,literals,
                                             invariant,stereotypes,interfaces,
                                             thyname}) =
    Enumeration{name=name,
                parent=parent,
                operations=operations,
                literals=literals,
                invariant=invariant',
                stereotypes=stereotypes,
                interfaces=interfaces,
                thyname=thyname}
  | update_invariant invariant' (Primitive{name,parent,operations,associations,
                                           invariant,stereotypes,interfaces,
                                           thyname}) =
    Primitive{name=name,
              parent=parent,
              operations=operations,
              associations=associations,
              invariant=invariant',
              stereotypes=stereotypes,
              interfaces=interfaces,
              thyname=thyname} 
  | update_invariant _ (Template T) = 
    error ("in update_invariant: Template does not have an invariant")
                                      

fun update_operations operations' (Class{name,parent,attributes,invariant,
                                         operations,stereotypes,interfaces,
                                         associations,activity_graphs,
                                         visibility,thyname}) =
    Class{name=name,
          parent=parent,
          attributes=attributes,
          invariant=invariant,
          associations=associations,
          operations=operations',
          stereotypes=stereotypes,
          interfaces=interfaces,
	        visibility=visibility,
          thyname=thyname,
          activity_graphs=activity_graphs }
  | update_operations operations' (AssociationClass{name,parent,attributes,
                                                    invariant,operations,
                                                    stereotypes,interfaces,
                                                    associations,association,
						                                        visibility,
                                                    activity_graphs,thyname}) =
    AssociationClass{name=name,
                     parent=parent,
                     attributes=attributes,
                     invariant=invariant,
		     associations=associations,
                     association=association,
                     operations=operations',
                     stereotypes=stereotypes,
		     interfaces=interfaces,
		     visibility=visibility,
                     thyname=thyname,
                     activity_graphs=activity_graphs }
  | update_operations operations' (Interface{name,parents,invariant,
                                             stereotypes,operations,thyname}) =
    Interface{name=name,
              parents=parents,
              invariant=invariant,
              stereotypes=stereotypes,
              operations=operations',
              thyname=thyname} 
  | update_operations operations' (Enumeration{name,parent,invariant,literals,
                                               operations,stereotypes,
                                               interfaces,thyname}) =
    Enumeration{name=name,
                parent=parent,
                invariant=invariant,
                literals=literals,
                operations=operations',
                stereotypes=stereotypes,
                interfaces=interfaces,
                thyname=thyname}
  | update_operations operations' (Primitive{name,parent,invariant,
                                             associations,operations,
                                             stereotypes,interfaces,thyname}) =
    Primitive{name=name,
              parent=parent,
              invariant=invariant,
              associations=associations,
              operations=operations',
              stereotypes=stereotypes,
              interfaces=interfaces,
              thyname=thyname} 
  | update_operations _ (Template T) = 
    error ("in update_operations: Template does not have operations")
     
      
fun update_precondition pre' ({name,precondition,postcondition,body,arguments,
                               result,isQuery,scope,stereotypes,visibility}:operation) =
    {name=name,
     precondition=pre',
     postcondition=postcondition,
     arguments=arguments,
     body=body,
     result=result,
     isQuery=isQuery,
     scope=scope,
     visibility=visibility,
     stereotypes=stereotypes}:operation

fun update_postcondition post' ({name,precondition,postcondition,body,
                                 arguments,result,isQuery,scope,
                                stereotypes, visibility}:operation) =
    {name=name,
     precondition=precondition,
     postcondition=post',
     arguments=arguments,
     body=body,
     result=result,
     isQuery=isQuery,
     scope=scope,
     visibility=visibility,
     stereotypes=stereotypes}:operation




fun visibility_of (Class{visibility,...})            = visibility
  | visibility_of (AssociationClass{visibility,...}) = visibility
  | visibility_of (Template{classifier,...})   = visibility_of classifier


fun short_name_of C =  case (name_of C)  of
	[] => error "in Rep.short_name_of: empty type"
	| p => (hd o rev)  p

fun stereotypes_of (Class{stereotypes,...})       = stereotypes  
  | stereotypes_of (AssociationClass{stereotypes,...}) = stereotypes
  | stereotypes_of (Interface{stereotypes,...})   = stereotypes
  | stereotypes_of (Enumeration{stereotypes,...}) = stereotypes
  | stereotypes_of (Primitive{stereotypes,...})    = stereotypes
  | stereotypes_of (Template _) = error "in Rep.stereotypes_of: \
                                        \unsupported argument type Template"



fun package_of (Class{name,...}) = 
    if (length (path_of_OclType name)) > 1 
    then take (((length (path_of_OclType name)) -1),
               (path_of_OclType name))  
    else []
  | package_of (AssociationClass{name,...}) = 
    if (length (path_of_OclType name)) > 1 
    then take (((length (path_of_OclType name)) -1),
	       (path_of_OclType name))  
    else []
  | package_of (Interface{name,...})   = 
    if (length (path_of_OclType name)) > 1 
    then take (((length (path_of_OclType name)) -1),
               (path_of_OclType name)) 
    else []
  | package_of (Enumeration{name,...}) = 
    if (length (path_of_OclType name)) > 1 
    then take (((length (path_of_OclType name)) -1),
               (path_of_OclType name))
    else []
  | package_of (Primitive{name,...})   = 
    if (length (path_of_OclType name)) > 1 
    then take (((length (path_of_OclType name)) -1),
               (path_of_OclType name)) 
    else []
  | package_of (Template{classifier,...}) = package_of classifier

fun classes_of_package pkg (model as (clist,alist)) = 
    List.filter (fn a => package_of a = pkg) clist
       


fun substitute_package [] tpackage [] = raise Rep_CoreError ("Not possible to substitute package since names belongs to package itself and not a class of it.\n")
  | substitute_package [] tpackage path = tpackage@path
  | substitute_package x tpackage [] = raise Rep_CoreError ("Not Possible to substitute Package since package longer than path.\n")
  | substitute_package (hf::fpackage) (tpackage) (hp::path) = 
    if (hf = hp) 
    then substitute_package fpackage tpackage path
    else (hp::path)

fun parent_short_name_of C =  
    (case (parent_name_of C) of
       [] => error "in Rep.parent_short_name_of: empty type"
     | p => (hd o rev)  p)
	    
fun parent_package_of (Class{parent,...})       = 
    (case parent of  NONE => package_of OclAnyC
		   | SOME q   => let val p = path_of_OclType q in 
				     if (length p) > 1 
                                     then  (take (((length p) -1),p))  
                                     else []
				 end)
  | parent_package_of (AssociationClass{parent,...})       = 
    (case parent of  NONE => package_of OclAnyC
		   | SOME q   => let val p = path_of_OclType q in 
				     if (length p) > 1 
                                     then  (take (((length p) -1),p))  
                                     else []
				 end)
  | parent_package_of (Interface{...})        = 
    error "in Rep.parent_package_of: unsupported argument type Interface"
  | parent_package_of (E as Enumeration{parent,...}) = 
    (case parent of  NONE => error ("in Rep.parent_package_of: Enumeration "^
                                    (string_of_path o name_of) E^
                                    " has no parent")
		   | SOME q   => let val p = path_of_OclType q in 
				    if (length p) > 1 
                                    then (take (((length p) -1),p))  
                                    else []
				end )
  | parent_package_of (Primitive{parent,...})    = 
    (case parent of NONE => package_of OclAnyC
	  (* NONE => error "Primitive has no parent" *)
		 |  SOME q   => let val p = path_of_OclType q in
				   if (length p) > 1 
                                   then (take (((length p) -1),p))  
                                   else []
			       end)
  | parent_package_of (Template{...})        = 
    error "in Rep.parent_package_of: unsupported argument type Template"
						

(* Get parent interfaces of a Classifier. *)
fun parent_interfaces_of (Interface{parents,...}) = parents
  | parent_interfaces_of (Class{interfaces,...}) = interfaces
  | parent_interfaces_of (AssociationClass{interfaces,...}) = interfaces
  | parent_interfaces_of (Enumeration{interfaces,...}) = interfaces
  | parent_interfaces_of (Primitive{interfaces,...}) = interfaces
  | parent_interfaces_of (Template{...}) = error "parent_interfaces_of <Template> not supported"




(* Get the names of parent interfaces of a Classifier *)
fun parent_interface_names_of c = map path_of_OclType (parent_interfaces_of c)


fun p_invariant_of (Class{invariant,...})       = invariant 
  | p_invariant_of (AssociationClass{invariant,...}) = invariant
  | p_invariant_of (Interface{invariant,...})   = invariant
  | p_invariant_of (Enumeration{invariant,...}) = invariant
  | p_invariant_of (Primitive{invariant,...})   = invariant
  | p_invariant_of (Template _) = error "in Rep.p_invariant_of: \
                                        \unsupported argument type Template"

fun invariant_of C = 
    (case p_invariant_of C of  
       [] => [(NONE, Rep_OclTerm.Literal ("true",Rep_OclType.Boolean))]
     | il => il)


fun precondition_of_op ({precondition,...}:operation) = 
    (case precondition  of  
       [] => [(NONE, Rep_OclTerm.Literal ("true",Rep_OclType.Boolean))]
     | il => il)

fun body_of_op ({body,...}:operation) = body
                                        
fun postcondition_of_op ({postcondition, ...}:operation) = 
    (case postcondition  of  
       [] => [(NONE, Rep_OclTerm.Literal ("true",Rep_OclType.Boolean))]
     | il => il)


              
fun name_of_ae ({name,...}:associationend) = name



fun thy_name_of (C as Class{thyname,...}) = 
     (case thyname of  SOME tname =>  tname
		     | NONE => error  ("Class "^((string_of_path o name_of) C)^
                                       " has no thyname"))
  |  thy_name_of (AC as AssociationClass{thyname,...}) = 
     (case thyname of  SOME tname =>  tname
		     | NONE => error  ("AssociationClass "^((string_of_path o 
                                                             name_of) AC)^
                                       " has no thyname"))
  | thy_name_of (I as Interface{thyname,...})   = 
     (case thyname of SOME tname =>  tname
		    | NONE => error  ("Interface "^((string_of_path o 
                                                     name_of) I)
                                      ^" has no thyname"))
  | thy_name_of (E as Enumeration{thyname,...}) = 
    (case thyname of SOME tname =>  tname
		   | NONE => error  ("Enumeration "^((string_of_path o 
                                                      name_of) E)
                                     ^" has no thyname"))
  | thy_name_of (P as Primitive{thyname,...})    = 
    (case thyname of SOME tname =>  tname
		   | NONE => error  ("Primitive "^((string_of_path o 
                                                    name_of) P)^
                                     " has no thyname"))
  | thy_name_of (Template _) = error "in Rep.thy_name_of: \
                                     \unsupported argument type Template"


                       

        
    
(* returns the activity graphs (list) of the given Classifier --> this is a list of StateMachines*)
(* Classifier -> ActivityGraph list *)
fun activity_graphs_of (Class{activity_graphs,...}) = activity_graphs
  | activity_graphs_of _                            = []
				                      
fun is_visible_cl (Class {visibility,...}) =
    if (visibility = public) then true else false
  | is_visible_cl (AssociationClass {visibility,...}) =
    if (visibility = public) then true else false
  | is_visible_cl x = true


fun is_visible_op ({visibility,...}:operation) =
    if (visibility = public) then true else false

fun is_visible_attr ({visibility,...}:attribute) = 
    if (visibility = public) then true else false

(* topological sort of class lists *)
fun topsort_cl cl =
    let 
      val OclAny_subcl = filter (fn a => (parent_name_of a) = 
                                         (name_of OclAnyC)) cl	      
      fun subclasses_of cl c = filter (fn a => (parent_name_of a=(name_of c))) 
                                      cl
      fun sub [] _ = []
	| sub cl c =  c :: (foldl (op@) [] (map (fn a => sub cl a) 
						(subclasses_of cl c)))  
    in
      foldl (op@) [] (map (fn a => sub cl a) (OclAny_subcl))  
    end
                                              
(** adds an invariant to a classifier.
 *)
fun addInvariant inv (Class {name, parent, attributes, operations, 
                             associations, invariant, stereotypes, 
                             interfaces, thyname, visibility,
			     activity_graphs}) =
    Class {name=name, parent=parent, attributes=attributes, 
           operations=operations, 
           associations=associations, invariant=inv::invariant, 
           stereotypes=stereotypes, interfaces=interfaces, 
           thyname=thyname, visibility=visibility,activity_graphs=activity_graphs}
  | addInvariant inv (AssociationClass {name, parent, attributes, 
					operations, associations,
					association, invariant,
					stereotypes, interfaces, 
					thyname, visibility,activity_graphs}) =
    AssociationClass {name=name, parent=parent, attributes=attributes, 
		      operations=operations, associations=associations,
		      association=association, invariant=inv::invariant, 
		      stereotypes=stereotypes, interfaces=interfaces, 
		      thyname=thyname, visibility=visibility,activity_graphs=activity_graphs}
  | addInvariant inv (Interface {name, parents, operations,  
                                 invariant, stereotypes,  thyname}) =
    Interface {name=name, parents=parents, operations=operations,
               invariant=inv::invariant, stereotypes=stereotypes, 
               thyname=thyname}
  | addInvariant inv (Enumeration {name, parent, operations,
                                   literals, invariant, stereotypes,
                                   interfaces, thyname}) =
    Enumeration{name=name, parent=parent, operations=operations,
                literals=literals,invariant=inv::invariant,
                stereotypes=stereotypes,
                interfaces=interfaces, thyname=thyname}
  | addInvariant inv (Primitive {name, parent, operations, 
                                 associations, invariant, 
                                 stereotypes, interfaces, thyname}) =
    Primitive{name=name, parent=parent, operations=operations, 
              associations=associations, invariant=inv::invariant, 
              stereotypes=stereotypes, interfaces=interfaces, 
              thyname=thyname}
  | addInvariant inv (Template {parameter, classifier})  =
    Template {parameter=parameter, 
              classifier=addInvariant inv classifier}

fun addInvariants invs (Class {name, parent, attributes, operations, 
                               associations, invariant, stereotypes, 
                               interfaces, thyname, visibility,activity_graphs}) =
    Class {name=name, 
           parent=parent, 
           attributes=attributes, 
           operations=operations, 
           associations=associations, 
           invariant=invs@invariant, 
           stereotypes=stereotypes, 
	   visibility=visibility,
           interfaces=interfaces, 
           thyname=thyname, 
           activity_graphs=activity_graphs}
  | addInvariants invs (AssociationClass {name, parent, attributes, 
					  operations, associations,
					  association, invariant,
					  stereotypes, interfaces, 
					  thyname, visibility, activity_graphs}) =
    AssociationClass {name=name, 
                      parent=parent, 
                      attributes=attributes, 
		      operations=operations, 
                      associations=associations,
		      association=association, 
                      invariant=invs@invariant, 
		      stereotypes=stereotypes, 
		      visibility=visibility,
                      interfaces=interfaces, 
		      thyname=thyname, 
                      activity_graphs=activity_graphs}
  | addInvariants invs (Interface {name, parents, operations,  
                                   invariant, stereotypes,  thyname}) =
    Interface {name=name, 
               parents=parents, 
               operations=operations,
               invariant=invs@invariant, 
               stereotypes=stereotypes, 
               thyname=thyname}
  | addInvariants invs (Enumeration {name, parent, operations,
                                     literals, invariant, stereotypes,
                                     interfaces, thyname}) =
    Enumeration{name=name, 
                parent=parent, 
                operations=operations,
                literals=literals,
                invariant=invs@invariant,
                stereotypes=stereotypes,
                interfaces=interfaces, 
                thyname=thyname}
  | addInvariants invs (Primitive {name, parent, operations, 
                                   associations, invariant, 
                                   stereotypes, interfaces, thyname}) =
    Primitive{name=name, 
              parent=parent, 
              operations=operations, 
              associations=associations, 
              invariant=invs@invariant, 
              stereotypes=stereotypes, 
              interfaces=interfaces, 
              thyname=thyname}
  | addInvariants invs (Template {parameter, classifier})  =
    Template {parameter=parameter, 
              classifier=addInvariants invs classifier}              
             
(** adds an operation to a classifier. *)
fun addOperation oper (Class {name, parent, attributes, operations, 
                              associations, invariant, stereotypes, 
                              interfaces, thyname, visibility,activity_graphs})
  = Class {name=name, parent=parent, attributes=attributes, 
           operations=oper::operations, 
           associations=associations, invariant=invariant, 
           stereotypes=stereotypes, interfaces=interfaces, 
	   visibility=visibility,
           thyname=thyname, activity_graphs=activity_graphs}
  | addOperation oper (AssociationClass {name, parent, attributes, 
					 operations, associations, 
					 association, invariant,
					 stereotypes, interfaces,
					 thyname, visibility,activity_graphs})
    = AssociationClass {name=name, parent=parent, attributes=attributes, 
		        operations=oper::operations, associations=associations,
		        association=association, invariant=invariant, 
		        stereotypes=stereotypes, interfaces=interfaces,
			visibility=visibility,
		        thyname=thyname, activity_graphs=activity_graphs}
  | addOperation  oper (Interface {name, parents, operations,  
                                   invariant, stereotypes,  thyname})
    = Interface {name=name, parents=parents, operations=oper::operations,
                 invariant=invariant, stereotypes=stereotypes, thyname=thyname}
  | addOperation oper (Enumeration {name, parent, operations,
                                    literals, invariant, stereotypes,
                                    interfaces, thyname})
    = Enumeration{name=name, parent=parent, operations=oper::operations, 
                  literals=literals, invariant=invariant, 
                  stereotypes=stereotypes,
                  interfaces=interfaces, thyname=thyname}
  | addOperation oper (Primitive {name, parent, operations, 
                                  associations, invariant, 
                                  stereotypes, interfaces, thyname})
    = Primitive{name=name, parent=parent, operations=oper::operations, 
                associations=associations, invariant=invariant, 
                stereotypes=stereotypes, interfaces=interfaces, 
                thyname=thyname}
  | addOperation oper (Template {parameter, classifier}) 
    = Template { parameter=parameter, 
                 classifier=addOperation oper classifier}



(** type operations **)
fun flatten_type (Set(Set(t))) = Set(t)
  | flatten_type (Set(Collection(t))) = Set(t)
  | flatten_type (Set(Bag(t))) = Set(t)
  | flatten_type (Set(OrderedSet(t))) = Set(t)
  | flatten_type (Set(Sequence(t))) = Set(t)

  | flatten_type (Collection(Set(t))) = Collection(t)
  | flatten_type (Collection(Bag(t))) = Collection(t)
  | flatten_type (Collection(OrderedSet(t))) = Collection(t)
  | flatten_type (Collection(Sequence(t))) = Collection(t)
  | flatten_type (Collection(Collection(t))) = Collection(t)

  | flatten_type (OrderedSet(Collection(t))) = OrderedSet(t)
  | flatten_type (OrderedSet(Bag(t))) = OrderedSet(t)
  | flatten_type (OrderedSet(OrderedSet(t))) = OrderedSet(t)
  | flatten_type (OrderedSet(Sequence(t))) = OrderedSet(t)
  | flatten_type (OrderedSet(Set(t))) = OrderedSet(t)

  | flatten_type (Bag(Collection(t))) = Bag(t)
  | flatten_type (Bag(Bag(t))) = Bag(t)
  | flatten_type (Bag(OrderedSet(t))) = Bag(t)
  | flatten_type (Bag(Sequence(t))) = Bag(t)
  | flatten_type (Bag(Set(t))) = Bag(t)

  | flatten_type (Sequence(Collection(t))) = Sequence(t) 
  | flatten_type (Sequence(Bag(t))) = Sequence(t)
  | flatten_type (Sequence(OrderedSet(t))) = Sequence(t)
  | flatten_type (Sequence(Sequence(t))) = Sequence(t)
  | flatten_type (Sequence(Set(t))) = Sequence(t)

  | flatten_type typ = typ


fun type_of_CollPart (CollectionItem (term,typ)) = typ
  | type_of_CollPart (CollectionRange (term1,term2,typ)) = typ

fun find_operation op_name list = List.find (fn a => (name_of_op a = op_name)) list

fun get_operation op_name class model = 
    let
	val ops = all_operations_of class model
    in
	valOf (find_operation op_name ops)
    end

fun get_attribute att_name (list:attribute list) = List.find (fn a => (#name a) = att_name) list

fun is_prefix x [] = false
  | is_prefix [h1] [h2] =
    if h1 = h2 
    then true
    else false
  | is_prefix (h1::tail1) (h2::tail2) = 
    if (h1 = h2)
    then is_prefix tail1 tail2
    else false

fun prefix_path prefix [] = prefix
  | prefix_path prefix path = 
    if (is_prefix prefix path)
    then path
    else prefix@path

fun prefix_type [] typ = typ
  | prefix_type h (Classifier (path)) = Classifier (h@[List.last path])
  | prefix_type h (Set (t)) = Set (prefix_type h t)
  | prefix_type h (Collection (t)) = Collection (prefix_type h t)
  | prefix_type h (OrderedSet (t)) = OrderedSet (prefix_type h t)
  | prefix_type h (Sequence (t)) = Sequence (prefix_type h t)
  | prefix_type h (Bag (t)) = Bag (prefix_type h t)
  | prefix_type h  basic_type = basic_type

fun prefix_signature ext_path [] = []
  | prefix_signature ext_path ((s,typ)::tail) = 
    (s,prefix_type ext_path typ)::(prefix_signature ext_path tail)

fun prefix_collectionpart ext_path (CollectionItem (term,typ)) = 
    (CollectionItem (prefix_expression ext_path term,typ))
  | prefix_collectionpart ext_path (CollectionRange (first_term,last_term,typ)) =
    (CollectionRange (prefix_expression ext_path first_term,prefix_expression ext_path last_term,typ))


(* RETURN: OclTerm *)
and prefix_expression ext_path (Variable (s,t)) = (Variable (s,t))
  | prefix_expression ext_path (Literal(s,t)) = Literal (s,t)
  | prefix_expression ext_path (AttributeCall (sterm,stype,path,res_typ)) =
    (AttributeCall (prefix_expression ext_path sterm,stype,path,res_typ))
  | prefix_expression ext_path (OperationCall (sterm,stype,path,args,res_typ)) =
    (OperationCall (prefix_expression ext_path sterm,stype,path,args,res_typ))
  | prefix_expression ext_path (Iterator (name,iter_vars,sterm,styp,expr,expr_typ,res_typ)) =
    let
	val prefixed_vars = List.map (fn a => (#1 a,prefix_type ext_path (#2 a))) iter_vars
    in
	Iterator (name,prefixed_vars,prefix_expression ext_path sterm,styp,prefix_expression ext_path expr,expr_typ,res_typ)
    end
  | prefix_expression ext_path (Let (var_name,var_type,rhs_term,rhs_type,expr,expr_type)) =
    (Let (var_name,prefix_type ext_path var_type,rhs_term,rhs_type,prefix_expression ext_path expr,expr_type))
  | prefix_expression ext_path (If (cond,cond_type,then_e,then_type,else_e,else_type,res_type)) = 
    (If (prefix_expression ext_path cond,cond_type,prefix_expression ext_path then_e,then_type,prefix_expression ext_path else_e,else_type,res_type))
  | prefix_expression ext_path (OperationWithType (sterm,stype,para_name,para_type,res_type)) = 
    (OperationWithType (prefix_expression ext_path sterm,stype,para_name,para_type,res_type)) 
  | prefix_expression ext_path (CollectionLiteral (coll_part_list,typ)) =
    (CollectionLiteral (List.map (prefix_collectionpart ext_path) coll_part_list,typ))
  | prefix_expression ext_path (Iterate (iter_vars,acc_var_name,acc_var_type,acc_var_term,sterm,stype,bterm,btype,restype)) = 
    let
	val prefixed_vars = List.map (fn a => (#1 a,prefix_type ext_path (#2 a))) iter_vars
	val prefix_acc_type = prefix_type ext_path acc_var_type 
    in
	(Iterate (prefixed_vars,acc_var_name,prefix_acc_type,acc_var_term,sterm,stype,bterm,btype,restype))
    end






fun type_of_template (T as Template{classifier,parameter}) =
    (case (name_of classifier) of
	["Collection(T)"] => Collection (parameter)
      | ["Set(T)"] => Set (parameter)
      | ["OrderedSet(T)"] => OrderedSet (parameter)
      | ["Bag(T)"] => Bag (parameter)
      | ["Sequence(T)"] => Sequence (parameter)
    )
  | type_of_template x = raise TemplateError ("type_of_template: Only Template Classifiers can be used.\n")






(* RETURN: classifier *) 



(* RETURN: Classifier *)

 
(* RETURN: Classifier /* one-time classifier */ *)
(*         Return the "one-time" classifier.
           The classifier is instanciated form the corresponding
           template form the library. Its only instanciate for one
           use and not stored later in the library.
*) 

(* element in lib not a template *)

fun get_op cl op_name (model:transform_model) = 
    let 
      val ops = all_operations_of cl model
    in
	List.find (fn a => op_name = name_of_op a) ops

    end

fun connected_classifiers_of (all_associations:association list) 
                             (C as Class {attributes,associations,...}) 
                             (cl:Classifier list) =
    let 
      val att_classifiers = List.mapPartial 
                                (fn (Classifier p) => SOME (class_of p (cl,[]))
                                  | _              => NONE)
                                (map  #attr_type attributes)
      val aend_classifiers = List.mapPartial 
                                 (fn (Classifier p) => SOME (class_of p (cl,[]))
                                   | _              => NONE)
                                 (map #aend_type (associationends_of 
                                                      all_associations C))
    in
      att_classifiers @ aend_classifiers 
    end
  | connected_classifiers_of all_associations (AC as AssociationClass
                                                  {attributes,associations,
                                                   association,...}) 
                             (cl:Classifier list) =
    let 
      val att_classifiers = List.mapPartial 
                                (fn (Classifier p) => SOME (class_of p (cl,[]))
                                  | _              => NONE)
                                (map  #attr_type attributes)
      (* FIXME: correct handling for association classes? *)
      val aend_classifiers = List.mapPartial 
                                 (fn (Classifier p) => SOME (class_of p (cl,[]))
                                   | _              => NONE)
                                 (map #aend_type (associationends_of 
                                                      all_associations AC))
    in
      att_classifiers @ aend_classifiers 
    end
  | connected_classifiers_of all_associations(P as Primitive{associations,...})
                             (cl:Classifier list) = 
    List.mapPartial (fn (Classifier p) => SOME (class_of p (cl,[]))
                      | _              => NONE)
                    (map #aend_type (associationends_of all_associations P))
  | connected_classifiers_of _  _ _ = nil
(* HERE *)



fun upcastable_args [] [] model = true
  | upcastable_args ((str,typ)::tail) ((term,ttyp)::args) model =
    let
	val _ = trace low ("must conform to: " ^ (string_of_OclType typ) ^ "\n")
    in
	if (conforms_to (type_of_term term) typ model) then 
	    true
	else
	    false
    end
  (* not same nuber of arguments *)
  | upcastable_args [x] list model = false
  | upcastable_args list [x] model = false

(* RETURN: (OclTerm * OclType) list *)
fun upcast_args [] [] model = []
  | upcast_args ((str,typ)::tail) ((term,_)::args) model  =
    let
	val _ = trace low ("interfere args" ^ "\n")
    in
	if (type_equals typ (type_of_term term)) then
	    (term,type_of_term term)::(upcast_args tail args model)
	else
	    if (conforms_to (type_of_term term) typ model) then
		(term,typ)::(upcast_args tail args model)
	    else
		raise UpcastingError ("Arguments are not interferebable \n") 
    end

(* RETURN: OclType *)
fun upcast_type t1 t2 model = 
    if (conforms_to t1 t2 model)
    then t2
    else raise UpcastingError ("Result type  does not conform \n") 


(* RETURN: OclTerm *)
fun upcast_op [] source args model = 
    let
	val _ = trace development ("UpcastingError ... \n")
    in
	raise UpcastingError ("interefere_methods: No operation signature matches given types.")
    end
  | upcast_op ((class,meth)::class_meth_list) source args model =
    let
	val _ = trace low ("\nInterfere method      : name : '" ^ name_of_op meth ^ "'\n")
       val check_source = conforms_to (type_of_term source) (type_of class) model
       val check_args = upcastable_args (#arguments meth) args model
       val _ = trace low ("Upcastable ?       : Source conforms : "  ^ Bool.toString check_source ^ "   Args conforms : " ^ Bool.toString check_args ^ "\n")
       val _ = trace low ("Return type of method : " ^ string_of_OclType (result_of_op meth) ^ "\n\n")
    in
	if (check_source andalso check_args) then
	    (* signature matches given types *)
	    (OperationCall(source,type_of class,(name_of class)@[name_of_op meth],upcast_args (#arguments meth) args model,result_of_op meth))	    
	else
	    (upcast_op class_meth_list source args model)
    end

(* RETURN: (OclTerm) *)
fun upcast_att (class,attr:attribute) source (model:transform_model) =
   let
       val check_source = conforms_to (type_of_term source) (type_of class) model
       val _ = trace low ("interfere attribute: check_source "^ Bool.toString check_source ^ "\n\n")
   in
       if check_source then
	   (* signature matches given types *)
	   SOME ((AttributeCall (source,type_of class,(name_of class)@[(#name attr)],(#attr_type attr))))
       else
	   NONE
   end


fun upcast_aend (class,assocend:associationend) source (model:transform_model) = 
    let 
	val check_source = conforms_to (type_of_term source) (type_of class) model 
	val _ = trace low ("Interfere assocend: check_source " ^ Bool.toString check_source ^ "\n")
	val _ = trace low ("type of assoc " ^ string_of_OclType (convert_aend_type assocend) ^ "\n")
    in
	if check_source then
	    (* billk_tag *)
	    (* associationend has changed *)
	    (*SOME ((AssociationEndCall (source,type_of class,(name_of class)@[(#name assocend)],convert_aend_type assocend))) *)
	    SOME ((AssociationEndCall (source,type_of class,(name_of class)@[List.last (#name assocend)],convert_aend_type assocend)))
	else
	    NONE
    end

(* RETURN: OclTerm *) 
fun upcast_att_aend [] source (model:transform_model) = 
    raise UpcastingError ("interference_attr_or_assoc: No operation signature matches given types.")
  | upcast_att_aend ((class,SOME(attr:attribute),NONE)::class_attr_or_assoc_list) source model =
    (
     case (upcast_att (class,attr) source model) of
	 NONE => (upcast_att_aend class_attr_or_assoc_list source model)
       | SOME (term) => term
    )
  | upcast_att_aend ((class,NONE,SOME(assocend:associationend))::class_attr_or_assoc_list) source model = 
    (
     case (upcast_aend (class,assocend) source model) of 
	 NONE => (upcast_att_aend class_attr_or_assoc_list source model)
       | SOME (term) => term
    )

(*
fun class_of_parent (Class{parent,...}:Classifier) (model:transform_model) =
    (case parent of
	 NONE => class_of_term (Variable ("x",OclAny)) model
       | SOME (others) => class_of_term (Variable ("x",others)) model 
    )
  | class_of_parent (AssociationClass {parent,...}) model = 
    (case parent of
	 NONE => class_of_term (Variable ("x",OclAny)) model
       | SOME (others) => class_of_term (Variable ("x",others)) model 
    )

  | class_of_parent (Primitive {parent,...}) model = 
    (case parent of
	 NONE => class_of_type OclAny model
       | SOME (others) => class_of_type  others model
    )
  | class_of_parent (Interface {parents,...}) model = 
*)
    (* TODO: change API *)
    (*  
     (case (List.last (parents)) of 
	  NONE => class_of_type OclAny clist
	| SOME (others) => class_of_type others clist
     )
     *)
(*    class_of_type (List.hd parents) model
  | class_of_parent c model = raise NoParentForDatatype "No Parent for this type of Classifier"
*)
fun end_of_recursion classifier = 
    case (type_of classifier) of
	Collection (T) => true
      | others => false

(* last_implentation_of_op *)

fun get_overloaded_methods class op_name model = 
    let
	val _ = trace rep_core ("get_overloaded_methods, look for operation = " ^ op_name ^ "\n")
	val parents = parents_of class model
	val loc_ops = List.map (fn a => (class,a)) (local_operations_of class)
	val cl_op_list = (loc_ops)@(List.concat (List.map (fn a => (List.map (fn b => (a,b)) (all_operations_of a model))) parents))
	val cls_ops = List.filter (fn (a,b) => if (name_of_op b = op_name) then true else false) cl_op_list
	val _ = trace rep_core ("number of overloaded operations found = " ^ Int.toString(List.length(cls_ops)) ^ "\n")
    in
	cls_ops
    end

fun last_implementation_of_op class op_name model = 
    List.hd (get_overloaded_methods class op_name model)
(*
fun get_overloaded_methods class op_name ([],_) = raise NoModelReferenced ("in 'get_overloaded_methods' ...\n")
  | get_overloaded_methods class op_name (model as (classifiers,associations)) =
   let
       val _ = trace function_calls "get_overloaded_methods\n"
       val _ = trace low("\n")
       val ops = local_operations_of class
       val _ = trace low("Look for methods for classifier: " ^ string_of_OclType (type_of class) ^ "\n")
       val ops2 = List.filter (fn a => (if ((#name a) = op_name) then true else false)) ops
       val _ = trace low("operation name                 : " ^ op_name ^ "  Found " ^ Int.toString (List.length ops2) ^ " method(s) \n")
       val parent = parent_of class model
       val _ = trace low("Parent class                   : " ^ string_of_OclType (type_of parent) ^ "\n\n")
       val cl_op = List.map (fn a => (class,a)) ops2
   in
       if (class = class_of_type OclAny model) 
       then (* end of hierarchie *)
            if (List.length ops2 = 0) 
	    then[]
	    else[(class,List.hd(ops2))]
       else 
	   ( 
	    if (end_of_recursion class)  
	    then (* end of collection hierarchie *)
		if (List.length ops2 = 0) 
		then []
		else [(class,List.hd(ops2))]
	    else (* go up the hierarchie tree *)
		(
		 if (List.length ops2 = 0) 
		 then (get_overloaded_methods parent op_name model)
		 else (cl_op)@(get_overloaded_methods parent op_name model)
		)
	   )
   end
*)

fun get_overloaded_attrs_or_assocends class attr_name (model as (clist,alist)) = 
    let
	val _ = trace function_calls ("Rep_Core.get_overloaded_attrs_or_assocends, look for attr_or_assoc = " ^ attr_name ^ "\n")
	val parents = parents_of class model
        (* Attributes *)
	val loc_atts = List.map (fn a => (class,a)) (local_attributes_of class)
	val cl_att_list = (loc_atts)@(List.concat (List.map (fn a => (List.map (fn b => (a,b)) (all_attributes_of a model))) parents))
	val cls_atts = 	List.filter (fn (a,b) => if (name_of_att b = attr_name) then true else false) cl_att_list
        (* Associations *)
	val _ = trace rep_core ("middle get_overloaded_attrs_or_assocends \n")
	val loc_assE = List.map (fn a => (class,a)) (local_associationends_of alist class)   
	val cl_assE_list = (loc_assE)@(List.concat (List.map (fn a => (List.map (fn b => (a,b)) (all_associationends_of a model))) parents))
	val cls_assEs = List.filter (fn (a,b) => if (name_of_aend b = attr_name) then true else false) cl_assE_list
	val res = 	(** ATTENTION: undefined in standard if assocEnds and attributes are allowed for same naem **)
	    if (List.length(cls_atts) = 0) 
	    then (
		if (List.length(cls_assEs) = 0)
		then [] 
		else 
		    let
			val (cl,assE) = List.hd (cls_assEs)
		    in
			[(cl,NONE,SOME(assE))]
		    end
		)
	    else (
		if (List.length(cls_assEs) = 0)
		then
		    let 
			val (cl,att) = List.hd (cls_atts)
		    in
			[(cl,SOME(att),NONE)]
		    end
		else
		    raise AttributeAssocEndNameClash ("Attributes and AssocEnd in same inheritance tree are named equal.\n")
		)
	val  _ = trace function_ends ("Rep_Core.get_overloaded_attrs_or_assocends\n")
    in
	res
    end
	
fun get_meth source op_name args (model as (classifiers,associations))=
    (* object type *)
    let
	val _ = trace function_calls ("Rep_Core: get_meth: Type of Classifier : " ^ string_of_OclType (type_of_term source ) ^ "\n")
	val class = class_of_term source model
	val meth_list = get_overloaded_methods class op_name model
	val res = upcast_op meth_list source args model
	val _ = trace function_ends ("Rep_Core: overloaded methods found: " ^ Int.toString (List.length meth_list) ^ "\n")
    in
	res
    end

fun get_attr_or_assoc source attr_name (model as (classifiers,associations)) =
    let 
	val _ = trace function_calls ("Rep_Core.get_attr_or_assoc\n")
	val _ = trace rep_core ("GET ATTRIBUTES OR ASSOCENDS: source term.\n")
	val class = class_of_term source model
	val attr_or_assocend_list = get_overloaded_attrs_or_assocends class attr_name model
	val res = 
	    let 
		val x = upcast_att_aend attr_or_assocend_list source model
		val _ = trace rep_core ("Return type of attribute: " ^ string_of_OclType (type_of_term x) ^ "\n\n")
	    in
		x
	    end
	val _ = trace function_ends ("Rep_Core.end get_attr_or_assoc\n")
    in
	res
    end

fun package_of_template_parameter typ = 
    case (typ) of
	Set (t) => (package_of_template_parameter (type_of_template_parameter t)
		    handle NoCollectionTypeError t => package_of_template_parameter t)
      | OrderedSet (t) => (package_of_template_parameter (type_of_template_parameter t)
			   handle NoCollectionTypeError t => package_of_template_parameter t)
      | Collection (t) => (package_of_template_parameter (type_of_template_parameter t)
			   handle NoCollectionTypeError t => package_of_template_parameter t)
      | Sequence (t) => (package_of_template_parameter (type_of_template_parameter t)
			 handle NoCollectionTypeError t => package_of_template_parameter t)
      | Bag (t) => (package_of_template_parameter (type_of_template_parameter t)
		    handle NoCollectionTypeError t => package_of_template_parameter t)
      | Integer => [OclLibPackage]
      | String => [OclLibPackage]
      | Boolean => [OclLibPackage]
      | Real => [OclLibPackage]
      | DummyT => []
      | OclVoid => []
      | OclAny => []
      | Classifier (p) => 
	if (length p > 1) then 
	    List.take (p,(length p) -1) 
        else 
	    []

fun collection_type classifier = 
    case (type_of classifier) of
	Collection(T) => true
      | Set (T) => true
      | OrderedSet (T) => true
      | Sequence (T) => true
      | Bag (T) => true
      | x => false

fun create_set (selector,typ) = 
    case selector of
	"Set" => Set (typ)
      | "Sequence" => Sequence (typ)
      | "Collection" => Collection (typ)
      | "OrderedSet" => OrderedSet (typ)
      | "Bag" => Bag (typ)
      | _ => DummyT

fun correct_type_for_CollLiteral coll_typ (CollectionItem (term,typ)) = 
    if (typ = coll_typ) then 
	true
    else
	false
  | correct_type_for_CollLiteral coll_typ (CollectionRange (term1,term2,typ)) =
    if (typ = coll_typ) then
	true
    else
	false

fun local_invariants_of class = invariant_of class

fun inherited_invariants_of class (model:transform_model as (clist,alist)) = 
    let
	val parent = parent_of class model
    in
	if (type_of parent = OclAny) 
	then []
	else (local_invariants_of class)@(inherited_invariants_of parent model)
    end

fun all_invariants_of class model = 
    (local_invariants_of class)@(inherited_invariants_of class model)


fun string_to_type "Integer" = Integer
  | string_to_type "Boolean" = Boolean
  | string_to_type "Real" = Real
  | string_to_type "OclAny" = OclAny
  | string_to_type "DummyT" = DummyT
  | string_to_type "String" = String
  | string_to_type "OclVoid" = OclVoid
  | string_to_type complex_type =
    let
	fun string_to_cons "Set" typ = Set(typ)
	  | string_to_cons "Bag" typ = Bag(typ)
	  | string_to_cons "Collection" typ = Collection (typ)
	  | string_to_cons "OrderedSet" typ = OrderedSet (typ)
	  | string_to_cons "Sequence" typ = Sequence (typ)
	
	fun string_to_package string = 
	    if (List.exists (fn a => if (a = (#".")) then true else false) (String.explode string)) 
	    then 
		let
		    (* The Type has a package prefixed *)
		    val tokens = parse_string (#".") (String.explode string)
		    val package_part = String.implode (#1 tokens)
		    (* delete the "." *)
		    val tail = List.tl (#2 tokens)
		in
		    [package_part]@(string_to_package (String.implode (tail)))
		end		
	    else
		[string]
    in
	if (List.exists (fn a => if (a = (#"(")) then true else false) (String.explode complex_type)) 
	then
	    (* The Type is a collection type. *)
	    let
		val tokens = parse_string (#"(") (String.explode complex_type)
		val cons = (#1 tokens)
		(* delete first "(" and last ")" element *)
		val tail = List.tl (real_path (#2 tokens))
		val _ = trace important ("tail "^ (String.implode tail) ^ "\n")
	    in
		string_to_cons (String.implode cons) (string_to_type (String.implode tail))
	    end
	else
	    (
	     if (List.exists (fn a => if (a = (#".")) then true else false) (String.explode complex_type))
	     then
		 (* The Type has a package prefixed. *) 
		 Classifier (string_to_package complex_type)
	     else
		 (* The Type is just one Class, without Collection and without a package.*)
		 Classifier ([complex_type])
		)
    end

fun all_packages_of_model ([],alist) = []
  | all_packages_of_model (model as (h::clist,alist):transform_model) =
    remove_dup (package_of h)::(all_packages_of_model (clist,alist))
    


end
