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
 * RETURN transform_model                *
 *****************************************)
(**
 * TODO: Description 
 *)
val joinModel      : transform_model -> transform_model -> transform_model

(** 
 * TODO: Description 
 *)
val normalize_ext  : transform_model -> transform_model

(*****************************************
 * RETURN Classifier                     *
 *****************************************)
(**
 * Ocl Classifier OclAny.
 *)
val OclAnyC : Classifier

(**
 * TODO: Description
 *)
val normalize      : association list -> Classifier -> Classifier

(**
 * TODO: Description
 *)
val normalize_init : Classifier -> Classifier

(**
 * Returns the classifier of a given path.
 *)
val class_of            : Rep_OclType.Path -> Classifier list -> Classifier
(**
 * Returns the classifier of the parent of classifier.
 *) 
val parent_of           : Classifier -> Classifier list -> Classifier

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
 * RETURN OclType                        *
 *****************************************)

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
 * Returns the type of a given term.
 *)
val type_of_term  : Rep_OclTerm.OclTerm -> Rep_OclType.OclType

(**
 * returns the type of the classifier this association end belongs to.
 * @params {aend}
 * @param aend association end
 * @return type of the classifier at the association end
 *)
val type_of_aend   : associationend -> Rep_OclType.OclType

(** 
 * TODO: Description 
 *)
val aend_to_attr_type : associationend -> Rep_OclType.OclType
(**
 * Returns the type of a not yet instantiate template
 *)
val type_of_template : Classifier -> Rep_OclType.OclType

(**
 * Prefixes a type with a given package name.
 *)
val prefix_type             : string list -> Rep_OclType.OclType -> Rep_OclType.OclType

(**
 * Returns the type of a given CollectionPart.
 *)
val type_of_CollPart : Rep_OclTerm.CollectionPart -> Rep_OclType.OclType

(**
 * Converts a string to a OclType including Collections.
 *)
val string_to_type   : string list -> Rep_OclType.OclType

(**
 * Collections of Collections are flattened according to Ocl 2.0 Standard.
 *)
val flatten_type  : Rep_OclType.OclType -> Rep_OclType.OclType



(*****************************************
 * RETURN OclTerms/CollectionPart        *
 *****************************************)


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
 * RETURN operation                      *
 *****************************************)

(**
 * Find an operation in a list of operations.
 *)
val find_operation : string -> operation list -> operation

(** 
 * Find an attribute in a list of attributes.
 *)
val find_attribute : string -> attribute list -> attribute

(** OBSOLETE **)
val operations_of       : Classifier -> operation list

(**
 * Returns the preconditions of an operation.
 *)
val precondition_of_op  : operation -> (string option * Rep_OclTerm.OclTerm) list

(**
 * Returns the postconditions of an operation.
 *)
val postcondition_of_op : operation -> (string option * Rep_OclTerm.OclTerm) list

(**
 * Returns the invariants of an operation.
 *)
val invariant_of        : Classifier -> (string option * Rep_OclTerm.OclTerm) list

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
 * TODO: Description
 *)
val mangled_name_of_op  : operation -> string

(**
 * TODO: Description (OBSOLETE?)
 *)
val operation_of        : Classifier list -> Rep_OclType.Path -> operation option

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
 * RETURN signature                      *
 *****************************************)

(** 
 * Prefixes all types in the signature with a 
 * given string list.
 *)
val prefix_signature        : string list -> (string * Rep_OclType.OclType) list -> (string * Rep_OclType.OclType) list



(*****************************************
 * RETURN attribute                      *
 *****************************************)
val attributes_of     : Classifier -> attribute list

(*****************************************
 * RETURN associationend                 *
 *****************************************)

(**
 * TODO: Description
 *)
val associationends_of: association list -> Classifier -> associationend list 

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
 * RETURN association                    *
 *****************************************)


(*****************************************
 * RETURN Path/string                    *
 *****************************************)

(**
 * Returns the name of the classifier.
 *)
val name_of              : Classifier -> Rep_OclType.Path 

(** 
 * Returns the thy_name of a classifer.
 *)
val thy_name_of       : Classifier -> string

(** 
 * Returns one of the parents from the classifier.
 *)
val parent_name_of       : Classifier -> Rep_OclType.Path  

(**
 * Returns the name of the package.
 *)
val package_of           : Classifier -> Rep_OclType.Path 

(** 
 * Returns the name of the package from the
 * parent class.
 *)
val parent_package_of    : Classifier -> Rep_OclType.Path  

(**
 * Returns the name of the class without the first package part.
 *)
val real_path            : Rep_OclType.Path -> Rep_OclType.Path 

(** 
 * Returns the last part (last string in path) of 
 * the name of the classifier.
 *)
val short_name_of : Classifier -> string 

(**
 * Returns the last part (last string in path) of the name
 * of the parent of the classifier.
 *)
val short_parent_name_of : Classifier -> string 

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

(** 
 * Returns paths of the associations of a classifier.
 *)
val associations_of   : Classifier -> Rep_OclType.Path list 

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
 * Get the stereotypes of a classifier.
 *)
val stereotypes_of      : Classifier -> string list

(** 
 * Convert Path(string list) into a string.
 *)
val string_of_path      : Rep_OclType.Path -> string    

(**
 * TODO: Description
 *)
val short_name_of_path  : Rep_OclType.Path -> string    

(**
 * Returns all parents of a classifier. 
 *)
val parents_of          : Classifier -> Classifier list -> Rep_OclType.Path list


(*****************************************
 * RETURN activity_graphs                *
 *****************************************)

(** 
 * TODO: Description
 *)
val activity_graphs_of: Classifier -> Rep_ActivityGraph.ActivityGraph list 

(*****************************************
 * RETURN bool                           *
 *****************************************)
(**
 * Are two types equal?
 *)
val type_equals         : Rep_OclType.OclType -> Rep_OclType.OclType -> bool

(** 
 * Type a collection type?
 *)
val isColl_Type      : Rep_OclType.OclType -> bool
(**
 * Is the classifier visible?
 *)
val is_visible_cl       : Classifier -> bool
(** 
 * Is the operation visible?
 *)
val is_visible_op       : operation -> bool
(**
 * Is the attribute visible?
 *)
val is_visible_attr     : attribute -> bool




(** update model **)


(* visibility *)
val visibility_of : Classifier -> Visibility

exception InvalidArguments of string
exception TemplateError of string 


end

structure Rep_Core :  REP_CORE = 
struct
open library
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

(* convert an association end into the corresponding collection type *)
fun aend_to_attr_type ({name,aend_type,multiplicity,
                        ordered,visibility,init}:associationend) =
    (case multiplicity of 
       [(0,1)] => aend_type
     | [(1,1)] => aend_type
     | _ =>if ordered then Rep_OclType.Sequence aend_type (* OrderedSet? *)
           else Rep_OclType.Set aend_type)
    

fun aend_to_attr (cls_name:string) (aend:associationend):attribute = 
    {name = List.last (#name aend),
     attr_type = aend_to_attr_type aend,
     visibility = #visibility aend,
     scope = XMI.InstanceScope,
     stereotypes = nil,
     init = #init aend}


(* convert a multiplicity range into an invariant of the form *)
(* size > lowerBound and size < upperBound )                 *)
fun range_to_inv cls_name aend (a,b) = 
    let val cls       = Rep_OclType.Classifier cls_name
      val attr_type = aend_to_attr_type aend
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
    
   
fun name_of_association ({name,aends,qualifiers,aclass}:association) = name
fun path_of_association assoc = name_of_association assoc
 
fun short_name_of_path p = (hd o rev) p

fun path_of_aend ({name,aend_type,...}:associationend) = name 

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
	             val attr_type = aend_to_attr_type aend
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
    

fun aends_of_association {name,aends,qualifiers,aclass} = aends

fun association_of_aend ({name,aend_type,...}:associationend) =
	  List.take(name, (List.length name)-1)

fun type_of_aend ({name,aend_type,...}:associationend) = aend_type

fun name_of_aend ({name,aend_type,...}:associationend) = 
    short_name_of_path name

fun role_of_aend ({name,aend_type,...}:associationend) = List.last name

fun associations_of (Class{name,associations,...}) = associations
  | associations_of (AssociationClass{name,associations,association,...}) = 
    associations  
  | associations_of (Primitive{name,associations,...}) = associations
                                                             

fun oppositeAendsOfAssociation name allAssociations associationPath =
    let
      val [association] = List.filter (fn assoc => path_of_association assoc =
                                                   associationPath)
                                      allAssociations
    in
      List.filter (fn aend => type_of_aend aend <> name)
                  (aends_of_association association)
    end

fun incomingAendsOfAssociation name allAssociations associationPath =
    let
      val [association] = List.filter (fn assoc => path_of_association assoc =
                                                   associationPath)
                                      allAssociations
    in
      List.filter (fn aend => type_of_aend aend = name)
                  (aends_of_association association)
    end

(** find the associationends belonging to a classifier.
 * This mean all other associationends from all associations the
 * classifer is part of. For association classes, the belonging 
 * association also needs to be checked.
 * If the association is reflexiv, all aends will be returned.
 *)
fun associationends_of (all_associations:association list) 
                       (Class{name,associations,...}):associationend list = 
    let 
      val oppAends =  
          List.concat (map (oppositeAendsOfAssociation name all_associations) 
                           associations)
      val selfAends = map (incomingAendsOfAssociation name all_associations) 
                          associations
      val filteredSelfAends = List.concat (List.filter (fn x => length x >= 2) selfAends)
    in
        oppAends@filteredSelfAends
    end
  | associationends_of all_associations (AssociationClass{name,associations,
                                                          association,...}) = 
    (* association only contains endpoints to the other, pure classes *)
    let
      val assocs = if List.exists (fn x => x = association ) associations 
                   then associations
		   else association::associations
      val oppAends =  
          List.concat (map (oppositeAendsOfAssociation name all_associations) 
                           assocs)
      val selfAends = map (incomingAendsOfAssociation name all_associations) 
                          associations
      val filteredSelfAends = List.concat (List.filter (fn x => length x >= 2) selfAends)
    in
        oppAends@filteredSelfAends
    end
  | associationends_of all_associations (Primitive{name,associations,...}) = 
    let 
      val oppAends =  
          List.concat (map (oppositeAendsOfAssociation name all_associations) 
                           associations)
      val selfAends = map (incomingAendsOfAssociation name all_associations) 
                          associations
      val filteredSelfAends = List.concat (List.filter (fn x => length x >= 2) selfAends)
    in
        oppAends@filteredSelfAends
    end
  | associationends_of _ _ = error ("in associationends_of: This classifier has no associationends") (*FIXME: or rather []? *)
                                   
                 
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
      val _ = trace function_calls "normalize: class\n"
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
    in
      Class {name   = name,
	     parent = parent,
	     attributes = append (map (aend_to_attr (List.last(path_of_OclType
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
      val _ = trace function_calls "normalize: associationclass\n"
      val _ = trace function_arguments 
                    ("number of associations: "^
                     (Int.toString (List.length associations ))^"\n")
      fun mapPath (aend1,aend2) = (aend1,path_of_aend aend2)
                                  
      val aendPathPairs = (bidirectionalPairs name all_associations
                                                          associations)
    in
      AssociationClass {
      name   = name,
      parent = parent,
      attributes = append (map (aend_to_attr (List.last (path_of_OclType 
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

      
      
fun type_of (Class{name,...})            = name  
  | type_of (AssociationClass{name,...}) = name
  | type_of (Interface{name,...})        = name
  | type_of (Enumeration{name,...})      = name
  | type_of (Primitive{name,...})        = name
  | type_of (Template{classifier,...})   = type_of classifier 


fun name_of (Class{name,...})            = path_of_OclType name  
  | name_of (AssociationClass{name,...}) = path_of_OclType name
  | name_of (Interface{name,...})        = path_of_OclType name
  | name_of (Enumeration{name,...})      = path_of_OclType name
  | name_of (Primitive{name,...})        = path_of_OclType name
  | name_of (Template{classifier,...})   = name_of classifier

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
    
    
fun short_parent_name_of C =  
    (case (parent_name_of C) of
       [] => error "in Rep.short_parent_name_of: empty type"
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

fun attributes_of (Class{attributes,...}) = attributes
  | attributes_of (AssociationClass{attributes,...}) = attributes					    
  | attributes_of (Interface{...})        = 
         error "in Rep.attributes_of: argument is Interface"
  | attributes_of (Enumeration{...})      = 
         error "in Rep.attributes_of: argument is Enumeration"  
  | attributes_of (Primitive{...})         = []  
         (* error "attributes_of <Primitive> not supported" *)  
  | attributes_of (Template{parameter,classifier}) = attributes_of classifier

fun operations_of (Class{operations,...})          = operations
  | operations_of (AssociationClass{operations,...}) = operations
  | operations_of (Interface{operations,...})      = operations
  | operations_of (Enumeration{operations,...})    = operations
  | operations_of (Primitive{operations,...})      = operations  
  | operations_of (Template{parameter,classifier}) = operations_of classifier

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

fun name_of_op ({name,...}:operation) = name
              
fun name_of_ae ({name,...}:associationend) = name

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
     
                               
fun class_of (name:Path) (cl:Classifier list):Classifier = hd (filter (fn a => if ((name_of a) = name)
                                                                               then true else false ) cl )
    handle _ => error ("class_of: class "^(string_of_path name)^" not found!\n")
		



fun parent_of  C cl =  (class_of (parent_name_of C) cl)
                       
fun parents_of C cl = 
    (case parent_name_of C of
       [] => []    
     | class => (if( class = (name_of OclAnyC) )
                 then [(name_of OclAnyC)]
                 else [class]@(parents_of (class_of class cl) cl)))
        
    
(* returns the activity graphs (list) of the given Classifier --> this is a list of StateMachines*)
(* Classifier -> ActivityGraph list *)
fun activity_graphs_of (Class{activity_graphs,...}) = activity_graphs
  | activity_graphs_of _                            = []
				                      
fun operation_of cl fq_name = 
    let 
      val classname   = (rev o  tl o rev) fq_name
      val operations  = operations_of (class_of classname cl)
      val name        = (hd o rev) fq_name	
    in
      SOME(hd (filter (fn a => if ((name_of_op a) = name)
			       then true else false ) operations ))
    end	
        
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
    
fun connected_classifiers_of (all_associations:association list) 
                             (C as Class {attributes,associations,...}) 
                             (cl:Classifier list) =
    let 
      val att_classifiers = List.mapPartial 
                                (fn (Classifier p) => SOME (class_of p cl)
                                  | _              => NONE)
                                (map  #attr_type attributes)
      val aend_classifiers = List.mapPartial 
                                 (fn (Classifier p) => SOME (class_of p cl)
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
                                (fn (Classifier p) => SOME (class_of p cl)
                                  | _              => NONE)
                                (map  #attr_type attributes)
      (* FIXME: correct handling for association classes? *)
      val aend_classifiers = List.mapPartial 
                                 (fn (Classifier p) => SOME (class_of p cl)
                                   | _              => NONE)
                                 (map #aend_type (associationends_of 
                                                      all_associations AC))
    in
      att_classifiers @ aend_classifiers 
    end
  | connected_classifiers_of all_associations(P as Primitive{associations,...})
                             (cl:Classifier list) = 
    List.mapPartial (fn (Classifier p) => SOME (class_of p cl)
                      | _              => NONE)
                    (map #aend_type (associationends_of all_associations P))
  | connected_classifiers_of _  _ _ = nil
                                          
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

fun type_of_term (Literal (s,typ)) = typ
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

fun type_of_CollPart (CollectionItem (term,typ)) = typ
  | type_of_CollPart (CollectionRange (term1,term2,typ)) = typ

fun isColl_Type (Set(x)) = true
  | isColl_Type (Sequence(x)) = true
  | isColl_Type (OrderedSet(x)) = true
  | isColl_Type (Bag(x)) = true
  | isColl_Type (Collection(x)) = true
  | isColl_Type _ = false

fun find_operation op_name (list:operation list) = List.hd (List.filter (fn a => (#name a) = op_name) list)

fun find_attribute att_name (list:attribute list) = List.hd (List.filter (fn a => (#name a) = att_name) list)

fun type_of_parent (Class {parent,...}) = 
    let
	val _ = trace development ("type_of_parent : Class{parent,...} \n")
    in
	Option.valOf(parent)
    end
  | type_of_parent (AssociationClass {parent,...}) = 
    let
	val _ = trace development ("type_of_parent : AssociationClass{parent,...} \n")
    in
	Option.valOf(parent)
    end
  | type_of_parent (Primitive {parent, ...}) = 	Option.valOf(parent)
  | type_of_parent (Interface {parents, ...}) = (List.hd parents)
  | type_of_parent (Template{classifier,...}) = raise TemplateError ("Parent of a class can never be of type template(x).\n")

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

fun real_path ([]) = []
  | real_path ([x]) = []
  | real_path (x::tail) = x::real_path tail

fun string_to_type ["Integer"] = Integer
  | string_to_type ["Boolean"] = Boolean
  | string_to_type ["Real"] = Real
  | string_to_type ["OclAny"] = OclAny
  | string_to_type ["DummyT"] = DummyT
  | string_to_type ["String"] = String
  | string_to_type ["OclVoid"] = OclVoid
  | string_to_type (("oclLib")::tail) = string_to_type tail
  | string_to_type [set] = 
    if (List.exists (fn a => if (a = (#"(")) then true else false) (String.explode set)) then
	(* set *)
	let
	    fun string_to_cons "Set" typ = Set(typ)
	      | string_to_cons "Bag" typ = Bag(typ)
	      | string_to_cons "Collection" typ = Collection (typ)
	      | string_to_cons "OrderedSet" typ = OrderedSet (typ)
	      | string_to_cons "Sequence" typ = Sequence (typ)	
	    fun parse_string c ([]) = ([],[])
	      | parse_string c (h::tail) =
		if (c = h) then
		    ([],h::tail)
		else
 		    (h::(#1 (parse_string c tail)),(#2 (parse_string c tail)))
	    val tokens = parse_string (#"(") (String.explode set)
	    val cons = (#1 tokens)
	    (* delete first "(" and last ")" element *)
	    val tail = List.tl (real_path (#2 tokens))
	    val _ = TextIO.output(TextIO.stdOut,"tail "^ (String.implode tail) ^ "\n")

	in
	    string_to_cons (String.implode cons) (string_to_type ([String.implode tail]))
	end
    else
	Classifier ([set])
  | string_to_type list = Classifier (list)

fun type_of_template (T as Template{classifier,parameter}) =
    (case (name_of classifier) of
	["Collection(T)"] => Collection (parameter)
      | ["Set(T)"] => Set (parameter)
      | ["OrderedSet(T)"] => OrderedSet (parameter)
      | ["Bag(T)"] => Bag (parameter)
      | ["Sequence(T)"] => Sequence (parameter)
    )
  | type_of_template x = raise TemplateError ("type_of_template: Only Template Classifiers can be used.\n")

fun type_equals Integer (Classifier ([OclLibPackage,"Real"])) = true
  | type_equals (Classifier ([OclLibPackage,"Integer"])) Real = true
  | type_equals _ OclAny = true
  | type_equals _ (Classifier ([OclLibPackage,"OclAny"])) = true
  | type_equals x y = 
    if (x = y) then
	true
    else
	false

end
