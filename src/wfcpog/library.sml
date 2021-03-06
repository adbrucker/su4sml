(*****************************************************************************
 * su4sml --- an SML repository for managing (Secure)UML/OCL models
 *             http://projects.brucker.ch/su4sml/
 *                                                                            
 * context_declarations.sml --- 
 * This file is part of su4sml.
 *
 * Copyright (c) 2005-2007 ETH Zurich, Switzerland
 *               2008-2009 Achim D. Brucker, Germany
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
(* $Id: context_declarations.sml 6727 2007-07-30 08:43:40Z brucker $ *)

(** This library provides different functions used in many components of the WFCPO-generator.
 *  This operations are used very often and therefore they are accessible over this library.
 *  Although they are used frequently, it does make no sense to export them to the standard ocl 
 *  library because there just important for implementing constraints.						
 *)
signature WFCPOG_LIBRARY = 
sig
    
    (** Get the name of a certain precondition.*)
    val name_of_precondition      : (string option * Rep_OclTerm.OclTerm) -> string option
    (** Get the name of a certain postcondition.*)
    val name_of_postcondition     : (string option * Rep_OclTerm.OclTerm)-> string option
    (** Get the term of a certain precondition.*)
    val term_of_precondition      : (string option * Rep_OclTerm.OclTerm) -> Rep_OclTerm.OclTerm
    (** Get the term of a certain postcondition.*)
    val term_of_postcondition     : (string option * Rep_OclTerm.OclTerm) -> Rep_OclTerm.OclTerm
    (** Wrap a predicate over an OclTerm.*)
(*    val wrap_predicate            : Rep_OclTerm.OclTerm -> string option -> (Rep_OclTerm.OclTerm * Rep_OclType.OclType) list -> Rep_OclTerm.OclTerm *)
    (** Conjungtion of a list of OclTerms to one single term.*)							 
    val conjugate_terms           : Rep_OclTerm.OclTerm list -> Rep_OclTerm.OclTerm
    (** Conjungtion of a list of HolOclTerms to one single term.*)
    val conjugate_holoclterms     : Rep_OclTerm.OclTerm list -> Rep_OclTerm.OclTerm
    (** Disjunction of a list of OclTerms to one single term.*)
    val disjugate_terms           : Rep_OclTerm.OclTerm list -> Rep_OclTerm.OclTerm
    (** Get an attribute by name. *)
    val get_attribute             : string -> Rep_Core.Classifier -> Rep.Model -> Rep_Core.attribute
    (** Get an associationend by name.*)
    val get_associationend        : string -> Rep_Core.Classifier -> Rep.Model -> Rep_Core.associationend
    (** Get an operation by name. *)
    (*    val get_operation             : string -> Rep_Core.Classifier -> Rep.Model -> Rep_Core.operation *)
    (** *) 
    val class_contains_op         : Rep_Core.operation -> Rep.Model -> Rep_Core.Classifier -> bool
    (** *)
    val class_has_local_op        : string -> Rep.Model -> Rep_Core.Classifier -> bool
    (** Get the class his children *)
    val children_of               : Rep_Core.Classifier -> Rep.Model -> Rep_OclType.Path list
    (** Check inheritance tree for a given property and return first classifer fullfilling property.*)
    val go_up_hierarchy           : Rep_Core.Classifier -> (Rep_Core.Classifier -> bool) ->  Rep.Model -> Rep_Core.Classifier
    (** get the relative path according to the package *)
    val rel_path_of               : Rep_OclType.Path -> Rep_OclType.Path -> Rep_OclType.Path
    (** Substitute (string,Type) args as (Variable(s,Type),Type) args.*)
    val args2varargs              : (string * Rep_OclType.OclType) list -> (Rep_OclTerm.OclTerm * Rep_OclType.OclType) list
    (** Add self as argument *)
    val selfarg                   : Rep_OclType.OclType -> (Rep_OclTerm.OclTerm * Rep_OclType.OclType)
    (** Print option *)
    val opt2string                : string option -> string
    (** Any kind of exceptions. *)
    val is_Class                  : Rep_Core.Classifier -> bool
    val is_AssoClass              : Rep_Core.Classifier -> bool
    val is_Primi                  : Rep_Core.Classifier -> bool
    val is_Enum                   : Rep_Core.Classifier -> bool
    val is_Iface                  : Rep_Core.Classifier -> bool
    val is_Templ                  : Rep_Core.Classifier -> bool
    exception WFCPOG_LibraryError of string
end
structure WFCPOG_Library:WFCPOG_LIBRARY =
struct

(* SU4SML *)
open Rep_Helper
open Rep_Core
open Rep 
open Rep_OclType
open Rep_OclTerm
open OclLibrary
open Rep2String
open XMI_DataTypes
(* OclParser *)



exception WFCPOG_LibraryError of string 


fun opt2string (NONE) = ""
  | opt2string (SOME(s)) = s


fun is_Class (Class{...}) = true
  | is_Class x = false

fun is_AssoClass (AssociationClass{...}) = true
  | is_AssoClass x = false

fun is_Enum (Enumeration{...}) = true
  | is_Enum x = false

fun is_Templ (Template{...}) = true
  | is_Templ x = false

fun is_Primi (Primitive{...}) = true
  | is_Primi x = false

fun is_Iface (Interface{...}) = true
  | is_Iface x = false

fun name_of_precondition ((a:string option),(t:OclTerm)) = a

fun name_of_postcondition ((a:string option),(t:OclTerm)) = a

fun term_of_precondition ((a:string option),(t:OclTerm)) = t

fun term_of_postcondition ((a:string option),(t:OclTerm)) = t

(* FixME: adapter for info in subterm *)
(* fun holocl_adapter path (t:OclTerm) =

    OperationCall (
*)

fun conjugate_terms [] = raise WFCPOG_LibraryError ("Empty list not conjugateable. \n")
  | conjugate_terms [x:OclTerm] = (x)
  | conjugate_terms ((h:OclTerm)::tail) = 
    let
	val x = conjugate_terms tail
    in
	if (type_of_term h = Boolean)
	then (OperationCall(h,type_of_term h,["oclLib","Boolean","and"],[(x,type_of_term x)],Boolean))
	else raise WFCPOG_LibraryError ("type of term is not Boolean. \n") 
    end

fun conjugate_holoclterms [] = raise WFCPOG_LibraryError ("Empty list not conjugateable. \n")
  | conjugate_holoclterms [x] = x
  | conjugate_holoclterms (h::tail) = 
    let
	val x = conjugate_holoclterms tail
    in
	if (type_of_term h = Boolean)
	then (OperationCall(h,type_of_term h,["holOclLib","Boolean","and"],[(x,type_of_term x)],Boolean))
	else raise WFCPOG_LibraryError ("type of term is not Boolean. \n") 
    end

fun disjugate_terms [] = raise WFCPOG_LibraryError("Empty list not disjugateable. \n")
  | disjugate_terms [x:OclTerm] = (x)
  | disjugate_terms ((h:OclTerm)::tail) =
    let
	val x = disjugate_terms tail
    in
	if (type_of_term h = Boolean)
	then (OperationCall(h,type_of_term h,["oclLib","Boolean","or"],[(x,type_of_term x)],Boolean))
	else raise WFCPOG_LibraryError ("type of term is not Boolean. \n")
    end

fun filter_out_none [] = []
  | filter_out_none (NONE::tail) = filter_out_none tail
  | filter_out_none (SOME(x)::tail) = (SOME(x)::(filter_out_none tail))

fun class_contains_op oper model classifier = 
    let
	val ops = local_operations_of classifier 
    in
	List.exists (fn a => if (#name oper) = (#name a) 
				andalso (sig_conforms_to (arguments_of_op oper) (arguments_of_op a) model)
			     then true
			     else false) ops
    end

fun class_has_local_op name model classifier = 
    let
	val ops = local_operations_of classifier 
    in
	List.exists (fn a => (#name a) = name) ops
    end


fun get_operation s classifier model = 
    let
	val x = List.find (fn a => if (name_of_op a = s) then true else false) (all_operations_of classifier model)
    in
	case x of
	    NONE => raise WFCPOG_LibraryError ("No operation found using 'get_operation'.\n")
	  | SOME (x) => x
    end


fun get_attribute s classifier model = 
    let
	val x = List.find (fn a => if ((#name a) = s) then true else false) (all_attributes_of classifier model)
    in
	case x of
	    NONE => 
	    let
		val _ = Logger.info ("No such Attribute: \n In Classifier "^(string_of_path (name_of classifier))^" in attribute "^s)
	    in
		raise WFCPOG_LibraryError ("No attribute found using 'get_attribute'.\n")
	    end
	  | SOME (x) => x
    end

fun get_associationend s classifier model = 
    let
	val x = List.find (fn a => if ((List.last(#name a)) = s) then true else false) (all_associationends_of classifier model)
    in
	case x of 
	    NONE =>
	    let
		val _ = Logger.info ("No such associationend: \n In Classifier "^(string_of_path (Rep_Core.name_of classifier))^" no associationend called "^(s)^".\n")
	    in
		raise WFCPOG_LibraryError ("No attribute found using 'get_attribute'.\n")
	    end 
	  | SOME(x) => x
    end


fun go_up_hierarchy location func (model as (clist,alist)) = 
    let 
	val parent = parent_of location model
    in
	if (func parent = true) 
	then  parent
	else 
	    (if (type_of parent = OclAny) 
	     then raise WFCPOG_LibraryError ("No such property using 'go_up_hierarchy'.\n")
	     else go_up_hierarchy parent func model
	    )
    end



fun children_of class (model as ([],alist)) = []
  | children_of class (model as ((h::tail),alist)) = 
    if (parent_of h model = class)
    then (name_of h)::(children_of class ((tail,alist)))
    else (children_of class ((tail,alist)))

fun has_children class (model as (clist,alist)) = 
    let 
	val ch = children_of class model
    in
	if (List.length (ch) = 0)
	then false
	else true
    end
    





fun rel_path_of [] name = name
  | rel_path_of [x] [y] = if (x=y) then [] else raise WFCPOG_LibraryError ("rel_path_of only possible for name with same package/prefix.\n")
  | rel_path_of [x] name = if (x = List.hd (name)) then (List.tl (name)) else raise WFCPOG_LibraryError ("rel_path_of only possible for name with same package/prefix")
  | rel_path_of pkg name =
    if (List.hd(pkg) = List.hd(name)) then (rel_path_of (List.tl pkg) (List.tl name)) else raise WFCPOG_LibraryError ("rel_path_of only possible for name with same package/prefix")

fun args2varargs [] = []
  | args2varargs ((a,b)::tail) = (Variable(a,b),b)::(args2varargs tail)

fun selfarg typ = (Variable("self",typ),typ)

end;
