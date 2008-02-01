(*****************************************************************************
 * su4sml --- a SML repository for managing (Secure)UML/OCL models
 *             http://projects.brucker.ch/su4sml/
 *                                                                            
 * context_declarations.sml --- 
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
 *     * Redistributions of source code must retain the above copyright *       notice, this list of conditions and the following disclaimer.
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
signature WFCPO_LIBRARY = 
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
    val wrap_predicate            : Rep_OclTerm.OclTerm -> string option -> (Rep_OclTerm.OclTerm * Rep_OclType.OclType) list -> Rep_OclTerm.OclTerm
    (** Conjugate a list of terms to one single term.*)							 
    val conjugate_terms           : Rep_OclTerm.OclTerm list -> Rep_OclTerm.OclTerm
    (** Transform a option list to a normal list.*)
    val optlist2list              : 'a option list -> 'a list
    (** Get a local operation by name. *)
    val get_operation             : string -> Rep_Core.Classifier -> Rep_Core.operation
    (** Test wheter the signatures are type consistent.*)
    val sig_conforms_to           : (string * Rep_OclType.OclType) list -> (string * Rep_OclType.OclType) list -> Rep.Model -> bool
    (** Check if the operation is a refinement of another operation.*)
    val same_op                   : Rep_Core.operation -> Rep_Core.operation -> Rep.Model -> bool
    (** *) 
    val class_contains_op         : Rep_Core.operation -> Rep.Model -> Rep_Core.Classifier -> bool
    (** Get the local operations of a classifier.*)
    val local_operations_of       : Rep_OclType.Path -> Rep.Model -> Rep_Core.operation list
    (** Get the redefined/refined operations of a classifier.*)
    val modified_operations_of    : Rep_OclType.Path -> Rep.Model -> Rep_Core.operation list
    (** Get all the inherited (without the redefined ones) operations of a classifier.*)
    val inherited_operations_of   : Rep_OclType.Path -> Rep.Model -> Rep_Core.operation list
    (** Get all operations of a classifier (for redefined ones the more special is choosen).*)
    val all_operations_of         : Rep_OclType.Path -> Rep.Model -> Rep_Core.operation list
    (** Get all creators of a classifier.*)
    val creation_operations_of    : Rep_OclType.Path -> Rep.Model -> Rep_Core.operation list
    (** Get all destroying operations of a classifier.*)
    val destruction_operations_of     : Rep_OclType.Path -> Rep.Model -> Rep_Core.operation list
    (** Get the class his children *)
    val children_of               : Rep_Core.Classifier -> Rep.Model -> Rep_OclType.Path list
    (** Check inheritance tree for a given property and return first classifer fullfilling property.*)
    val go_up_hierarchy           : Rep_Core.Classifier -> (Rep_Core.Classifier -> bool) ->  Rep.Model -> Rep_Core.Classifier
    (** Get the local invariants of a classifier.*)
    val local_invariants_of       : Rep_Core.Classifier -> (string option * Rep_OclTerm.OclTerm) list
    (** Get the inherited invarinats of a classifier.*)
    val inherited_invariants_of   : Rep_Core.Classifier -> Rep.Model -> (string option * Rep_OclTerm.OclTerm) list
    (** Get all invariants of a classifier.*)
    val all_invariants_of         : Rep_Core.Classifier -> Rep.Model -> (string option * Rep_OclTerm.OclTerm) list
    (** get the relative path according to the package *)
    val rel_path_of               : Rep_OclType.Path -> Rep_OclType.Path -> Rep_OclType.Path
    (** Substitute a package name of a path. *)
    val substitute_package        : Rep_OclType.Path -> Rep_OclType.Path -> Rep_OclType.Path -> Rep_OclType.Path
    (** Any kind of exceptions. *)
    exception WFCPO_LibraryError of string 
end
structure WFCPO_Library:WFCPO_LIBRARY =
struct

(* SU4SML *)
open Rep_Core
open Rep 
open Rep_OclType
open Rep_OclTerm
open OclLibrary
open Rep2String

(* OclParser *)
open Ext_Library

(* WFCPO-Gen *)
open WFCPO_Naming
open Base_Constraint

exception WFCPO_LibraryError of string 

fun name_of_precondition ((a:string option),(t:OclTerm)) = a

fun name_of_postcondition ((a:string option),(t:OclTerm)) = a

fun term_of_precondition ((a:string option),(t:OclTerm)) = t

fun term_of_postcondition ((a:string option),(t:OclTerm)) = t

(* FixME: adapter for info in subterm *)
(* fun holocl_adapter path (t:OclTerm) =
    OperationCall (
*)

fun wrap_predicate term (NONE) args = Predicate (term,type_of_term term,[generate_name "gen_name"],args)
  | wrap_predicate term (SOME(x)) args = Predicate (term,type_of_term term,[x],args)

fun conjugate_terms [] = raise WFCPO_LibraryError ("Empty list not conjugateable. \n")
  | conjugate_terms [x:OclTerm] = (x)
  | conjugate_terms ((h:OclTerm)::tail) = 
    let
	val x = conjugate_terms tail
    in
	if (type_of_term h = Boolean)
	then (OperationCall(h,type_of_term h,["oclLib","Boolean","implies"],[(x,type_of_term x)],Boolean))
	else raise WFCPO_LibraryError ("type of term is not Boolean") 
    end

(* create normal list from a list of options type *)
fun optlist2list [] = []
  | optlist2list (h::tail) =
    (
     case h  of
	 NONE => optlist2list (tail)
       | SOME (e) => (e::(optlist2list tail))
    )

fun filter_out_none [] = []
  | filter_out_none (NONE::tail) = filter_out_none tail
  | filter_out_none (SOME(x)::tail) = (SOME(x)::(filter_out_none tail))

fun get_operation s classifier = 
    let
	val x = List.find (fn a => if (name_of_op a = s) then true else false) (operations_of classifier)
    in
	case x of
	    NONE => raise WFCPO_LibraryError ("No operation found using 'get_operation'.\n")
	  | SOME (x) => x
    end


(* check whether two given signatures match each other from the type point of view *)
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

fun same_op (sub_op:operation) (super_op:operation) (model:Model) =
    if ((name_of_op sub_op = name_of_op super_op ) andalso (sig_conforms_to (arguments_of_op sub_op) (arguments_of_op super_op) model))
    then true
    else false

fun class_contains_op oper model classifier = 
    let
	val ops = local_operations_of (name_of classifier) model
    in
	List.exists (fn a => if (#name oper) = (#name a) 
				andalso (sig_conforms_to (arguments_of_op oper) (arguments_of_op a) model)
			     then true
			     else false) ops
    end

(* get all local operations of a classifier *)
and local_operations_of c_name model = operations_of (class_of c_name (#1 model))

fun embed_local_operation oper [] model = [oper]
  | embed_local_operation lop ((oper:operation)::iops) model = 
    if (same_op lop oper model)
    then (lop::iops)
    else (oper)::(embed_local_operation lop iops model) 

(* embed local operations to the inherited operations *)
fun embed_local_operations [] iops model = iops
  | embed_local_operations x [] model = x
  | embed_local_operations (h::tail) iops model = 
    let
	val tmp = embed_local_operation h iops model
    in
	(embed_local_operations tail tmp model)
    end

(* get all inherited operations of a classifier, without the local operations *)
fun inherited_operations_of c_name (model as (clist,alist)) =
    let
	val class = class_of c_name (#1 model)
	val parents = parents_of class (#1 model)
	val c_parents = List.map (fn a => class_of a (#1 model)) parents
	val ops_of_par = (List.map (operations_of) c_parents)
    in
	List.foldr (fn (a,b) => embed_local_operations a b model) (List.last (ops_of_par)) ops_of_par
    end
		      
(* get absolutelly all operations of a classifier. In case of a functions which occurs serveral times in the inheritance tree, the must specified function is listed. *)
fun all_operations_of c_name model =
    let 
	val lo = local_operations_of c_name model
	val io = inherited_operations_of c_name model
    in
	embed_local_operations lo io model
    end

(* get all local operations, which occurs in one of the parent classes at least each time also *)
fun modified_operations_of c_name model = 
    let
	val io = inherited_operations_of c_name model
	val lo = local_operations_of c_name model
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


fun creation_operations_of c_name (model:Rep.Model) = 
    let
	val oper = all_operations_of c_name model
	val creators = List.filter (fn a => List.exists (fn b => b = "create") (#stereotypes a)) (oper)
    in
	creators
    end   

fun destruction_operations_of c_name (model:Rep.Model) = 
    let
	val oper = all_operations_of c_name model
	val creators = List.filter (fn a => List.exists (fn b => b = "destroy") (#stereotypes a)) (oper)
    in
	creators
    end   

fun go_up_hierarchy location func (model as (clist,alist)) = 
    let 
	val parent = parent_of location (#1 model)
    in
	if (func parent = true) 
	then  parent
	else 
	    (if (type_of parent = OclAny) 
	     then raise WFCPO_LibraryError ("No such property using 'go_up_hierarchy'.\n")
	     else go_up_hierarchy parent func model
	    )
    end



fun children_of class (model as ([],alist)) = []
  | children_of class (model as ((h::tail),alist)) = 
    if (parent_of h (#1 model) = class)
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
    


fun local_invariants_of class = invariant_of class

fun inherited_invariants_of class (model:Rep.Model as (clist,alist)) = 
    let
	val parent = parent_of class (#1 model)
    in
	if (type_of parent = OclAny) 
	then []
	else (local_invariants_of class)@(inherited_invariants_of parent model)
    end

fun all_invariants_of class model = 
    (local_invariants_of class)@(inherited_invariants_of class model)


fun rel_path_of [] name = name
  | rel_path_of [x] [y] = if (x=y) then [] else raise WFCPO_LibraryError ("rel_path_of only possible for name with same package/prefix.\n")
  | rel_path_of [x] name = if (x = List.hd (name)) then (List.tl (name)) else raise WFCPO_LibraryError ("rel_path_of only possible for name with same package/prefix")
  | rel_path_of package name =
    if (List.hd(package) = List.hd(name)) then (rel_path_of (List.tl package) (List.tl name)) else raise WFCPO_LibraryError ("rel_path_of only possible for name with same package/prefix")


fun substitute_package [] tpackage [] = raise WFCPO_LibraryError ("Not possible to substitute package since names belongs to package itself and not a class of it.\n")
  | substitute_package [] tpackage path = tpackage@path
  | substitute_package x tpackage [] = raise WFCPO_LibraryError ("Not Possible to substitute Package since package longer than path.\n")
  | substitute_package (hf::fpackage) (tpackage) (hp::path) = 
    if (hf = hp) 
    then substitute_package fpackage tpackage path
    else (hp::path)

end;
