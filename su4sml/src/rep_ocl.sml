(*****************************************************************************
 * su4sml --- a SML repository for managing (Secure)UML/OCL models
 *             http://projects.brucker.ch/su4sml/
 *                                                                            
 * rep_ocl.sml --- 
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

(** Repository datatypes and helper functions for UML/OCL types. *)
signature REP_OCL_TYPE =
sig
    
    type Path = string list

    datatype OclType    =  Integer | Real | String | Boolean | OclAny 
		         | Set of OclType | Sequence of OclType
		         | OrderedSet of OclType | Bag of OclType 
		         | Collection of OclType | OclState
			 | TupleType of (string * OclType) list 
		         | Classifier of Path | OclVoid | DummyT | TemplateParameter of string

    val short_name_of_OclType: OclType -> string
    val path_of_OclType   : OclType -> Path
    val collection_type_of_OclType : OclType -> OclType
    val string_of_OclType : OclType -> string	
    val string_of_path    : Path -> string	
    val path_to_string: Path -> string -> string
    val string_of_OclType_colon : OclType -> string
    val pathstring_of_path: Path -> string
    val is_Classifier     : OclType -> bool
    val is_Collection     : OclType -> bool
end
     
    
(** Repository datatypes and helper functions for OCL expressions. *)
signature REP_OCL_TERM =
sig
include REP_OCL_TYPE

datatype OclTerm = 
	       Literal of string * OclType         (* Literal with type  *)
	     | Tuple of 
	             (string * OclTerm * OclType ) list     
	     | CollectionLiteral of CollectionPart list 
				    * OclType      (* content with type  *)
	     | If of OclTerm * OclType             (* condition          *)
		     * OclTerm * OclType           (* then               *)
		     * OclTerm * OclType           (* else               *)
		     * OclType                     (* result type        *)
	     | QualifiedAssociationEndCall of 
	               OclTerm * OclType           (* source    *)
                     * (OclTerm * OclType) list    (* qualies*)
                     * Path                        (* assoc.-enc         *)
                     * OclType                     (* result type        *)
	     | AssociationEndCall of 
	               OclTerm * OclType           (* source             *)
	             * Path                        (* assoc.-enc         *)
	             * OclType                     (* result type        *)
	     | AttributeCall of 
                       OclTerm * OclType           (* source             *)
	             * Path                        (* attribute          *)
		     * OclType                     (* result type        *)
	     | OperationCall of 
                       OclTerm * OclType           (* source             *)
		     * Path                        (* operation          *)
		     * (OclTerm * OclType) list    (* parameters         *)
		     * OclType                     (* result tupe        *)
	     | OperationWithType of 
                       OclTerm * OclType           (* source             *)
		     * string * OclType            (* type parameter     *)
		     * OclType                     (* result type        *)
	     | Predicate of 
                       OclTerm * OclType           (* source             *)
		     * Path                        (* name               *)
		     * (OclTerm * OclType) list    (* arguments          *)
	     | Variable of 
                       string * OclType            (* name with type     *)
	     | Let      of 
                       string * OclType            (* variable           *)
		     * OclTerm * OclType           (* rhs                *)
		     * OclTerm * OclType           (* in                 *)
	     | Iterate of 
                       (string * OclType) list     (* iterator variables *)
		     * string * OclType * OclTerm  (* result variable    *)
		     * OclTerm * OclType           (* source             *)
		     * OclTerm * OclType           (* iterator body      *)
		     * OclType                     (* result type        *)
	     | Iterator of 
                       string                      (* name of iterator   *)
		     * (string * OclType) list     (* iterator variables *)
		     * OclTerm * OclType           (* source             *)
		     * OclTerm * OclType           (* iterator-body      *)
		     * OclType                     (* result type        *)

     and CollectionPart = CollectionItem  of OclTerm 
                                           * OclType
	                | CollectionRange of OclTerm   (* first          *)
		         		   * OclTerm   (* last           *)
                                           * OclType
end


structure Rep_OclType : REP_OCL_TYPE =
struct
open Rep_Helper
open Rep_Logger

type Path = string list
	    
	    
datatype OclType    =  Integer | Real | String | Boolean | OclAny 
		     | Set of OclType | Sequence of OclType
		     | OrderedSet of OclType | Bag of OclType 
		     | Collection of OclType | OclVoid | DummyT
		     | TupleType of (string * OclType) list | OclState
		     | Classifier of Path
		     | TemplateParameter of string


(** Convert Path to a string using given separator *)
fun path_to_string (path:Path) separator = case path of
			      [] => ""
			    | p  => foldr1 (fn (a,b) => a^separator^b) p

(** Convert Path to a string using ., creating a Java package name like string *)
fun string_of_path (path:Path) = path_to_string path "."

(** Convert Path to a string using /, creating a Unix directory like string *)
fun pathstring_of_path (path:Path) = path_to_string path "/"



fun string_of_OclType' f Integer        = "Integer" 
  | string_of_OclType' f Real           = "Real"
  | string_of_OclType' f String         = "String"
  | string_of_OclType' f Boolean        = "Boolean"
  | string_of_OclType' f OclAny         = "OclAny"
  | string_of_OclType' f (Set t)	      = ("Set("^(string_of_OclType' f t)^")")
  | string_of_OclType' f (Sequence t)   = ("Sequence("^(string_of_OclType' 
                                                            f t)^")")	
  | string_of_OclType' f (OrderedSet t) = ("OrderedSet("^(string_of_OclType' 
                                                              f t)^")")	
  | string_of_OclType' f (Bag t)        = ("Bag("^(string_of_OclType' 
                                                       f t)^")")	
  | string_of_OclType' f (Collection t) = ("Collection("^(string_of_OclType' 
                                                              f t)^")")	
  | string_of_OclType' f OclVoid        = "OclVoid"
  | string_of_OclType' f (Classifier p) = (path_to_string p f)
  | string_of_OclType' f DummyT         = "DummyT"
  | string_of_OclType' f (TemplateParameter s) = "TemplateParameter \""^s^"\"" 

fun collection_type_of_OclType (Set t)        = t
  | collection_type_of_OclType (Sequence t)   = t
  | collection_type_of_OclType (OrderedSet t) = t
  | collection_type_of_OclType (Bag t)        = t
  | collection_type_of_OclType (Collection t) = t


fun string_of_OclType t = string_of_OclType' "." t

fun path_of_OclType (Classifier p) = p
  | path_of_OclType (Integer) = ["oclLib","Integer"]
  | path_of_OclType (Boolean) = ["oclLib","Boolean"]
  | path_of_OclType (Real) = ["oclLib","Real"]
  | path_of_OclType (String) = ["oclLib","String"]
  | path_of_OclType (OclAny) = ["oclLib","OclAny"]
  | path_of_OclType (OclVoid) = ["oclLib","OclVoid"]
  | path_of_OclType (TemplateParameter p) = [] (* FIXME *)
  | path_of_OclType x = ["oclLib",string_of_OclType x]

fun short_name_of_OclType t = (List.last o path_of_OclType) t


(** Convert OclType to a string with :: in between *)
fun string_of_OclType_colon t = string_of_OclType' "::" t

fun is_Classifier (Classifier p) = true
  | is_Classifier _              = false 

fun is_Collection (Set _)        = true
  | is_Collection (Sequence _)   = true
  | is_Collection (OrderedSet _) = true
  | is_Collection (Bag _)        = true
  | is_Collection (Collection _) = true
  | is_Collection _              = false
end
     
    
structure Rep_OclTerm : REP_OCL_TERM =
struct
open Rep_OclType

datatype OclTerm = 
	       Literal of string * OclType         (* Literal with type  *)
	     | Tuple of 
	             (string * OclTerm * OclType) list
	     | CollectionLiteral of CollectionPart list 
				    * OclType      (* content with type  *)
	     | If of OclTerm * OclType             (* condition          *)
		     * OclTerm * OclType           (* then               *)
		     * OclTerm * OclType           (* else               *)
		     * OclType                     (* result type        *)
	     | QualifiedAssociationEndCall of 
	               OclTerm * OclType           (* source    *)
                     * (OclTerm * OclType) list    (* qualies*)
                     * Path                        (* assoc.-enc         *)
                     * OclType                     (* result type        *)
	     | AssociationEndCall of 
	               OclTerm * OclType           (* source             *)
	             * Path                        (* assoc.-enc         *)
	             * OclType                     (* result type        *)
	     | AttributeCall of 
                       OclTerm * OclType           (* source             *)
	             * Path                        (* attribute          *)
		     * OclType                     (* result type        *)
	     | OperationCall of 
                       OclTerm * OclType           (* source             *)
		     * Path                        (* operation          *)
		     * (OclTerm * OclType) list    (* parameters         *)
		     * OclType                     (* result tupe        *)
	     | OperationWithType of 
                       OclTerm * OclType           (* source             *)
		     * string * OclType            (* type parameter     *)
		     * OclType                     (* result type        *)
	     | Predicate of 
                       OclTerm * OclType           (* source             *)
		     * Path                        (* name               *)
		     * (OclTerm * OclType) list    (* arguments          *)
	     | Variable of 
                       string * OclType            (* name with type     *)
	     | Let      of 
                       string * OclType            (* variable           *)
		     * OclTerm * OclType           (* rhs                *)
		     * OclTerm * OclType           (* in                 *)
	     | Iterate of 
                       (string * OclType) list     (* iterator variables *)
		     * string * OclType * OclTerm  (* result variable    *)
		     * OclTerm * OclType           (* source             *)
		     * OclTerm * OclType           (* iterator body      *)
		     * OclType                     (* result type        *)
	     | Iterator of 
                       string                      (* name of iterator   *)
		     * (string * OclType) list     (* iterator variables *)
		     * OclTerm * OclType           (* source             *)
		     * OclTerm * OclType           (* iterator-body      *)
		     * OclType                     (* result type        *)



     and CollectionPart = CollectionItem  of OclTerm 
                                           * OclType
	                | CollectionRange of OclTerm   (* first          *)
		         		   * OclTerm   (* last           *)
                                           * OclType
	 
end


structure Rep_OclHelper =
struct
open Rep_OclTerm

exception InvalidArguments of string

(** gives the type of an OCL expression.
 * Should be moved to Rep_Ocl? 
 *)
fun type_of (Literal                (_,t)) = t
  | type_of (CollectionLiteral      (_,t)) = t
  | type_of (If           (_,_,_,_,_,_,t)) = t
  | type_of (AssociationEndCall (_,_,_,t)) = t
  | type_of (AttributeCall      (_,_,_,t)) = t
  | type_of (OperationCall    (_,_,_,_,t)) = t
  | type_of (OperationWithType(_,_,_,_,t)) = t
  | type_of (Variable               (_,t)) = t
  | type_of (Let            (_,_,_,_,_,t)) = t
  | type_of (Iterate  (_,_,_,_,_,_,_,_,t)) = t
  | type_of (Iterator     (_,_,_,_,_,_,t)) = t
  | type_of (Predicate(_,_,_,_)) = Boolean
(* or rather short_string_of ?*)
fun term_name_of (Literal            _) = "Literal"
  | term_name_of (CollectionLiteral  _) = "CollectionLiteral"
  | term_name_of (If                 _) = "If"
  | term_name_of (AssociationEndCall _) = "AssociationEndCall"
  | term_name_of (AttributeCall      _) = "AttributeCall"
  | term_name_of (OperationCall      _) = "OperationCall"
  | term_name_of (OperationWithType  _) = "OperationWithType"
  | term_name_of (Variable           _) = "Variable"
  | term_name_of (Let                _) = "Let"
  | term_name_of (Iterate            _) = "Iterate"
  | term_name_of (Iterator           _) = "Iterator"


fun self t = Variable ("self",t)
fun result t = Variable ("result", t)

(* apply f to aendCalls, attCalls and qualiCalls *)
fun mapOclCalls f (If(cond,condType,thenn,thennType,elsee,elseeType,
                      resultType))=
    If(mapOclCalls f cond,condType,
       mapOclCalls f thenn,thennType,
       mapOclCalls f elsee,elseeType,
       resultType)
  | mapOclCalls f (QualifiedAssociationEndCall(source, sourceType,
                                               qualifierVals,path,
                                               resultType)) =
    f (QualifiedAssociationEndCall (mapOclCalls f source, sourceType,
                                    qualifierVals, path,
                                    resultType))
  | mapOclCalls f (AssociationEndCall(source,sourceType,path,
                                      resultType)) =
    f (AssociationEndCall(mapOclCalls f source,sourceType,
                          path,resultType))
  | mapOclCalls f (AttributeCall(source,sourceType,path,resultType)) =
    f (AttributeCall(mapOclCalls f source,sourceType,
                     path,resultType))
  | mapOclCalls f (OperationCall(source,sourceType,path,parameters,
                                 resultType)) =
    let
      fun handleParameter (term,termType) = (mapOclCalls f term,termType)
    in
      OperationCall(mapOclCalls f source,sourceType,
                    path,map handleParameter parameters,resultType)
    end
  | mapOclCalls f (OperationWithType(source,sourceType,var,varType,
                                     resulType)) =
    OperationWithType(mapOclCalls f source,sourceType,
                      var,varType,resulType)
  | mapOclCalls f (Let(name,nameType,rhs,rhsType,body,bodyType)) =
    Let(name,nameType,
        mapOclCalls f rhs,rhsType,
        mapOclCalls f body,bodyType)
  | mapOclCalls f (Iterate (vars,name,nameType,nameTerm,
                            source,sourceType,body,
                            bodyType,resultType)) =
    Iterate (vars,
             name,nameType, mapOclCalls f nameTerm,
             mapOclCalls f source,sourceType,
             mapOclCalls f body,bodyType,
             resultType)
  | mapOclCalls f (Iterator (name,vars,source,sourceType,body,bodyType,
                             resultType)) =
    Iterator (name,vars,
              mapOclCalls f source, sourceType,
              mapOclCalls f body,bodyType,
              resultType)
  | mapOclCalls f x = x



(* BUG: let...? *)
fun ocl_let var rhs body = Let (var,type_of rhs,rhs,type_of rhs,body,type_of body)
fun ocl_opcall source f args t  = OperationCall (source, type_of source, f,
                                                 map (fn x => (x,type_of x)) args,
                                                 t)
fun ocl_attcall source att t    = AttributeCall (source, type_of source, att, t)
fun ocl_aendcall source aend t  = AssociationEndCall (source, type_of source, 
                                                      aend, t)
fun ocl_opwithtype source f t s = OperationWithType (source, type_of source, f,
                                                     t, s)
                                                    
(* requires type_of t = type_of e *)
fun ocl_if  cond t e = If (cond, type_of cond, t, type_of t, e, type_of e,
                           (* FIXME: use the least common supertype of t and e *)
                           (* or even DummyT?                                  *)
                           type_of t)

(* requires type_of init = type_of body *)
fun ocl_iterate var acc init source body = Iterate ([(var,type_of source)],
                                                    acc, type_of init, init,
                                                    source, type_of source,
                                                    body, type_of body,
                                                    type_of init)

(* Boolean *)
val ocl_true        = Literal ("true",Boolean) 
val ocl_false       = Literal ("false",Boolean)
fun ocl_not     a   = ocl_opcall a ["oclLib", "Boolean", "not"]      []  Boolean
fun ocl_and     a b = ocl_opcall a ["oclLib", "Boolean", "and"]      [b] Boolean
fun ocl_or      a b = ocl_opcall a ["oclLib", "Boolean", "or"]       [b] Boolean
fun ocl_xor     a b = ocl_opcall a ["oclLib", "Boolean", "xor"]      [b] Boolean
fun ocl_implies a b = ocl_opcall a ["oclLib", "Boolean", "implies"]  [b] Boolean
fun ocl_and_all [] = raise (InvalidArguments "rep_ocl.ocl_and_all: empty argument list")
  | ocl_and_all [a] = a
  | ocl_and_all (a::xs) = ocl_and a (ocl_and_all xs)
fun ocl_or_all [] = raise (InvalidArguments "rep_ocl.ocl_or_all: empty argument list")
  | ocl_or_all [a] = a
  | ocl_or_all (a::xs) = ocl_or a (ocl_or_all xs)

(* Integer: -,+,-,*,/,abs,div,mod,max,min               *)
(* Real   : -,+,-,*,/,abs,floor,round,max,min,<,>,<=,>= *)
(* String : size, concat, substring, toInteger, toReal  *)
fun ocl_leq a b = ocl_opcall a ["oclLib", "Integer", "<="] [b] Boolean
fun ocl_geq a b = ocl_opcall a ["oclLib", "Integer", ">="] [b] Boolean

(* OclAny *)
fun ocl_eq a b  = ocl_opcall a ["oclLib", "OclAny", "="] [b] Boolean
fun ocl_neq a b = ocl_opcall a ["oclLib", "OclAny", "<>"] [b] Boolean
fun ocl_isNew a = ocl_opcall a ["oclLib", "OclAny", "oclIsNew"] nil Boolean
fun ocl_isUndefined  a = ocl_opcall a ["oclLib", "OclAny", "oclIsUndefined"] nil Boolean
fun ocl_allInstances s = ocl_opcall s ["oclLib", "OclAny", "allInstances"] nil 
				    (Set (type_of s)) 
fun ocl_isTypeOf a t = ocl_opwithtype a "oclIsTypeOf" t Boolean
fun ocl_isKindOf a t = ocl_opwithtype a "oclIsKindOf" t Boolean
fun ocl_asType   a t = ocl_opwithtype a "oclAsType"   t t

(* Collection: size,includes,excludes,count,includesAll,excludesAll,isEmpty, *)
(*             notEmpty,sum, product                                         *)
(* Set       : union_set,union_bag,=,intersection_set,intersection_bag,-,    *)
(*             including,excluding,symmetricDifference,count,flatten,asSet,  *)
(*             asOrderedSet,asSequence,asBag                                 *)
(* OrderedSet: append,prepend,insertAt,subOrderedSet,at,indexOf,first,last   *)
(* Bag       : =,union_bag,union_set,intersection_bag,intersection_set,      *)
(*             including,excluding,count,flatten,asBag,asSequence,asSet,     *)
(*             asOrderedSet                                                  *)
(* Sequence  : count,=,union,flatten,append,prepend,insertAt,subSequence,    *)
(*             at,indexOf,first,last,including,excluding,asBag,asSequence,   *)
(*             asSet,asOrderedSet                                            *)

fun ocl_includes a b = ocl_opcall a ["oclLib", "Collection", "includes"] [b] Boolean
fun ocl_includesAll a b = ocl_opcall a ["oclLib", "Collection", "includesAll"] [b] Boolean
fun ocl_excludes a b = ocl_opcall a ["oclLib", "Collection", "excludes"] [b] Boolean
fun ocl_excludesAll a b = ocl_opcall a ["oclLib", "Collection", "excludesAll"] [b] Boolean
fun ocl_intersection_set a b = ocl_opcall a ["oclLib", "Set", "intersection_set"] [b] (Set (type_of a))

fun ocl_modifiedOnly a = ocl_opcall a ["oclLib", "Set", "modifiedOnly"] [] Boolean

fun ocl_size a = ocl_opcall a ["oclLib", "Collection", "size"] [] Integer

(* Collection constructors *)
fun ocl_set xs t = CollectionLiteral (map (fn x => CollectionItem (x, type_of x)) xs, Set t)


(* Iterators(Collection): exists,forAll,isUnique,any,one,collect             *)
(* Iterators(Set)       : select,reject,collectNested,sortedBy               *)
(* Iterators(Bag)       : select,reject,collectNested,sortedBy               *)
(* Iterators(Sequence)  : select,reject,collectnested,sortedBy               *)

(* FIXME: automatically take a "fresh" variable that is free in s? *)
(* But then you need this information in body... *) 
fun ocl_collect source var body = Iterator ("collect", [(var,type_of source)],
                                            source, type_of source,
                                            body, type_of body,
                                            Bag (type_of body))

(* source::Collection/Set/..., variables:: Variable list , body:: expression to be evaluated *)
(* body must evaluate to Boolean *)
fun ocl_forAll (source:OclTerm) (variables:OclTerm list) (body:OclTerm) = 
    let
	    fun strip_var (Variable(name,var_type)) = (name,var_type)
    in
	    Iterator ("forAll", map strip_var variables,
    		        source, type_of source,
		            body, type_of body,
		            Bag (type_of body))
    end

fun ocl_select (source:OclTerm) (Variable variable) (body:OclTerm) = 
    Iterator ("select", [variable],
							source, type_of source,
							body, type_of body,
							Set (type_of body))
    
fun ocl_exists (source:OclTerm) (Variable variable) (body:OclTerm) = 
    Iterator ("exists", [variable],
							source, type_of source,
							body, type_of body,
							Set (type_of body))
    
fun atpre exp = ocl_opcall exp ["oclLib","OclAny","atPre"] nil (type_of exp)

end


