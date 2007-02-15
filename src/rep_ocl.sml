(*****************************************************************************
 *          su4sml - a SecureUML repository for SML              
 *                                                                            
 * ocl.sig - 
 * Copyright (C) 2001-2005  Achim D. Brucker <brucker@inf.ethz.ch>   
 *                          Jürgen Doser <doserj@inf.ethz.ch>
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

(** Repository datatypes and helper functions for UML/OCL types. *)
signature REP_OCL_TYPE =
sig
    
    type Path = string list

    datatype OclType    =  Integer | Real | String | Boolean | OclAny 
		         | Set of OclType | Sequence of OclType
		         | OrderedSet of OclType | Bag of OclType 
		         | Collection of OclType
		         | Classifier of Path | OclVoid | DummyT | TemplateParameter of string

    val path_of_OclType   : OclType -> Path
    val string_of_OclType : OclType -> string	
    val string_of_path    : Path -> string	
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
	 Literal            of string * OclType              (* Literal with type  *)
       | CollectionLiteral  of CollectionPart list * OclType (* content with type  *)
       | If                 of OclTerm * OclType             (* condition          *)
			       * OclTerm * OclType           (* then               *)
			       * OclTerm * OclType           (* else               *)
			       * OclType                     (* result type        *)
       | AssociationEndCall of OclTerm * OclType             (* source             *)
			       * Path                        (* assoc.-enc         *)
			       * OclType                     (* result type        *)
       | AttributeCall      of OclTerm * OclType             (* source             *)
			       * Path                        (* attribute          *)
			       * OclType                     (* result type        *)
       | OperationCall      of OclTerm * OclType             (* source             *)
			       * Path                        (* operation          *)
			       * (OclTerm * OclType) list    (* parameters         *)
			       * OclType                     (* result tupe        *)
       | OperationWithType  of OclTerm * OclType             (* source             *)
			       * string * OclType            (* type parameter     *)
			       * OclType                     (* result type        *)
       | Variable           of string * OclType              (* name with type     *)
       | Let                of string * OclType              (* variable           *)
			       * OclTerm * OclType           (* rhs                *)
			       * OclTerm * OclType           (* in                 *)
       | Iterate            of (string * OclType) list       (* iterator variables *)
			       * string * OclType * OclTerm  (* result variable    *)
			       * OclTerm * OclType           (* source             *)
			       * OclTerm * OclType           (* iterator body      *)
			       * OclType                     (* result type        *)
       | Iterator           of string                        (* name of iterator   *)
			       * (string * OclType) list     (* iterator variables *)
			       * OclTerm * OclType           (* source             *)
			       * OclTerm * OclType           (* iterator-body      *)
			       * OclType                     (* result type        *)
     and CollectionPart = CollectionItem of OclTerm * OclType
	                | CollectionRange of OclTerm         (* first              *)
		         		     * OclTerm       (* last               *)
                                             * OclType
end


structure Rep_OclType : REP_OCL_TYPE =
struct
open library
type Path = string list
	    
	    
datatype OclType    =  Integer | Real | String | Boolean | OclAny 
		     | Set of OclType | Sequence of OclType
		     | OrderedSet of OclType | Bag of OclType 
		     | Collection of OclType | OclVoid | DummyT
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
  | string_of_OclType' f (Set t)	     = ("Set("^(string_of_OclType' f t)^")")			
  | string_of_OclType' f (Sequence t)   = ("Sequence("^(string_of_OclType' f t)^")")	
  | string_of_OclType' f (OrderedSet t) = ("OrderedSet("^(string_of_OclType' f t)^")")	
  | string_of_OclType' f (Bag t)        = ("Bag("^(string_of_OclType' f t)^")")	
  | string_of_OclType' f (Collection t) = ("Collection("^(string_of_OclType' f t)^")")	
  | string_of_OclType' f OclVoid        = "OclVoid"
  | string_of_OclType' f (Classifier p) = (path_to_string p f)
  | string_of_OclType' f DummyT         = "DummyT"

fun string_of_OclType t = string_of_OclType' "." t


fun path_of_OclType (Classifier p) = p
  | path_of_OclType (TemplateParameter p) = [] (* FIXME *)
  | path_of_OclType x = ["oclLib",string_of_OclType x]

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
	 Literal            of string * OclType              (* Literal with type  *)
       | CollectionLiteral  of CollectionPart list * OclType (* content with type  *)
       | If                 of OclTerm * OclType             (* condition          *)
			       * OclTerm * OclType           (* then               *)
			       * OclTerm * OclType           (* else               *)
			       * OclType                     (* result type        *)
       | AssociationEndCall of OclTerm * OclType             (* source             *)
			       * Path                        (* assoc.-enc         *)
			       * OclType                     (* result type        *)
       | AttributeCall      of OclTerm * OclType             (* source             *)
			       * Path                        (* attribute          *)
			       * OclType                     (* result type        *)
       | OperationCall      of OclTerm * OclType             (* source             *)
			       * Path                        (* operation          *)
			       * (OclTerm * OclType) list    (* parameters         *)
			       * OclType                     (* result tupe        *)
       | OperationWithType  of OclTerm * OclType             (* source             *)
			       * string * OclType            (* type parameter     *)
			       * OclType                     (* result type        *)
       | Variable           of string * OclType              (* name with type     *)
       | Let                of string * OclType              (* variable           *)
			       * OclTerm * OclType           (* rhs                *)
			       * OclTerm * OclType           (* in                 *)
       | Iterate            of (string * OclType) list       (* iterator variables *)
			       * string * OclType * OclTerm  (* result variable    *)
			       * OclTerm * OclType           (* source             *)
			       * OclTerm * OclType           (* iterator body      *)
			       * OclType                     (* result type        *)
       | Iterator           of string                        (* name of iterator   *)
			       * (string * OclType) list     (* iterator variables *)
			       * OclTerm * OclType           (* source             *)
			       * OclTerm * OclType           (* iterator-body      *)
			       * OclType                     (* result type        *)
     and CollectionPart = CollectionItem of OclTerm * OclType
	                | CollectionRange of OclTerm         (* first              *)
		         		     * OclTerm       (* last               *)
                                             * OclType

end


structure Rep_OclHelper =
struct
open Rep_OclTerm
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

fun self t = Variable ("self",t)
fun result t = Variable ("result", t)


fun ocl_let var rhs body = Let (var,type_of rhs,rhs,type_of rhs,body,type_of body)
fun ocl_opcall source f args t  = OperationCall (source, type_of source, f,
                                                 map (fn x => (x,type_of x)) args,
                                                 t)
fun ocl_attcall source att t    = AttributeCall (source, type_of source, att, t)
fun ocl_aendcall source aend t  = AssociationEndCall (source, type_of source, aend,
                                                      t)
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

(* Integer: -,+,-,*,/,abs,div,mod,max,min               *)
(* Real   : -,+,-,*,/,abs,floor,round,max,min,<,>,<=,>= *)
(* String : size, concat, substring, toInteger, toReal  *)

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
fun ocl_excludes a b = ocl_opcall a ["oclLib", "Collection", "excludes"] [b] Boolean

fun ocl_modifiedOnly a = ocl_opcall a ["oclLib", "Set", "modifiedOnly"] [] Boolean

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
                                  
fun atpre exp = ocl_opcall exp ["oclLib","OclAny","atPre"] nil (type_of exp)

end


