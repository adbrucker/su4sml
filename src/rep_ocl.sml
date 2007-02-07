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

fun string_of_OclType Integer        = "Integer" 
  | string_of_OclType Real           = "Real"
  | string_of_OclType String         = "String"
  | string_of_OclType Boolean        = "Boolean"
  | string_of_OclType OclAny         = "OclAny"
  | string_of_OclType (Set t)	     = ("Set("^(string_of_OclType t)^")")			
  | string_of_OclType (Sequence t)   = ("Sequence("^(string_of_OclType t)^")")	
  | string_of_OclType (OrderedSet t) = ("OrderedSet("^(string_of_OclType t)^")")	
  | string_of_OclType (Bag t)        = ("Bag("^(string_of_OclType t)^")")	
  | string_of_OclType (Collection t) = ("Collection("^(string_of_OclType t)^")")	
  | string_of_OclType OclVoid        = "OclVoid"
  | string_of_OclType (Classifier p) = (string_of_path p)
  | string_of_OclType DummyT         = "DummyT"

fun path_of_OclType (Classifier p) = p
  | path_of_OclType (TemplateParameter p) = [] (* FIXME *)
  | path_of_OclType x = ["oclLib",string_of_OclType x]


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

