(*****************************************************************************
 *          su4sml - a SecureUML repository for SML              
 *                                                                            
 * ocl.sig - 
 * Copyright (C) 2001-2005  Achim D. Brucker <brucker@inf.ethz.ch>   
 *                          Burkhart Wolff   <bwolff@inf.ethz.ch>    
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

signature REP_OCL_TYPE =
sig
	
    type Path = string list

    datatype OclType    =  Integer | Real | String | Boolean | OclAny 
		     | Set of OclType | Sequence of OclType
		     | OrderedSet of OclType | Bag of OclType 
		     | Collection of OclType
		     | Classifier of Path | OclVoid | DummyT
	val string_of_OclType : OclType -> string	
	val string_of_path    : Path -> string	
	
end
     
    
signature REP_OCL_TERM =
sig
include REP_OCL_TYPE

datatype OclTerm = 
	 Literal            of string * OclType    (* Literal with type *)
       | If                 of OclTerm * OclType   (* condition   *)
			       * OclTerm * OclType (* then        *)
			       * OclTerm * OclType (* else        *)
			       * OclType           (* result type *)
       | AssociationEndCall of OclTerm * OclType (* source      *)
			       * Path            (* assoc.-enc  *)
			       * OclType         (* result type *)
       | AttributeCall      of OclTerm * OclType (* source      *)
			       * Path            (* attribute   *)
			       * OclType         (* result type *)
       | OperationCall      of OclTerm * OclType          (* source      *)
			       * Path                     (* operation   *)
			       * (OclTerm * OclType) list (* parameters  *)
			       * OclType                  (* result tupe *)
       | OperationWithType  of OclTerm * OclType  (* source         *)
			       * string * OclType (* type parameter *)
			       * OclType          (* result type    *)
       | Variable           of string * OclType    (* name with type *)
       | Let                of string * OclType    (* variable *)
			       * OclTerm * OclType (* rhs      *)
			       * OclTerm * OclType (* in       *)
       | Iterate            of (string * OclType) list      (* iterator variables *)
			       * string * OclType * OclTerm (* result variable    *)
			       * OclTerm * OclType          (* source             *)
			       * OclTerm * OclType          (* iterator body      *)
			       * OclType                    (* result type        *)
       | Iterator           of string                    (* name of iterator   *)
			       * (string * OclType) list (* iterator variables *)
			       * OclTerm * OclType       (* source             *)
			       * OclTerm * OclType       (* iterator-body      *)
			       * OclType                 (* result type        *)
end
