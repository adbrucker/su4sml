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

structure Rep_OclType : REP_OCL_TYPE =
struct
open library
type Path = string list
	    
	    
datatype OclType    =  Integer | Real | String | Boolean | OclAny 
		     | Set of OclType | Sequence of OclType
		     | OrderedSet of OclType | Bag of OclType 
		     | Collection of OclType | OclVoid | DummyT
		     | Classifier of Path

fun string_of_path (path:Path) = case path of
			      [] => ""
			    | p  => foldr1 (fn (a,b) => a^"."^b) p


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

end
     
    
structure Rep_OclTerm : REP_OCL_TERM =
struct
open Rep_OclType

datatype OclTerm = 
	 Literal of string * OclType
       | If of  OclTerm * OclType 
		 * OclTerm * OclType 
		 * OclTerm * OclType * OclType 
       | AssociationEndCall of OclTerm * OclType * Path * OclType
       | AttributeCall      of OclTerm * OclType * Path * OclType
       | OperationCall         of OclTerm * OclType * Path 
				* (OclTerm * OclType) list * OclType 
       | OperationWithType of OclTerm * OclType * string * OclType * OclType 
       | Variable           of string * OclType
       | Let of string * OclType * OclTerm * OclType * OclTerm * OclType
       | Iterate of (string * OclType) list * string * OclType 
		     * OclTerm * OclType * OclTerm * OclType * OclType
       | Iterator of string * (string * OclType) list
		      * OclTerm * OclType * OclTerm * OclType * OclType
end

