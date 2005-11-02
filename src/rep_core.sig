(*****************************************************************************
 *          su4sml - a SecureUML repository for SML              
 *                                                                            
 * mdr_core.sig - generic meta data repository import signature for su4sml
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

signature REP_CORE = 
sig
type Scope
type Visibility
type operation = { name          : string,	
		   precondition  : (string option * Rep_OclTerm.OclTerm) list,
		   postcondition : (string option * Rep_OclTerm.OclTerm) list,
		   arguments     : (string * Rep_OclType.OclType) list,
		   result        : Rep_OclType.OclType,
		   isQuery       : bool,
		   scope         : Scope,
		   visibility    : Visibility	
				   }     	

type associationend = {name : string,
		       aend_type : Rep_OclType.OclType,	
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
     init : Rep_OclTerm.OclTerm option
}

datatype Classifier =  
	 Class of 
	 { name        : Rep_OclType.Path, 
	   parent      : Rep_OclType.Path option,
	   attributes  : attribute list,
	   operations  : operation list,
	   associationends : associationend list,
	   invariant   : (string option * Rep_OclTerm.OclTerm) list,
	   stereotypes : string list,
	   interfaces  : Rep_OclType.Path list,
	   thyname     : string option,
           activity_graphs : Rep_ActivityGraph.ActivityGraph list
	  }
       | Interface of               (* not supported yet *)
	 { name        : Rep_OclType.Path,
	   parents     : Rep_OclType.Path list, 
	   operations  : operation list,
	   stereotypes : string list,
	   invariant   : (string option * Rep_OclTerm.OclTerm) list,
	   thyname     : string option
	  }
       | Enumeration of (* not really supported yet? *)
	 { name        : Rep_OclType.Path,
	   parent      : Rep_OclType.Path option,
	   operations  : operation list,
	   literals    : string list,
	   invariant   : (string option * Rep_OclTerm.OclTerm) list,
	   stereotypes : string list,
	   interfaces  : Rep_OclType.Path list,
	   thyname     : string option
	  }
       | Primitive of (* not really supported yet *)
	 { name        : Rep_OclType.Path,
	   parent      : Rep_OclType.Path option,
	   operations  : operation list,
	   associationends : associationend list,
	   invariant   : (string option * Rep_OclTerm.OclTerm) list,
	   stereotypes : string list,
	   interfaces  : Rep_OclType.Path list,
	   thyname     : string option
	  }

val OclAnyC : Classifier

val normalize : Classifier -> Classifier

val name_of       : Classifier -> Rep_OclType.Path 
val package_of    : Classifier -> Rep_OclType.Path
val short_name_of : Classifier -> string 

val parent_name_of       : Classifier -> Rep_OclType.Path 
val parent_package_of    : Classifier -> Rep_OclType.Path 
val short_parent_name_of : Classifier -> string 

val thy_name_of       : Classifier -> string
val update_thyname    : string -> Classifier -> Classifier
val attributes_of     : Classifier -> attribute list
val operations_of     : Classifier -> operation list
val invariant_of      : Classifier -> (string option * Rep_OclTerm.OclTerm) list
val string_of_path    : string list -> string    

val arguments_of_op    : operation -> (string * Rep_OclType.OclType) list
val precondition_of_op  : operation -> (string option * Rep_OclTerm.OclTerm) list
val result_of_op  : operation -> Rep_OclType.OclType
val postcondition_of_op : operation -> (string option * Rep_OclTerm.OclTerm) list
val name_of_op          : operation -> string
val mangled_name_of_op          : operation -> string

end
