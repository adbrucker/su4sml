(*****************************************************************************
 *          su4sml - a shallow embedding of OCL in Isabelle/HOL              
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

signature MDR_CORE = 
sig
type operation = { name          : string,	
		   precondition  : (string option * ocl_term.OclTerm) list,
		   postcondition : (string option * ocl_term.OclTerm) list,
		   arguments     : (string * ocl_type.OclType) list,
		   result        : ocl_type.OclType,
		   isQuery       : bool
				   }     

type associationend = {name : string,
		       aend_type : ocl_type.OclType,	
		       multiplicity: (int * int) list,
		       ordered: bool	
				     }	       
	     

datatype Classifier =  
	 Class of 
	 { name        : ocl_type.Path, 
	   parent      : ocl_type.Path option,
	   attributes  : (string * ocl_type.OclType) list,
	   operations  : operation list,
	   associationends : associationend list,
	   invariant   : (string option * ocl_term.OclTerm) list,
	   stereotypes : string list,
	   interfaces  : ocl_type.Path list,
	   thyname     : string option
	  }
       | Interface of               (* not supported yet *)
	 { name        : ocl_type.Path,
	   parents     : ocl_type.Path list, 
	   operations  : operation list,
	   stereotypes : string list,
	   invariant   : (string option * ocl_term.OclTerm) list,
	   thyname     : string option
	  }
       | Enumeration of (* not really supported yet? *)
	 { name        : ocl_type.Path,
	   parent      : ocl_type.Path option,
	   operations  : operation list,
	   literals    : string list,
	   invariant   : (string option * ocl_term.OclTerm) list,
	   stereotypes : string list,
	   interfaces  : ocl_type.Path list,
	   thyname     : string option
	  }
       | Primitive of (* not really supported yet *)
	 { name        : ocl_type.Path,
	   parent      : ocl_type.Path option,
	   operations  : operation list,
	   associationends : associationend list,
	   invariant   : (string option * ocl_term.OclTerm) list,
	   stereotypes : string list,
	   interfaces  : ocl_type.Path list,
	   thyname     : string option
	  }

val OclAnyC : Classifier

val normalize : Classifier -> Classifier

val name_of       : Classifier -> ocl_type.Path 
val package_of    : Classifier -> ocl_type.Path
val short_name_of : Classifier -> string 

val parent_name_of       : Classifier -> ocl_type.Path 
val parent_package_of    : Classifier -> ocl_type.Path 
val short_parent_name_of : Classifier -> string 

val thy_name_of       : Classifier -> string
val update_thyname    : string -> Classifier -> Classifier
val attributes_of     : Classifier -> (string * ocl_type.OclType) list
val operations_of     : Classifier -> operation list
val invariant_of      : Classifier -> (string option * ocl_term.OclTerm) list
val string_of_path    : string list -> string    

val arguments_of_op    : operation -> (string * ocl_type.OclType) list
val precondition_of_op  : operation -> (string option * ocl_term.OclTerm) list
val result_of_op  : operation -> ocl_type.OclType
val postcondition_of_op : operation -> (string option * ocl_term.OclTerm) list
val name_of_op          : operation -> string
val mangled_name_of_op          : operation -> string

end
