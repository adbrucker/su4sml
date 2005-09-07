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

structure mdr_core :  MDR_CORE = 
struct
type operation = { name          : string,	
		   precondition  : (string option * ocl_term.OclTerm) list,
		   postcondition : (string option * ocl_term.OclTerm) list,
		   arguments     : (string * ocl_type.OclType) list,
		   result        : ocl_type.OclType,
		   isQuery       : bool }     

type associationend = {name : string,
		       aend_type: ocl_type.OclType,
		       multiplicity: (int*int) list,
		       ordered: bool }

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
	   thyname     : string option,
           activity_graphs : Rep_ActivityGraph.ActivityGraph list
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
 
(* convert an association end into the corresponding collection type *)
fun assoc_to_attr_type {name,aend_type,multiplicity,ordered} =
    if ordered then ocl_type.Sequence aend_type (* OrderedSet? *)
    else ocl_type.Set aend_type
    
(* convert an association end into an attribute of the *)
(* corresponding collection type                       *)
fun assoc_to_attr (assoc:associationend) = (#name assoc,assoc_to_attr_type assoc)

(* convert a multiplicity range into an invariant of the form *)
(* size > lowerBound and size < upperBound )                 *)
fun range_to_inv cls_name aend (a,b) = 
    let val cls       = ocl_type.Classifier cls_name
	val attr_type = assoc_to_attr_type aend
	val attr_name = cls_name@[#name aend]
	val literal_a = ocl_term.Literal (Int.toString a, ocl_type.Integer)
	val literal_b = ocl_term.Literal (Int.toString b, ocl_type.Integer)
	val self      = ocl_term.Variable ("self",cls)
	val attribute = ocl_term.AttributeCall (self,cls,attr_name,attr_type)
	val attribute_size = 
	    ocl_term.OperationCall (attribute,attr_type,
				    ["oclLib","Collection","size"],[],
				    ocl_type.Integer)
	val lower_bound = 
	    ocl_term.OperationCall (attribute_size,ocl_type.Integer,
				    ["oclLib","Real",">="],
				    [(literal_a,ocl_type.Integer)],ocl_type.Boolean)
	val upper_bound = 
	    ocl_term.OperationCall (attribute_size,ocl_type.Integer,
				    ["oclLib","Real","<="],
				    [(literal_b,ocl_type.Integer)],ocl_type.Boolean)
	val equal = 
	    ocl_term.OperationCall (attribute_size,ocl_type.Integer,
				    ["oclLib","OclAny","="],
				    [(literal_a,ocl_type.Integer)],ocl_type.Boolean)
    in
	if a = b then equal
	else if b = ~1 then lower_bound
	else ocl_term.OperationCall (lower_bound,ocl_type.Boolean,
				     ["oclLib","Boolean","and"],
				     [(upper_bound,ocl_type.Boolean)],
				     ocl_type.Boolean)
    end

(* calculate the invariants of an association end:               *)
(* 1. multiplicity constraints                                   *)
(* 2. consistency constraints between opposing association ends  *)
(*    i.e., A.b.a->includes(A)                                   *)
(*    FIXME: 2. is not implemented yet...                        *)
fun assoc_to_inv cls_name (aend:associationend) =
    let val inv_name = "multiplicity_constraint_for_association_end_"^(#name aend)
	val range_constraints = map (range_to_inv cls_name aend) 
				    (#multiplicity aend)
	fun ocl_or (x,y) = 
	    ocl_term.OperationCall (x,ocl_type.Boolean,
				    ["oclLib","Boolean","or"],
				    [(y,ocl_type.Boolean)],ocl_type.Boolean)
    in if range_constraints = [] 
       then (SOME inv_name, ocl_term.Literal ("true",ocl_type.Boolean)) 
       else (SOME inv_name, foldr1 ocl_or range_constraints)
    end
	     
(* convert association ends into attributes + invariants *)
fun normalize (Class {name,parent,attributes,operations,associationends,invariant,
		      stereotypes,interfaces,thyname,activity_graphs}) =
	       Class {name   = name,
		      parent = parent,
		      attributes = (append (map assoc_to_attr associationends) 
					   attributes),
		      operations = operations,
		      associationends = nil,
		      invariant = append (map (assoc_to_inv name) associationends)  
					  invariant,
		      stereotypes = stereotypes,
                      interfaces = interfaces,
		      thyname = thyname,
                      activity_graphs=activity_graphs}
  | normalize (Primitive p) =
    (* Primitive's do not have attributes, so we have to convert *)
    (* them into Classes...                                      *)
    if (#associationends p) = [] 
    then Primitive p 
    else normalize (Class {name = #name p, parent = #parent p, attributes=[],
			   operations = #operations p, invariant = #invariant p,
			   associationends = #associationends p,
			   stereotypes = #stereotypes p,
			   interfaces = #interfaces p,
			   thyname = #thyname p,
                           activity_graphs=nil})
  | normalize c = c

val OclAnyC = Class{name=["OclAny"],parent=NONE,attributes=[],
			  operations=[], interfaces=[],
			  invariant=[],stereotypes=[], associationends=[],
			  thyname=NONE,
                          activity_graphs=nil}

		   
fun string_of_path (path:ocl_type.Path) = case path of
			      [] => ""
			    | p  => foldr1 (fn (a,b) => a^"."^b) p



fun update_thyname tname (Class{name,parent,attributes,operations,invariant,
                                stereotypes,interfaces,associationends,activity_graphs,...})
    = Class{name=name,parent=parent,attributes=attributes,operations=operations,
            associationends=associationends,invariant=invariant,stereotypes=stereotypes,
            interfaces=interfaces,thyname=(SOME tname),activity_graphs=activity_graphs }
  | update_thyname tname (Interface{name,parents,operations,stereotypes,invariant,...}) 
    = Interface{name=name,parents=parents,operations=operations,stereotypes=stereotypes,
                invariant=invariant,thyname=(SOME tname)} 
  | update_thyname tname (Enumeration{name,parent,operations,literals,invariant,
                                      stereotypes,interfaces,...}) 
    = Enumeration{name=name,parent=parent,operations=operations,literals=literals,
                  invariant=invariant,stereotypes=stereotypes,interfaces=interfaces,
                  thyname=(SOME tname)}
  | update_thyname tname (Primitive{name,parent,operations,associationends,invariant,
                                    stereotypes,interfaces,...}) 
    = Primitive{name=name,parent=parent,operations=operations,
                associationends=associationends,invariant=invariant,
                stereotypes=stereotypes,interfaces=interfaces,thyname=(SOME tname)} 


fun name_of (Class{name,...})       = name  
  | name_of (Interface{name,...})   = name
  | name_of (Enumeration{name,...}) = name
  | name_of (Primitive{name,...})    = name

fun short_name_of (Class{name,...})       = (hd o rev) name  
  | short_name_of (Interface{name,...})   = (hd o rev) name
  | short_name_of (Enumeration{name,...}) = (hd o rev) name
  | short_name_of (Primitive{name,...})    = (hd o rev) name



fun package_of (Class{name,...})       = if (length name) > 1 
                                         then take (((length name) -1),name)  
                                         else []
  | package_of (Interface{name,...})   = if (length name) > 1 
                                         then take (((length name) -1),name) 
                                         else []
  | package_of (Enumeration{name,...}) = if (length name) > 1 
                                         then take (((length name) -1),name) 
                                         else []
  | package_of (Primitive{name,...})   = if (length name) > 1 
                                         then take (((length name) -1),name) 
                                         else []

fun parent_name_of (C as Class{parent,...}) = 
    (case parent  of NONE   => name_of OclAnyC
		    |SOME p => p ) 
  | parent_name_of (Interface{...})         = 
                    error "parent_name_of <Interface> not supported"
  | parent_name_of (E as Enumeration{parent,...}) = 
    (case parent  of NONE => error ("Enumeration "^((string_of_path o name_of) E)
                                    ^" has no parent")
		   | SOME p  => p )  
  | parent_name_of (D as Primitive{parent,...})    = 
    (case parent  of NONE => name_of OclAnyC
	(* error ("Primitive "^((string_of_path o name_of) D)^" has no parent") *)
		   | SOME p  => p )
 
fun short_parent_name_of (C as Class{parent,...})       = 
    (case parent  of NONE => short_name_of OclAnyC
		   | SOME p  => (hd o rev) p ) 
  | short_parent_name_of (Interface{...})        = 
    error "parent_name_of <Interface> not supported"
  | short_parent_name_of (E as Enumeration{parent,...}) = 
    (case parent  of  NONE => error ("Enumeration "^((string_of_path o name_of) E)^
                                     " has no parent")
		   |  SOME p  => (hd o rev) p )  
  | short_parent_name_of (D as Primitive{parent,...})    = 
    (case parent  of  NONE =>  short_name_of OclAnyC 
          (* error ("Primitive "^((string_of_path o name_of) D)^" has no parent") *)
		   | SOME p  => (hd o rev) p ) 
					     					     
							 
fun parent_package_of (Class{parent,...})       = 
    (case parent of  NONE => package_of OclAnyC
		   | SOME p   =>if (length p) > 1 
                                then  (take (((length p) -1),p))  
                                else [])
  | parent_package_of (Interface{...})        = 
                   error "parent_package_of <Interface> not supported"
  | parent_package_of (Enumeration{parent,...}) = 
    (case parent of  NONE => error "Enumeration has no parent"
		   | SOME p  => if (length p) > 1 
                                then (take (((length p) -1),p))  
                                else [])
  | parent_package_of (Primitive{parent,...})    = 
    (case parent of NONE => package_of OclAnyC
	  (* NONE => error "Primitive has no parent" *)
		 |  SOME p   => if (length p) > 1 
                                then (take (((length p) -1),p))  
                                else [])
						

fun attributes_of (Class{attributes,...}) = attributes
  | attributes_of (Interface{...})        = 
         error "attributes_of <Interface> not supported" 
  | attributes_of (Enumeration{...})      = 
         error "attributes_of <Enumeration> not supported"  
  | attributes_of (Primitive{...})         = []  
         (* error "attributes_of <Primitive> not supported" *)  

fun operations_of (Class{operations,...}) = operations
  | operations_of (Interface{...})        = 
         error "operations_of <Interface> not supported" 
  | operations_of (Enumeration{...})      = 
         error "operations_of <Enumeration> not supported"  
  | operations_of (Primitive{...})         = []  
         (* error "operations_of <Primitive> not supported" *)  


fun p_invariant_of (Class{invariant,...})       = invariant 
  | p_invariant_of (Interface{invariant,...})   = invariant
  | p_invariant_of (Enumeration{invariant,...}) = invariant
  | p_invariant_of (Primitive{invariant,...})    = invariant

fun invariant_of C = case p_invariant_of C of  
			 [] => [(NONE, ocl_term.Literal ("true",ocl_type.Boolean))]
		       | il => il


fun precondition_of_op ({precondition,...}:operation) = case precondition  of  
			 [] => [(NONE, ocl_term.Literal ("true",ocl_type.Boolean))]
		       | il => il


fun postcondition_of_op ({postcondition, ...}:operation) = case postcondition  of  
			 [] => [(NONE, ocl_term.Literal ("true",ocl_type.Boolean))]
		       | il => il

fun name_of_op ({name,...}:operation) = name

fun mangled_name_of_op ({name,arguments,result,...}:operation) = 
    let
	val arg_typestrs = map (fn a => (ocl_type.string_of_OclType o snd ) a ) arguments
    in 
	 foldr1 (fn (a,b) =>(a^"_"^b)) 
                ((name::arg_typestrs)@[ocl_type.string_of_OclType result])
    end
							   
fun result_of_op ({result,...}:operation) = result

fun arguments_of_op ({arguments,...}:operation) = arguments





fun thy_name_of (C as Class{thyname,...})       = 
     (case thyname of  SOME tname =>  tname
		     | NONE => error  ("Class "^((string_of_path o name_of) C)^
                                       " has no thyname"))
  | thy_name_of (I as Interface{thyname,...})   = 
     (case thyname of SOME tname =>  tname
		     | NONE => error  ("Interface "^((string_of_path o name_of) I)
                                       ^" has no thyname"))
  | thy_name_of (E as Enumeration{thyname,...}) = 
      (case thyname of SOME tname =>  tname
		     | NONE => error  ("Enumeration "^((string_of_path o name_of) E)
                                       ^" has no thyname"))
  | thy_name_of (P as Primitive{thyname,...})    = 
      (case thyname of SOME tname =>  tname
		     | NONE => error  ("Primitive "^((string_of_path o name_of) P)^
                                       " has no thyname"))
 
end
