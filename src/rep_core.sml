(*****************************************************************************
 *          su4sml - a SecureUML repository for SML              
 *                                                                            
 * rep_core.sml - core repository datastructures for su4sml
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

(** Repository datatypes and helper functions for classifiers. *)
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
	 stereotypes: string list,
     init : Rep_OclTerm.OclTerm option
}

datatype Classifier =  
	 Class of 
	 { name        : Rep_OclType.OclType, 
	   parent      : Rep_OclType.OclType option,
	   attributes  : attribute list,
	   operations  : operation list,
	   associationends : associationend list,
	   invariant   : (string option * Rep_OclTerm.OclTerm) list,
	   stereotypes : string list,
	   interfaces  : Rep_OclType.OclType list,
	   thyname     : string option,
           activity_graphs : Rep_ActivityGraph.ActivityGraph list
	  }
       | Interface of               (* not supported yet *)
	 { name        : Rep_OclType.OclType,
	   parents     : Rep_OclType.OclType list, 
	   operations  : operation list,
	   stereotypes : string list,
	   invariant   : (string option * Rep_OclTerm.OclTerm) list,
	   thyname     : string option
	  }
       | Enumeration of (* not really supported yet? *)
	 { name        : Rep_OclType.OclType,
	   parent      : Rep_OclType.OclType option,
	   operations  : operation list,
	   literals    : string list,
	   invariant   : (string option * Rep_OclTerm.OclTerm) list,
	   stereotypes : string list,
	   interfaces  : Rep_OclType.OclType list,
	   thyname     : string option
	  }
       | Primitive of (* not really supported yet *)
	 { name        : Rep_OclType.OclType,
	   parent      : Rep_OclType.OclType option,
	   operations  : operation list,
	   associationends : associationend list,
	   invariant   : (string option * Rep_OclTerm.OclTerm) list,
	   stereotypes : string list,
	   interfaces  : Rep_OclType.OclType list,
	   thyname     : string option
	  }
       | Template of 
	 { parameter   : Rep_OclType.OclType,
	   classifier  : Classifier
	 }

val OclAnyC : Classifier

val normalize : Classifier -> Classifier
val normalize_init : Classifier -> Classifier

val name_of       : Classifier -> Rep_OclType.Path 
val type_of       : Classifier -> Rep_OclType.OclType 
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
val stereotypes_of    : Classifier -> string list
val string_of_path    : string list -> string    

val arguments_of_op     : operation -> (string * Rep_OclType.OclType) list
val precondition_of_op  : operation -> (string option * Rep_OclTerm.OclTerm) list
val result_of_op        : operation -> Rep_OclType.OclType
val postcondition_of_op : operation -> (string option * Rep_OclTerm.OclTerm) list
val name_of_op          : operation -> string
val mangled_name_of_op          : operation -> string

val class_of            : Rep_OclType.Path -> Classifier list -> Classifier
val parent_of           : Classifier -> Classifier list -> Classifier
val parents_of          : Classifier -> Classifier list -> Rep_OclType.Path list
val operation_of        : Classifier list -> Rep_OclType.Path -> operation option
val topsort_cl          : Classifier list -> Classifier list
end

structure Rep_Core :  REP_CORE = 
struct
open library
open Rep_OclType

type Visibility = XMI_DataTypes.VisibilityKind
type Scope      = XMI_DataTypes.ScopeKind

type operation = { name          : string,	
		   precondition  : (string option * Rep_OclTerm.OclTerm) list,
		   postcondition : (string option * Rep_OclTerm.OclTerm) list,
		   arguments     : (string * Rep_OclType.OclType) list,
		   result        : Rep_OclType.OclType,
		   isQuery       : bool,
		   visibility    : Visibility,
                   scope         : Scope }     

type associationend = {
     name : string,
     aend_type: Rep_OclType.OclType,
     multiplicity: (int*int) list,
     visibility: Visibility,
     ordered: bool,
     init : Rep_OclTerm.OclTerm option
}

type attribute = {
     name : string,
     attr_type : Rep_OclType.OclType,
     visibility : Visibility,
     scope: Scope,
	 stereotypes: string list,
     init : Rep_OclTerm.OclTerm option
}




datatype Classifier =  
	 Class of 
	 { name        : Rep_OclType.OclType, 
	   parent      : Rep_OclType.OclType option,
	   attributes  : attribute list,
	   operations  : operation list,
	   associationends : associationend list,
	   invariant   : (string option * Rep_OclTerm.OclTerm) list,
	   stereotypes : string list,
	   interfaces  : Rep_OclType.OclType list,
	   thyname     : string option,
           activity_graphs : Rep_ActivityGraph.ActivityGraph list
	  }
       | Interface of               (* not supported yet *)
	 { name        : Rep_OclType.OclType,
	   parents     : Rep_OclType.OclType list, 
	   operations  : operation list,
	   stereotypes : string list,
	   invariant   : (string option * Rep_OclTerm.OclTerm) list,
	   thyname     : string option
	  }
       | Enumeration of (* not really supported yet? *)
	 { name        : Rep_OclType.OclType,
	   parent      : Rep_OclType.OclType option,
	   operations  : operation list,
	   literals    : string list,
	   invariant   : (string option * Rep_OclTerm.OclTerm) list,
	   stereotypes : string list,
	   interfaces  : Rep_OclType.OclType list,
	   thyname     : string option
	  }
       | Primitive of (* not really supported yet *)
	 { name        : Rep_OclType.OclType,
	   parent      : Rep_OclType.OclType option,
	   operations  : operation list,
	   associationends : associationend list,
	   invariant   : (string option * Rep_OclTerm.OclTerm) list,
	   stereotypes : string list,
	   interfaces  : Rep_OclType.OclType list,
	   thyname     : string option
	  } 
       | Template of 
	 { parameter   : Rep_OclType.OclType,
	   classifier  : Classifier
	 }

(* convert an association end into the corresponding collection type *)
fun assoc_to_attr_type {name,aend_type,multiplicity,ordered,visibility,init} =
    case multiplicity of 
	[(0,1)] => aend_type
      | [(1,1)] => aend_type
      | _ =>if ordered then Rep_OclType.Sequence aend_type (* OrderedSet? *)
	    else Rep_OclType.Set aend_type
    
(* convert an association end into an attribute of the *)
(* corresponding collection type                       *)
fun assoc_to_attr (assoc:associationend) = {name = #name assoc,
					    attr_type = assoc_to_attr_type assoc,
					    visibility = #visibility assoc,
					    scope = XMI.InstanceScope,
						stereotypes = nil,
					    init = #init assoc}

(* convert a multiplicity range into an invariant of the form *)
(* size > lowerBound and size < upperBound )                 *)
fun range_to_inv cls_name aend (a,b) = 
    let val cls       = Rep_OclType.Classifier cls_name
	val attr_type = assoc_to_attr_type aend
	val attr_name = cls_name@[#name aend]
	val literal_a = Rep_OclTerm.Literal (Int.toString a, Rep_OclType.Integer)
	val literal_b = Rep_OclTerm.Literal (Int.toString b, Rep_OclType.Integer)
	val self      = Rep_OclTerm.Variable ("self",cls)
	val attribute = Rep_OclTerm.AttributeCall (self,cls,attr_name,attr_type)
	val attribute_size = 
	    Rep_OclTerm.OperationCall (attribute,attr_type,
				    ["oclLib","Collection","size"],[],
				    Rep_OclType.Integer)
	val lower_bound = 
	    Rep_OclTerm.OperationCall (attribute_size,Rep_OclType.Integer,
				    ["oclLib","Real",">="],
				    [(literal_a,Rep_OclType.Integer)],Rep_OclType.Boolean)
	val upper_bound = 
	    Rep_OclTerm.OperationCall (attribute_size,Rep_OclType.Integer,
				    ["oclLib","Real","<="],
				    [(literal_b,Rep_OclType.Integer)],Rep_OclType.Boolean)
	val equal = 
	    Rep_OclTerm.OperationCall (attribute_size,Rep_OclType.Integer,
				    ["oclLib","OclAny","="],
				    [(literal_a,Rep_OclType.Integer)],Rep_OclType.Boolean)
    in
	if a = b then equal
	else if b = ~1 then lower_bound
	else Rep_OclTerm.OperationCall (lower_bound,Rep_OclType.Boolean,
				     ["oclLib","Boolean","and"],
				     [(upper_bound,Rep_OclType.Boolean)],
				     Rep_OclType.Boolean)
    end




(* calculate the invariants of an association end:               *)
(* 1. multiplicity constraints                                   *)
(* 2. consistency constraints between opposing association ends  *)
(*    i.e., A.b.a->includes(A)                                   *)
(*    FIXME: 2. is not implemented yet...                        *)
fun assoc_to_inv cls_name (aend:associationend) =
    let val inv_name = "multconstraint_for_aend_"^(#name aend)
	val range_constraints = case (#multiplicity aend) of
				    [(0,1)] => []
				  | [(1,1)] => let
					val attr_name = cls_name@[#name aend]
					val attr_type = assoc_to_attr_type aend
					val cls       = Rep_OclType.Classifier cls_name
					val self      = Rep_OclTerm.Variable ("self",cls)
					val attribute = Rep_OclTerm.AttributeCall (self,cls,attr_name,attr_type)
				    in
					[Rep_OclTerm.OperationCall (attribute,attr_type,
								    ["oclIsDefined"],[],
								    Rep_OclType.Boolean)]
				    end
				  | _ =>  map (range_to_inv cls_name aend) 
					      (#multiplicity aend)
	fun ocl_or (x,y) = 
	    Rep_OclTerm.OperationCall (x,Rep_OclType.Boolean,
				    ["oclLib","Boolean","or"],
				    [(y,Rep_OclType.Boolean)],Rep_OclType.Boolean)
    in if range_constraints = [] 
       then (SOME inv_name, Rep_OclTerm.Literal ("true",Rep_OclType.Boolean)) 
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
		      invariant = append (map (assoc_to_inv (path_of_OclType name)) associationends)  
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


fun rm_init_attr (attr:attribute) = {
    name = #name attr,
    attr_type = #attr_type attr,
    visibility = #visibility attr,
    scope = #scope attr,
	stereotypes = #stereotypes attr,
    init = NONE
}:attribute


fun init_to_inv cls_name (attr:attribute) = 
    case (#init attr) of
	NONE => (SOME ("init_"^(#name attr)),
		      Rep_OclTerm.Literal ("true",Rep_OclType.Boolean))
      | SOME(init) => let
	    val attr_name = cls_name@[#name attr]
	    val attr_type = #attr_type attr
	    val cls       = Rep_OclType.Classifier cls_name
	    val self      = Rep_OclTerm.Variable ("self",cls)
	    val attribute = Rep_OclTerm.AttributeCall (self,cls,attr_name,attr_type)
	in	
            (SOME ("init_"^(#name attr)),
	     Rep_OclTerm.OperationCall
		 (Rep_OclTerm.OperationCall
		      (self,cls,
		       ["oclLib","OclAny","oclIsNew"],[],Rep_OclType.Boolean),Rep_OclType.Boolean,
		  ["oclLib","Boolean","implies"],
		  [(Rep_OclTerm.OperationCall (attribute,
			 attr_type,["oclLib","OclAny","="],
			 [(init,attr_type)],Rep_OclType.Boolean),Rep_OclType.Boolean)],
		  Rep_OclType.Boolean)
	    )
		
	end


fun normalize_init (Class {name,parent,attributes,operations,associationends,invariant,
		      stereotypes,interfaces,thyname,activity_graphs}) =
	       Class {name   = name,
		      parent = parent,
		      attributes = (map rm_init_attr attributes),
		      operations = operations,
		      associationends = nil,
		      invariant = append (map (init_to_inv  (path_of_OclType name)) attributes)  
					  invariant,
		      stereotypes = stereotypes,
                      interfaces = interfaces,
		      thyname = thyname,
                      activity_graphs=activity_graphs}
  | normalize_init c = c





val OclAnyC = Class{name=Rep_OclType.OclAny,parent=NONE,attributes=[],
			  operations=[], interfaces=[],
			  invariant=[],stereotypes=[], associationends=[],
			  thyname=NONE,
                          activity_graphs=nil}

		   
fun string_of_path (path:Rep_OclType.Path) = case path of
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
      
      
      
      
fun type_of (Class{name,...})       = name  
  | type_of (Interface{name,...})   = name
  | type_of (Enumeration{name,...}) = name
  | type_of (Primitive{name,...})   = name
  | type_of (Template{classifier,...}) = type_of classifier 


fun name_of (Class{name,...})       = path_of_OclType name  
  | name_of (Interface{name,...})   = path_of_OclType name
  | name_of (Enumeration{name,...}) = path_of_OclType name
  | name_of (Primitive{name,...})   = path_of_OclType name
  | name_of _                       = error "in Rep.name_of: Classifier has no name represenation"

fun short_name_of C =  case (name_of C)  of
	[] => error "in Rep.short_name_of: empty type"
	| p => (hd o rev)  p

fun stereotypes_of (Class{stereotypes,...})       = stereotypes  
  | stereotypes_of (Interface{stereotypes,...})   = stereotypes
  | stereotypes_of (Enumeration{stereotypes,...}) = stereotypes
  | stereotypes_of (Primitive{stereotypes,...})    = stereotypes
  | stereotypes_of (Template _) = error "in Rep.stereotypes_of: \
                                        \unsupported argument type Template"



fun package_of (Class{name,...})       = if (length (path_of_OclType name)) > 1 
                                         then take (((length (path_of_OclType name)) -1),
                                                    (path_of_OclType name))  
                                         else []
  | package_of (Interface{name,...})   = if (length (path_of_OclType name)) > 1 
                                         then take (((length (path_of_OclType name)) -1),
                                                    (path_of_OclType name)) 
                                         else []
  | package_of (Enumeration{name,...}) = if (length (path_of_OclType name)) > 1 
                                         then take (((length (path_of_OclType name)) -1),
                                                    (path_of_OclType name))
                                         else []
  | package_of (Primitive{name,...})   = if (length (path_of_OclType name)) > 1 
                                         then take (((length (path_of_OclType name)) -1),
                                                    (path_of_OclType name)) 
                                         else []
  | package_of (Template{classifier,...}) = package_of classifier

fun parent_name_of (C as Class{parent,...}) = 
    (case parent  of NONE   => name_of OclAnyC
		   | SOME p => path_of_OclType p ) 
  | parent_name_of (Interface{...})         =  error "in Rep.parent_name_of: \
                                                     \unsupported argument type Interface"
  | parent_name_of (E as Enumeration{parent,...}) = 
    (case parent  of NONE => error ("in Rep.parent_name_of: Enumeration "^
                                    ((string_of_path o name_of) E)
                                    ^" has no parent")
		   | SOME p  => path_of_OclType p )  
  | parent_name_of (D as Primitive{parent,...})    = 
    (case parent  of NONE => name_of OclAnyC
	           (* error ("Primitive "^((string_of_path o name_of) D)^" has no parent") *)
		   | SOME p  => path_of_OclType p )
  | parent_name_of (Template _) = error "in Rep.parent_name_of: \
                                        \unsupported argument type Template"

fun short_parent_name_of C =  case (parent_name_of C) of
	                          [] => error "in Rep.short_parent_name_of: empty type"
                                | p => (hd o rev)  p
				       
fun parent_package_of (Class{parent,...})       = 
    (case parent of  NONE => package_of OclAnyC
		   | SOME q   => let val p = path_of_OclType q in 
				     if (length p) > 1 
                                     then  (take (((length p) -1),p))  
                                     else []
				 end)
  | parent_package_of (Interface{...})        = 
    error "in Rep.parent_package_of: unsupported argument type Interface"
  | parent_package_of (E as Enumeration{parent,...}) = 
    (case parent of  NONE => error ("in Rep.parent_package_of: Enumeration "^
                                    (string_of_path o name_of) E^
                                    " has no parent")
		   | SOME q   => let val p = path_of_OclType q in 
				    if (length p) > 1 
                                    then (take (((length p) -1),p))  
                                    else []
				end )
  | parent_package_of (Primitive{parent,...})    = 
    (case parent of NONE => package_of OclAnyC
	  (* NONE => error "Primitive has no parent" *)
		 |  SOME q   => let val p = path_of_OclType q in
				   if (length p) > 1 
                                   then (take (((length p) -1),p))  
                                   else []
			       end)
  | parent_package_of (Template{...})        = 
    error "in Rep.parent_package_of: unsupported argument type Template"
						

fun attributes_of (Class{attributes,...}) = attributes
  | attributes_of (Interface{...})        = 
         error "in Rep.attributes_of: argument is Interface"
  | attributes_of (Enumeration{...})      = 
         error "in Rep.attributes_of: argument is Enumeration"  
  | attributes_of (Primitive{...})         = []  
         (* error "attributes_of <Primitive> not supported" *)  
  | attributes_of (Template{parameter,classifier}) = attributes_of classifier

fun operations_of (Class{operations,...})          = operations
  | operations_of (Interface{operations,...})      = operations
  | operations_of (Enumeration{operations,...})    = operations
  | operations_of (Primitive{operations,...})      = operations  
  | operations_of (Template{parameter,classifier}) = operations_of classifier

fun p_invariant_of (Class{invariant,...})       = invariant 
  | p_invariant_of (Interface{invariant,...})   = invariant
  | p_invariant_of (Enumeration{invariant,...}) = invariant
  | p_invariant_of (Primitive{invariant,...})   = invariant
  | p_invariant_of (Template _) = error "in Rep.p_invariant_of: \
                                        \unsupported argument type Template"

fun invariant_of C = case p_invariant_of C of  
			 [] => [(NONE, Rep_OclTerm.Literal ("true",Rep_OclType.Boolean))]
		       | il => il


fun precondition_of_op ({precondition,...}:operation) = case precondition  of  
			 [] => [(NONE, Rep_OclTerm.Literal ("true",Rep_OclType.Boolean))]
		       | il => il


fun postcondition_of_op ({postcondition, ...}:operation) = case postcondition  of  
			 [] => [(NONE, Rep_OclTerm.Literal ("true",Rep_OclType.Boolean))]
		       | il => il

fun name_of_op ({name,...}:operation) = name

fun mangled_name_of_op ({name,arguments,result,...}:operation) = 
    let
	val arg_typestrs = map (fn a => (Rep_OclType.string_of_OclType o #2 ) a ) arguments
    in 
	 foldr1 (fn (a,b) =>(a^"_"^b)) 
                ((name::arg_typestrs)@[Rep_OclType.string_of_OclType result])
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
  | thy_name_of (Template _) = error "in Rep.thy_name_of: \
                                     \unsupported argument type Template"
     






fun class_of name cl = hd (filter (fn a => if ((name_of a) = name)
                                           then true else false ) cl )
		       

fun parent_of  C cl =  (class_of (parent_name_of C) cl)

fun parents_of C cl = case parent_name_of C of
                          [] => []    
                        | class => (if( class = (name_of OclAnyC) )
                                   then [(name_of OclAnyC)]
                                   else [class]@(parents_of (class_of class cl) cl))
				  
fun operation_of cl fq_name = 
    let 
	val classname   = (rev o  tl o rev) fq_name
	val operations  = operations_of (class_of classname cl)
	val name        = (hd o rev) fq_name	
    in
	SOME(hd (filter (fn a => if ((name_of_op a) = name)
				 then true else false ) operations ))
    end	

(* topological sort of class lists *)
fun topsort_cl cl =
    let val OclAny_subcl = filter (fn a => (parent_name_of a) = (name_of OclAnyC)) cl	      
        fun subclasses_of cl c = filter (fn a =>   (parent_name_of a = (name_of c))) cl
        fun sub [] _ = []
	  | sub cl c =  c :: (foldl (op@) [] (map (fn a => sub cl a) 
						   (subclasses_of cl c)))  
    in
	foldl (op@) [] (map (fn a => sub cl a) (OclAny_subcl))  
    end

end
