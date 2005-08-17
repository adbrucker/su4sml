(*****************************************************************************
 *          su4sml - a shallow embedding of OCL in Isabelle/HOL              
 *                                                                            
 * xmi_parser.sml - an xmi-parser for the import interface for su4sml
 * Copyright (C) 2005  Achim D. Brucker <brucker@inf.ethz.ch>
 *                     Jürgen Doser <doserj@inf.ethz.ch>
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



structure Xmi2Mdr =
struct
exception IllFormed
open ocl_type;

datatype HashTableEntry = Package of ocl_type.Path
		        | Type of (ocl_type.OclType * (mdr_core.associationend list)) (* maps xmi.idref to OclType and list of association ends *)
                        | Generalization of (string * string) 
			| Constraint of XMI_OCL.OCLConstraint 
		        | Stereotype of string
		        | Variable of XMI_OCL.VariableDeclaration 
			| Attribute of ocl_type.Path
			| Operation of ocl_type.Path
                        

fun find_generalization t xmiid = 
    (case valOf (HashTable.find t xmiid) 
      of Generalization x => x)
    handle Option => error ("expected Generalization "^xmiid^" in table")

fun find_stereotype t xmiid =
    (case valOf (HashTable.find t xmiid) 
      of Stereotype x => x)
    handle Option => error ("expected Stereotype "^xmiid^" in table")

fun find_attribute t xmiid =
    (case valOf (HashTable.find t xmiid) 
      of Attribute x => x)
    handle Option => error ("expected Attribute "^xmiid^" in table")

fun find_operation t xmiid =
    (case valOf (HashTable.find t xmiid) 
      of Operation x => x)
    handle Option => error ("expected Operation "^xmiid^" in table")

fun find_aends t xmiid = 
    (case valOf (HashTable.find t xmiid) 
      of (Type (c,xs))  => xs)
    handle Option => error ("expected Type "^xmiid^" in table (aends)")


fun find_variable_dec t xmiid =
    (case valOf (HashTable.find t xmiid) 
      of Variable x => x)
    handle Option => error ("expected VariableDeclaration "^xmiid^" in table")

fun find_parent t xmiid = #2 (find_generalization t xmiid)

fun find_package t xmiid  = 
    (case valOf (HashTable.find t xmiid) 
      of Package path => path)
    handle Option => error ("expected Path "^xmiid^" in table")
					
fun path_of_classifier (ocl_type.Classifier x) = x

fun find_constraint t xmiid =
    (case valOf (HashTable.find t xmiid) 
      of Constraint c => c)
    handle Option => error ("expected Constraint "^xmiid^" in table")
		

fun filter_precondition t  cs 
  = filter (fn x => let val constraint = find_constraint t x
			val name = #name constraint
			val constr_type_ref = #constraint_type constraint
			val constr_type_name = find_stereotype t constr_type_ref
		    in 
			constr_type_name = "pre"
		    end) cs

fun filter_postcondition t cs
  = filter (fn x => let val constraint = find_constraint t x
			val name = #name constraint
			val constr_type_ref = #constraint_type constraint
			val constr_type_name = find_stereotype t constr_type_ref
		    in 
			constr_type_name = "post"
		    end) cs

			
fun find_classifier_type t xmiid
  = let val ocltype = case valOf (HashTable.find t xmiid) of (Type (x,xs)) => x
    in 
	case ocltype of ocl_type.Integer      => ocltype
		      | ocl_type.String       => ocltype
		      | ocl_type.Real         => ocltype
		      | ocl_type.Boolean      => ocltype
		      | ocl_type.Classifier x => ocltype
		      | ocl_type.OclVoid      => ocltype
		      | ocl_type.OclAny       => ocltype
		      | ocl_type.Collection (ocl_type.Classifier [x]) => ocl_type.Collection (find_classifier_type t x)
		      | ocl_type.Sequence   (ocl_type.Classifier [x]) => ocl_type.Sequence (find_classifier_type t x)
		      | ocl_type.Set        (ocl_type.Classifier [x]) => ocl_type.Set (find_classifier_type t x)
		      | ocl_type.Bag        (ocl_type.Classifier [x]) => ocl_type.Bag (find_classifier_type t x)
		      | ocl_type.OrderedSet (ocl_type.Classifier [x]) => ocl_type.OrderedSet (find_classifier_type t x)
	(*	      | ocl_type.Collection (ocl_type.Classifier [x]) => ocl_type.Collection (find_classifier_type t x) *)
    end
    handle Option => error ("expected Classifier "^xmiid^" in table")
		    

fun insert_constraint table (c:XMI_OCL.OCLConstraint) =
    HashTable.insert table (#xmiid c, Constraint c)

fun insert_variable_dec table (v:XMI_OCL.VariableDeclaration) =
    HashTable.insert table (#xmiid v, Variable v)

fun insert_stereotype table (s:XMI_UML.UMLStereotype) =
    HashTable.insert table (#xmiid s, Stereotype (#name s))

fun insert_generalization table (g:XMI_UML.UMLGeneralization) =
    HashTable.insert table (#xmiid g, Generalization (#child_id g, #parent_id g))

fun insert_attribute table path_prefix (a:XMI_UML.UMLAttribute) =
    HashTable.insert table (#xmiid a, Attribute (path_prefix @ [#name a]))

fun insert_operation table path_prefix (a:XMI_UML.UMLOperation) =
    HashTable.insert table (#xmiid a, Operation (path_prefix @ [#name a]))

fun add_aend table xmiid (aend:mdr_core.associationend) = () (* FIX *)


fun insert_classifier table package_prefix class = 
    let val id      = XMI_UML.classifier_xmiid_of class
	val name    = XMI_UML.classifier_name_of class
	val path    = package_prefix @ [name]
	val ocltype = if (package_prefix = ["oclLib"]
			  orelse package_prefix = ["UML_OCL"])
		      then if      name = "Integer" then ocl_type.Integer
			   else if name = "Boolean" then ocl_type.Boolean
			   else if name = "String"  then ocl_type.String
			   else if name = "Real"    then ocl_type.Real
			   else if name = "OclVoid" then ocl_type.OclVoid
			   else if name = "OclAny"  then ocl_type.OclAny
			   (* now this is really ugly... *)
			   else if String.isPrefix "Collection(" name then ocl_type.Collection (ocl_type.Classifier [XMI_UML.classifier_elementtype_of class])
			   else if String.isPrefix "Sequence("   name then ocl_type.Sequence   (ocl_type.Classifier [XMI_UML.classifier_elementtype_of class])
			   else if String.isPrefix "Set("        name then ocl_type.Set        (ocl_type.Classifier [XMI_UML.classifier_elementtype_of class])
			   else if String.isPrefix "Bag("        name then ocl_type.Bag        (ocl_type.Classifier [XMI_UML.classifier_elementtype_of class])
			   else if String.isPrefix "OrderedSet(" name then ocl_type.OrderedSet (ocl_type.Classifier [XMI_UML.classifier_elementtype_of class])
			   else error ("didn't recognize ocltype "^name) 
		      else ocl_type.Classifier path
	 val aends = nil (* aends = find_aends table id*)
    in 
	HashTable.insert table (id,Type (ocltype,aends));
	case class 
	 of XMI_UML.Class c => (map (insert_attribute table path) (#attributes c);
				map (insert_operation table path) (#operations c); ())
	  | XMI_UML.Primitive c => (map (insert_operation table path) (#operations c); ())
	  | XMI_UML.Enumeration c => (map (insert_operation table path) (#operations c); ())
	  | XMI_UML.Interface c => (map (insert_operation table path) (#operations c); ())
	  | XMI_UML.Collection c => (map (insert_operation table path) (#operations c); ())
	  | XMI_UML.Sequence c => (map (insert_operation table path) (#operations c); ())
	  | XMI_UML.Set c => (map (insert_operation table path) (#operations c); ())
	  | XMI_UML.Bag c => (map (insert_operation table path) (#operations c); ())
	  | XMI_UML.OrderedSet c => (map (insert_operation table path) (#operations c); ())
	  | _ => ()
    end

fun insert_package table package_prefix (XMI_UML.UMLPackage{xmiid,name,
							    ownedClassifiers,
							    ownedPackages,
							    ownedGeneralizations,
							    ownedConstraints,...}) =
    let val id = xmiid
	val full_name = package_prefix @ [name]
    in 
	map (insert_generalization table ) ownedGeneralizations;
	map (insert_constraint table) ownedConstraints;
	map (insert_classifier table full_name) ownedClassifiers;
	map (insert_package   table full_name) ownedPackages;
	HashTable.insert table (id,Package full_name)
    end 

fun transform_expression t (XMI_OCL.LiteralExp {symbol,expression_type}) = 
    ocl_term.Literal (symbol,find_classifier_type t expression_type)
  | transform_expression t (XMI_OCL.IfExp {condition,thenExpression,
					   elseExpression,expression_type}) = 
    ocl_term.If (transform_expression t condition, 
		 find_classifier_type t (XMI_OCL.expression_type_of condition),
		 transform_expression t thenExpression, 
		 find_classifier_type t (XMI_OCL.expression_type_of thenExpression),
		 transform_expression t elseExpression,
		 find_classifier_type t (XMI_OCL.expression_type_of elseExpression),
		 find_classifier_type t expression_type)
  | transform_expression t (XMI_OCL.AttributeCallExp {source,referredAttribute,
						      expression_type}) =
    ocl_term.AttributeCall (transform_expression t source,
			    find_classifier_type t (XMI_OCL.expression_type_of source),
			    find_attribute t referredAttribute,
			    find_classifier_type t expression_type)
  | transform_expression t (XMI_OCL.OperationCallExp {source,arguments,
						      referredOperation,
						      expression_type}) =
    let val arglist = map (transform_expression t) arguments
	val argtyplist = map ((find_classifier_type t) o XMI_UML.expression_type_of) arguments
    in
	ocl_term.OperationCall (transform_expression t source,
			     find_classifier_type t (XMI_OCL.expression_type_of source),
			     find_operation t referredOperation,
			     ListPair.zip (arglist, argtyplist),
			     find_classifier_type t expression_type)
    end
  | transform_expression t (XMI_OCL.VariableExp {referredVariable,expression_type})=
    let val var_dec = find_variable_dec t referredVariable
    in 
	ocl_term.Variable (#name var_dec,find_classifier_type t expression_type)
    end

fun transform_constraint t ({xmiid,name,body,...}:XMI_OCL.OCLConstraint) = 
    (name,transform_expression t body)


fun transform_parameter t {xmiid,name,kind,type_id} =
    (name, find_classifier_type t type_id)

fun transform_operation t {xmiid,name,isQuery,parameter,visibility,
			   constraints} =
    {name=name,
     arguments = map (transform_parameter t)
		     (filter (fn x => #kind x <> XMI_UML.Return) parameter),
     precondition = map ((transform_constraint t) o (find_constraint t)) 
			(filter_precondition t constraints),
     postcondition = map ((transform_constraint t) o (find_constraint t)) 
			 (filter_postcondition t constraints), 
     result = find_classifier_type t ((#type_id o hd)(filter (fn x => #kind x = XMI_UML.Return) parameter)),
     isQuery = isQuery      (* FIX *)
     }

      

fun transform_attribute t ({xmiid,name,type_id,changeability,visibility}) =
    (name,find_classifier_type t type_id)

	
fun transform_classifier t (XMI_UML.Class {xmiid,name,isActive,visibility,isLeaf,
					   generalizations,attributes,operations,
					   invariant}) =
    let val parents = map ((find_classifier_type t) o (find_parent t)) 
			  generalizations 
	val filtered_parents = filter (fn x => x <> ocl_type.OclAny) parents
    in
	mdr_core.Class {name = path_of_classifier (find_classifier_type t xmiid), 
			parent = case filtered_parents 
				  of [] => NONE
				   | xs => SOME (path_of_classifier (hd xs)),
			attributes = map (transform_attribute t) attributes,
			operations = map (transform_operation t) operations,
			invariant = map ((transform_constraint t) o 
					 (find_constraint t)) invariant, 
			associationends = nil, (* FIX *)
			stereotypes = nil, (* FIX *)
			interfaces = nil, (* FIX *)
			thyname = NONE}
    end
  | transform_classifier t (XMI_UML.Primitive {xmiid,name,generalizations,
					       operations,invariant}) =
    mdr_core.Primitive {name = case find_classifier_type t xmiid of ocl_type.Classifier x => x, 
			parent = NONE,    (* FIX *)
			operations = map (transform_operation t) operations,
			associationends = nil, (* FIX *)
			invariant = map ((transform_constraint t) o 
					 (find_constraint t)) invariant,
			stereotypes = nil, (*FIX *)
			interfaces = nil, (* FIX *)
		   thyname = NONE}
  | transform_classifier t (XMI_UML.Enumeration {xmiid,name,generalizations,
						 operations,literals,invariant}) =
    mdr_core.Enumeration {name = case find_classifier_type t xmiid of ocl_type.Classifier x => x, 
			  parent = NONE,    (* FIX *)
			  literals = literals,
			  operations = map (transform_operation t) operations,
			  invariant =   map ((transform_constraint t) o 
					     (find_constraint t)) invariant,
			  stereotypes = nil, (* FIX *)
			  interfaces = nil, (* FIX *)
		   thyname = NONE}
  | transform_classifier t (_) = error "Not supported Classifier type found."
			   

fun transform_package t (XMI_UML.UMLPackage {xmiid,name,ownedPackages,
					     ownedClassifiers,
					     ownedAssociations,
					     ownedGeneralizations,
					     ownedConstraints,visibility}) =
    append (map (transform_classifier t) ownedClassifiers)
	   (List.concat (map (transform_package t) ownedPackages))


(* splits an association into a list of two (or more) association ends, *)
(* together with the xmi.id of the "opposite" class"                    *)
fun split_assoc (assoc:XMI_UML.UMLAssociation) = 
    let val aends = #connection assoc
	fun attach_opposite ae aends  = map (fn (x:XMI_UML.UMLAssociationEnd) => (#participant_id x, ae)) 
					     (List.filter (fn x => x <> ae) aends)
    in 
	List.concat (map (fn x => attach_opposite x aends) aends)
    end

fun transform_aend t (id, {xmiid,name,ordering,multiplicity,participant_id,
			   isNavigable,aggregation,changeability,visibility}) 
  = (id,{name=name,
	 aend_type=ocl_type.OclAny (* FIX *),
	 multiplicity = multiplicity,
	 ordered = if ordering = XMI_UML.Ordered then true else false })


(* recursively collects all associations in the given list of packages *)
fun collect_associations xs [] = xs
  | collect_associations xs 
    ((XMI_UML.UMLPackage {ownedPackages,ownedAssociations,...})::ps) =
    collect_associations (append xs ownedAssociations) (append ownedPackages ps)

fun transform_toplevel t (XMI_UML.UMLPackage {xmiid,name,ownedPackages,
					      ownedClassifiers,
					      ownedAssociations,
					      ownedGeneralizations,
					      ownedConstraints,visibility}) =
    let val filteredPackages = 
	    filter (fn (XMI_UML.UMLPackage x) => ((#name x <> "oclLib")
						 andalso (#name x <> "UML_OCL")))
		   ownedPackages (* throw away oclLib *)
(*	val assocs = collect_associations ownedAssociations filteredPackages
	val aends = List.concat (map split_assoc assocs)
	val mdr_aends = map (transform_aend t) aends*)
	val _ = map (insert_package t nil) ownedPackages 
	val _ = map (insert_classifier t nil) ownedClassifiers
	val cls_in_this_package = map (transform_classifier t) 
				      ownedClassifiers
	val cls_in_contained_packages =
	    List.concat (map (transform_package t) filteredPackages)
    in 
	cls_in_this_package@cls_in_contained_packages
    end
    
fun transformXMI ({classifiers,constraints,packages,
		   stereotypes,variable_declarations}) =
    let val (xmiid_table: (string,HashTableEntry) HashTable.hash_table) =
	    HashTable.mkTable (HashString.hashString, (op =)) (101, IllFormed)
	val _ = map (insert_classifier xmiid_table nil) classifiers
	val _ = map (insert_constraint xmiid_table) constraints
	val _ = map (insert_stereotype xmiid_table) stereotypes
	val _ = map (insert_variable_dec xmiid_table) variable_declarations
    in 
	transform_toplevel xmiid_table (hd packages)
    end
end




