(*****************************************************************************
 *          su4sml - a SecureUML repository for SML              
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


structure Xmi_IDTable = 
struct
open library
exception IllFormed of string

datatype HashTableEntry = Package of Rep_OclType.Path
		        | Type of (Rep_OclType.OclType * 
				   (XMI.AssociationEnd list)) 
                        | Generalization of (string * string) 
			| Constraint of XMI.Constraint 
		        | Stereotype of string
		        | Variable of XMI.VariableDeclaration 
			| Attribute of Rep_OclType.Path
			| Operation of Rep_OclType.Path
		        | AssociationEnd of Rep_OclType.Path 

fun find_generalization t xmiid = 
    (case valOf (HashTable.find t xmiid) 
      of Generalization x => x
       | _                => raise Option) 
    handle Option => raise IllFormed ("expected Generalization "^xmiid^" in table")

fun find_stereotype t xmiid =
    (case valOf (HashTable.find t xmiid) 
      of Stereotype x => x
       | _                => raise Option) 
    handle Option => raise IllFormed ("expected Stereotype "^xmiid^" in table")

fun find_attribute t xmiid =
    (case valOf (HashTable.find t xmiid) 
      of Attribute x => x
       | _                => raise Option) 
    handle Option => raise IllFormed ("expected Attribute "^xmiid^" in table")

fun find_operation t xmiid =
    (case valOf (HashTable.find t xmiid) 
      of Operation x => x
       | _                => raise Option) 
    handle Option => raise IllFormed ("expected Operation "^xmiid^" in table")

fun find_type t xmiid = 
    (case valOf (HashTable.find t xmiid) 
      of Type x  => x
       | _                => raise Option) 
    handle Option => raise IllFormed ("expected Type "^xmiid^" in table (find_type)")

fun find_aends t xmiid = 
    (case valOf (HashTable.find t xmiid) 
      of (Type (c,xs))  => xs
       | _                => raise Option) 
    handle Option => raise IllFormed ("expected Type "^xmiid^" in table (find_aends)")

fun find_variable_dec t xmiid =
    (case valOf (HashTable.find t xmiid) 
      of Variable x => x
       | _                => raise Option) 
    handle Option => raise IllFormed ("expected VariableDeclaration "^xmiid^" in table")

fun find_parent t xmiid = #2 (find_generalization t xmiid)

fun find_package t xmiid  = 
    (case valOf (HashTable.find t xmiid) 
      of Package path => path
       | _                => raise Option) 
    handle Option => raise IllFormed ("expected Path "^xmiid^" in table")
					
fun path_of_classifier (Rep_OclType.Classifier x) = x

fun find_constraint t xmiid =
    (case valOf (HashTable.find t xmiid) 
      of Constraint c => c
       | _                => raise Option) 
    handle Option => raise IllFormed ("expected Constraint "^xmiid^" in table")

fun find_associationend t xmiid  = 
    (case valOf (HashTable.find t xmiid) 
      of AssociationEnd path => path
       | _                   => raise Option) 
    handle Option => raise IllFormed ("expected AssociationEnd "^xmiid^" in table")
		
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
							   | _  => raise Option
    in 
	case ocltype of Rep_OclType.Integer      => ocltype
		      | Rep_OclType.String       => ocltype
		      | Rep_OclType.Real         => ocltype
		      | Rep_OclType.Boolean      => ocltype
		      | Rep_OclType.Classifier x => ocltype
		      | Rep_OclType.OclVoid      => ocltype
		      | Rep_OclType.OclAny       => ocltype
		      | Rep_OclType.DummyT       => ocltype
		      | Rep_OclType.Collection (Rep_OclType.Classifier [x]) => Rep_OclType.Collection (find_classifier_type t x)
		      | Rep_OclType.Sequence   (Rep_OclType.Classifier [x]) => Rep_OclType.Sequence (find_classifier_type t x)
		      | Rep_OclType.Set        (Rep_OclType.Classifier [x]) => Rep_OclType.Set (find_classifier_type t x)
		      | Rep_OclType.Bag        (Rep_OclType.Classifier [x]) => Rep_OclType.Bag (find_classifier_type t x)
		      | Rep_OclType.OrderedSet (Rep_OclType.Classifier [x]) => Rep_OclType.OrderedSet (find_classifier_type t x)
		      | _ => raise Option
	 end
	handle Option => raise IllFormed ("expected Classifier "^xmiid^" in table")
		    

fun insert_constraint table (c:XMI.Constraint) =
    HashTable.insert table (#xmiid c, Constraint c)

fun insert_variable_dec table (v:XMI.VariableDeclaration) =
    HashTable.insert table (#xmiid v, Variable v)

fun insert_stereotype table (s:XMI.Stereotype) =
    HashTable.insert table (#xmiid s, Stereotype (#name s))

fun insert_generalization table (g:XMI.Generalization) =
    HashTable.insert table (#xmiid g, Generalization (#child_id g, #parent_id g))

fun insert_attribute table path_prefix (a:XMI.Attribute) =
    HashTable.insert table (#xmiid a, Attribute (path_prefix @ [#name a]))

fun insert_operation table path_prefix (a:XMI.Operation) =
    HashTable.insert table (#xmiid a, Operation (path_prefix @ [#name a]))

fun add_aend table xmiid (aend:Rep.associationend) = () (* FIX *)


fun insert_classifier table package_prefix class = 
    let val id      = XMI.classifier_xmiid_of class
	val name    = XMI.classifier_name_of class
	val path    = package_prefix @ [name]
	val ocltype = if (package_prefix = ["oclLib"]
			  orelse package_prefix = ["UML_OCL"])
		      then if      name = "Integer" then Rep_OclType.Integer
			   else if name = "Boolean" then Rep_OclType.Boolean
			   else if name = "String"  then Rep_OclType.String
			   else if name = "Real"    then Rep_OclType.Real
			   else if name = "OclVoid" then Rep_OclType.OclVoid
			   else if name = "OclAny"  then Rep_OclType.OclAny
			   (* now this is really ugly... *)
			   else if String.isPrefix "Collection(" name then Rep_OclType.Collection (Rep_OclType.Classifier [XMI.classifier_elementtype_of class])
			   else if String.isPrefix "Sequence("   name then Rep_OclType.Sequence   (Rep_OclType.Classifier [XMI.classifier_elementtype_of class])
			   else if String.isPrefix "Set("        name then Rep_OclType.Set        (Rep_OclType.Classifier [XMI.classifier_elementtype_of class])
			   else if String.isPrefix "Bag("        name then Rep_OclType.Bag        (Rep_OclType.Classifier [XMI.classifier_elementtype_of class])
			   else if String.isPrefix "OrderedSet(" name then Rep_OclType.OrderedSet (Rep_OclType.Classifier [XMI.classifier_elementtype_of class])
			   else raise IllFormed ("didn't recognize ocltype "^name) 
		      else Rep_OclType.Classifier path
	(* This function is called before the associations are handled, *)
	(* so we do not have to take care of them now...                *)
	val aends = nil 
    in 
	HashTable.insert table (id,Type (ocltype,aends));
	case class 
	 of XMI.Class c => (map (insert_attribute table path) (#attributes c);
				map (insert_operation table path) (#operations c); ())
	  | XMI.Primitive c => (map (insert_operation table path) (#operations c); ())
	  | XMI.Enumeration c => (map (insert_operation table path) (#operations c); ())
	  | XMI.Interface c => (map (insert_operation table path) (#operations c); ())
	  | XMI.Collection c => (map (insert_operation table path) (#operations c); ())
	  | XMI.Sequence c => (map (insert_operation table path) (#operations c); ())
	  | XMI.Set c => (map (insert_operation table path) (#operations c); ())
	  | XMI.Bag c => (map (insert_operation table path) (#operations c); ())
	  | XMI.OrderedSet c => (map (insert_operation table path) (#operations c); ())
	  | _ => ()
    end

(* recursively insert mapping of xmi.id's to model elements into Hashtable *)
fun insert_package table package_prefix (XMI.Package p) =
    let val full_name = package_prefix @ [#name p]
    in 
	map (insert_generalization table)           (#generalizations p);
	map (insert_constraint     table)           (#constraints p);
	map (insert_stereotype     table)           (#stereotypes p);
	map (insert_classifier     table full_name) (#classifiers p);
	map (insert_package        table full_name) (#packages p);
	HashTable.insert table (#xmiid p,Package full_name)
    end 

(* We do not want the name of the model to be part of the package hierarchy, *)
(* therefore we handle the top-level model seperately                        *)
fun insert_model table (XMI.Package p) = 
    let val full_name = nil
    in 
	map (insert_generalization table)           (#generalizations p);
	map (insert_constraint     table)           (#constraints p);
	map (insert_stereotype     table)           (#stereotypes p);
	map (insert_classifier     table full_name) (#classifiers p);
	map (insert_package        table full_name) (#packages p);
	HashTable.insert table (#xmiid p,Package full_name)
    end 


end
