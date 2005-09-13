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



structure Xmi2Rep : 
sig
    val transformXMI : XMI.XmiContent -> Rep.Classifier list
    val readXMI      : string -> Rep.Classifier list
    (* generic exception if something is wrong *)
    exception IllFormed of string
end  =
struct
exception IllFormed of string
exception NotYetImplemented

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


val triv_expr = Rep_OclTerm.Literal ("true",Rep_OclType.Boolean)

fun transform_expression t (XMI.LiteralExp {symbol,expression_type}) = 
    Rep_OclTerm.Literal (symbol,find_classifier_type t expression_type)
  | transform_expression t (XMI.IfExp {condition,thenExpression,
					   elseExpression,expression_type}) = 
    Rep_OclTerm.If (transform_expression t condition, 
		 find_classifier_type t (XMI.expression_type_of condition),
		 transform_expression t thenExpression, 
		 find_classifier_type t (XMI.expression_type_of thenExpression),
		 transform_expression t elseExpression,
		 find_classifier_type t (XMI.expression_type_of elseExpression),
		 find_classifier_type t expression_type)
  | transform_expression t (XMI.AttributeCallExp {source,referredAttribute,
						      expression_type}) =
    Rep_OclTerm.AttributeCall (transform_expression t source,
			    find_classifier_type t (XMI.expression_type_of source),
			    find_attribute t referredAttribute,
			    find_classifier_type t expression_type)
  | transform_expression t (XMI.OperationCallExp {source,arguments,
						      referredOperation,
						      expression_type}) =
    let val arglist = map (transform_expression t) arguments
	val argtyplist = map ((find_classifier_type t) o XMI.expression_type_of) arguments
    in
	Rep_OclTerm.OperationCall (transform_expression t source,
			     find_classifier_type t (XMI.expression_type_of source),
			     find_operation t referredOperation,
			     ListPair.zip (arglist, argtyplist),
			     find_classifier_type t expression_type)
    end
  | transform_expression t (XMI.OperationWithTypeArgExp {source,name,
							     typeArgument,
							     expression_type}) =
    Rep_OclTerm.OperationWithType (transform_expression t source,
				find_classifier_type t (XMI.expression_type_of source),
				name,
				find_classifier_type t typeArgument,
				find_classifier_type t expression_type)    
  | transform_expression t (XMI.VariableExp {referredVariable,expression_type})=
    let val var_dec = find_variable_dec t referredVariable
    in 
	Rep_OclTerm.Variable (#name var_dec,find_classifier_type t expression_type)
    end
  | transform_expression t (XMI.AssociationEndCallExp {source, referredAssociationEnd, expression_type}) =
    Rep_OclTerm.AssociationEndCall (transform_expression t source,
				 find_classifier_type t (XMI.expression_type_of source),
				 find_associationend t referredAssociationEnd,
				 find_classifier_type t expression_type
				 )
  | transform_expression t (XMI.IteratorExp {name,iterators,body,source,expression_type}) = 
    Rep_OclTerm.Iterator (name,
		       map (fn x => (#name x, find_classifier_type t (#declaration_type x))) iterators,
		       transform_expression t source, find_classifier_type t (XMI.expression_type_of source),
		       transform_expression t body, find_classifier_type t (XMI.expression_type_of body),
		       find_classifier_type t expression_type
		       )
  | transform_expression t _ = raise NotYetImplemented

fun transform_constraint t ({xmiid,name,body,...}:XMI.Constraint) = 
	let val n_name = case name of 
		(SOME s) => if (s = "") then NONE else (SOME(s))
	       |NONE     => NONE
	in	
    		(n_name,transform_expression t body)
		handle NotYetImplemented => (print "Warning: in Xmi2Mdr.transform_constraint: Something is not yet implemented.\n";(NONE, triv_expr))
		     | IllFormed msg => (print ("Warning: in Xmi2Mdr.transform_constraint: Could not parse Constraint: "^msg^"\n");(NONE, triv_expr))
		     | ParseXMI.IllFormed msg => (print ("Warning: in Xmi2Mdr.transform_constraint: Could not parse Constraint: "^msg^"\n");(NONE, triv_expr))
	end

fun transform_parameter t {xmiid,name,kind,type_id} =
    (name, find_classifier_type t type_id)

fun transform_operation t {xmiid,name,isQuery,parameter,visibility,
			   constraints} =
    {name=name,
     arguments = map (transform_parameter t)
		     (filter (fn x => #kind x <> XMI.Return) parameter),
     precondition = map ((transform_constraint t) o (find_constraint t)) 
			(filter_precondition t constraints),
     postcondition = map ((transform_constraint t) o (find_constraint t)) 
			 (filter_postcondition t constraints), 
     result = find_classifier_type t ((#type_id o hd)(filter (fn x => #kind x = XMI.Return) parameter)),
     isQuery = isQuery      (* FIX *)
     }

      

fun transform_attribute t ({xmiid,name,type_id,changeability,visibility,
			    ordering,multiplicity}) =
    let val cls_type = find_classifier_type t type_id 
    in
	(name,if multiplicity = [(1,1)] 
	      then cls_type
	      else if ordering = XMI.Ordered then Rep_OclType.Sequence cls_type
	      else Rep_OclType.Set cls_type)
    end

fun transform_aend t ({xmiid,name,ordering,multiplicity,participant_id,
		       isNavigable,aggregation,changeability,visibility})
  = {name         = valOf name,
     aend_type    = find_classifier_type t participant_id,
     multiplicity = multiplicity,
     ordered      = if ordering = XMI.Ordered then true else false }

val filter_named_aends  = List.filter (fn {name=SOME _,...}:XMI.AssociationEnd => true
					| _ => false)
	
fun transform_classifier t (XMI.Class {xmiid,name,isActive,visibility,isLeaf,
					   generalizations,attributes,operations,
					   invariant}) =
    let val parents = map ((find_classifier_type t) o (find_parent t)) 
			  generalizations 
	val filtered_parents = filter (fn x => x <> Rep_OclType.OclAny) parents
    in
	Rep.Class {name = path_of_classifier (find_classifier_type t xmiid), 
			parent = case filtered_parents 
				  of [] => NONE
				   | xs => SOME (path_of_classifier (hd xs)),
			attributes = map (transform_attribute t) attributes,
			operations = map (transform_operation t) operations,
			invariant  = map ((transform_constraint t) o 
					  (find_constraint t)) invariant, 
			associationends = map (transform_aend t) 
					      ((filter_named_aends (find_aends t xmiid))), 
			stereotypes = nil, (* FIX *)
			interfaces = nil, (* FIX *)
                        activity_graphs = nil,
			thyname = NONE}
    end
  | transform_classifier t (XMI.Primitive {xmiid,name,generalizations,
					       operations,invariant}) =
    Rep.Primitive {name = case find_classifier_type t xmiid of Rep_OclType.Classifier x => x
								  | _ => raise Option, 
			parent = NONE,    (* FIX *)
			operations = map (transform_operation t) operations,
			associationends = map (transform_aend t) 
					      (filter_named_aends (find_aends t xmiid)), 
			invariant = map ((transform_constraint t) o 
					 (find_constraint t)) invariant,
			stereotypes = nil, (*FIX *)
			interfaces = nil, (* FIX *)
		   thyname = NONE}
  | transform_classifier t (XMI.Enumeration {xmiid,name,generalizations,
						 operations,literals,invariant}) =
    Rep.Enumeration {name = case find_classifier_type t xmiid of Rep_OclType.Classifier x => x
								    | _ => raise Option, 
			  parent = NONE,    (* FIX *)
			  literals = literals,
			  operations = map (transform_operation t) operations,
			  invariant =   map ((transform_constraint t) o 
					     (find_constraint t)) invariant,
			  stereotypes = nil, (* FIX *)
			  interfaces = nil, (* FIX *)
		   thyname = NONE}
  | transform_classifier t (_) = raise IllFormed "Not supported Classifier type found."
			   

(* recursively transform all classes in the package *)
fun transform_package t (XMI.Package p) =
    let (* we do not transform the ocl library *)
	val filteredPackages = 
	    filter (fn (XMI.Package x) => 
		       ((#name x <> "oclLib") andalso (#name x <> "UML_OCL")))
		   (#packages p) 
    in 
	(map (transform_classifier t) (#classifiers p))@
		    (List.concat (map (transform_package t) filteredPackages))
    end

(* recursively insert mapping of xmi.id's to model elements into Hashtable *)
fun insert_package table package_prefix (XMI.Package p) =
    let val full_name = package_prefix @ [#name p]
    in 
	map (insert_generalization table)           (#generalizations p);
	map (insert_constraint     table)           (#constraints p);
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
	map (insert_classifier     table full_name) (#classifiers p);
	map (insert_package        table full_name) (#packages p);
	HashTable.insert table (#xmiid p,Package full_name)
    end 


(* split an association into association ends, and put the association ends *)
(* ends into the xmi.id table under the corresponding (i.e., opposite)      *)
(* classifier.                                                              *)   
(* 1. split the association into a list of two (or more) association ends   *)
(* 2. pair each association end with the participant_id's of all other      *)
(*    association ends: when a class is a participant in an association,    *)
(*    this association end is a feature of all _other_ participants in the  *) 
(*    association                                                           *)
(* 3. insert the mapping xmi.id of class to association end into the        *)
(*    hashtable                                                             *)
(* 4. insert mapping xmi.id of association end to path into the hashtable   *)
fun transform_assocation t (assoc:XMI.Association) =
    let	val aends = #connection assoc
	fun all_others x xs = List.filter 
				  (fn (y:XMI.AssociationEnd) => y <> x) xs
	fun pair_with ae aes = 
	    map (fn (x:XMI.AssociationEnd) => (#participant_id x, ae)) aes
	val mappings = List.concat (map (fn x => pair_with x (all_others x aends)) aends)
	fun add_aend_to_type (id,ae) = 
	    let val type_of_id = find_classifier_type t id
		val aends_of_id = ae::(find_aends t id)
		val path_of_id = case type_of_id of Rep_OclType.Classifier x => x
		val path_of_ae = path_of_id @ [case #name ae of SOME x => x
							      | NONE   => ""]
	    in 
		(HashTable.insert t (id,Type (type_of_id,aends_of_id));
		 HashTable.insert t (#xmiid ae, AssociationEnd path_of_ae))
	    end
    in 
	List.app add_aend_to_type mappings
    end

(* recursively transforms all associations in the package p, *)
fun transform_associations t (XMI.Package p) = 
    (map (transform_associations t) (#packages p);
    List.app (transform_assocation t) (#associations p))

(* transform a UML model into a list of Rep classes           *)
(* 1. traverse package hierarchy and put xmi.id of all interesting *)
(*    model elements into the hashtable                            *) 
(* 2. traverse again to find all associations, transform them into *)
(*    association ends and map the correct classes to them         *)
(*    (We have to handle associations seperately because there is  *)
(*     no direct link from classes to their association ends in    *)
(*     the xmi file)                                               *)
(* 3. traverse again, transforming all remaining model elements,   *)
(*    i.e., classes with their operations, attributes,             *)
(*    constraints, etc                                             *)
fun transformXMI ({classifiers,constraints,packages,
		   stereotypes,variable_declarations,state_machines, activity_graphs}) =
    let val (xmiid_table: (string,HashTableEntry) HashTable.hash_table) =
	    HashTable.mkTable (HashString.hashString, (op =)) (101, Option)
	(* hack: insert a dummy type into the table *)
	val _ = HashTable.insert xmiid_table ("DummyT",Type (Rep_OclType.DummyT,nil))
	(* for some reasons, there are model elements outside of the top-level *) 
	(* model the xmi-file. So we have to handle them here seperately:      *)
	val _ = map (insert_classifier xmiid_table nil) classifiers
	val _ = map (insert_constraint xmiid_table) constraints
	val _ = map (insert_stereotype xmiid_table) stereotypes
	val _ = map (insert_variable_dec xmiid_table) variable_declarations
	(* "hd packages" is supposed to be the first model in the xmi-file *)
	val model = hd packages
    in 
	insert_model           xmiid_table model; (* fill xmi.id table   *)
	transform_associations xmiid_table model; (* handle associations *)
	map Rep.normalize (transform_package xmiid_table model) (* transform classes   *)
    end
	handle Empty => raise Option

fun readXMI f = (transformXMI o ParseXMI.readFile) f
    handle ParseXMI.IllFormed msg => (print ("Warning: in Xmi2Mdr.readXMI: could not parse file "^f^":\n"^msg^"\n"); 
				      nil)
	 | Option => (print ("Warning: in Xmi2Mdr.readXMI: could not parse file "^f^"\n"); 
				      nil)
	 | IllFormed msg => (print ("Warning: in Xmi2Mdr.readXMI: could not parse file "^f^": "^msg^"\n"); 
				      nil)
end




