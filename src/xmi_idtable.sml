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
				   (XMI.AssociationEnd list) *
				   XMI.Classifier * 
				   (XMI.ActivityGraph list)) 
                        | Generalization of (string * string) 
			| Constraint of XMI.Constraint 
		        | Stereotype of string
		        | Variable of XMI.VariableDeclaration 
			| Attribute of Rep_OclType.Path
			| Operation of Rep_OclType.Path
		        | AssociationEnd of Rep_OclType.Path 
			| State of XMI.StateVertex
			| Transition of XMI.Transition 
		        | Dependency of XMI.Dependency
		        | TagDefinition of string
		        | ClassifierInState of string 
		        | Event of XMI.Event
 
fun find_tagdefinition t xmiid =
    (case valOf (HashTable.find t xmiid) 
      of TagDefinition x => x
       | _                => raise Option) 
    handle Option => raise IllFormed ("expected TagDefinition "^xmiid^" in table")

fun find_state t xmiid = 
    (case valOf (HashTable.find t xmiid) 
      of State x => x
       | _                => raise Option) 
    handle Option => raise IllFormed ("expected State "^xmiid^" in table")

fun find_event t xmiid = 
    (case valOf (HashTable.find t xmiid) 
      of Event x => x
       | _                => raise Option) 
    handle Option => raise IllFormed ("expected Event "^xmiid^" in table")

fun find_transition t xmiid = 
    (case valOf (HashTable.find t xmiid) 
      of Transition x => x
       | _                => raise Option) 
    handle Option => raise IllFormed ("expected Transition "^xmiid^" in table")

fun find_dependency t xmiid = 
    (case valOf (HashTable.find t xmiid) 
      of Dependency x => x
       | _                => raise Option) 
    handle Option => raise IllFormed ("expected Dependency "^xmiid^" in table")

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
      of (Type (c,xs,_,_))  => xs
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
  | path_of_classifier _ = raise IllFormed ("path_of_classifier called on non-Classifier argument")

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
		

fun filter_exists t cs = 
	filter (fn x => Option.isSome (HashTable.find t x)) cs 

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

fun filter_bodyconstraint t cs
  = filter (fn x => let val constraint = find_constraint t x
			val name = #name constraint
			val constr_type_ref = #constraint_type constraint
			val constr_type_name = find_stereotype t constr_type_ref
		    in 
			constr_type_name = "body"
		    end) cs


fun find_classifier t xmiid =
    (case valOf (HashTable.find t xmiid) 
      of Type (_,_,c,_) => c
       | _                => raise Option) 
    handle Option => raise IllFormed ("expected Classifier "^xmiid^" in table (in find_classifer)")

fun find_classifierInState_classifier t cis_id =
    (case valOf (HashTable.find t cis_id)
      of ClassifierInState c => find_classifier t c
       | Type (_,_,c,_)      => c
	   | _                   => raise Option)
    handle Option =>  raise IllFormed ("expected ClassifierInState "
				       ^cis_id^" in table")

fun find_activity_graph_of t xmiid = 
    (case valOf (HashTable.find t xmiid) 
      of Type (_,_,_,ag) => ag
       | _                => raise Option) 
    handle Option => raise IllFormed ("expected Classifier "^xmiid^" in table (in find_activity_graph_of)")

			
fun find_classifier_type t xmiid
  = let val ocltype = case valOf (HashTable.find t xmiid) of (Type (x,xs,_,_)) => x
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
		      | _ => raise IllFormed ("unexpected Classifier-Type "^xmiid^" in table")
	 end
	handle Option => raise IllFormed ("expected Classifier "^xmiid^" in table (in find_classifier_type)")
		    

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

fun insert_state table (XMI.CompositeState st) =
    (List.app (insert_state table) (#subvertex st);
    HashTable.insert table (#xmiid st, State (XMI.CompositeState st)))
  | insert_state table (XMI.SubactivityState st) =
    (List.app (insert_state table) (#subvertex st);
    HashTable.insert table (#xmiid st, State (XMI.SubactivityState st)))
  | insert_state table (st:XMI.StateVertex) = 
    HashTable.insert table (XMI.state_xmiid_of st, State st)
    
fun insert_event table (e as XMI.CallEvent ev) =
    HashTable.insert table (#xmiid ev, Event e)
  | insert_event  table (e as XMI.SignalEvent ev) =
    HashTable.insert table (#xmiid ev, Event e)

fun insert_transition table (XMI.mk_Transition trans) =
    HashTable.insert table (#xmiid trans, Transition (XMI.mk_Transition trans))

fun insert_activity_graph table (XMI.mk_ActivityGraph ag) =
    let val context = #contextxmiid ag 
    in  
	(case valOf (HashTable.find table context) 
	  of (Type (c,xs,aes,ags))  => HashTable.insert 
					   table (context, Type (c,xs,aes,
XMI.mk_ActivityGraph ag::ags))
	   | _                => raise Option) 
	      handle Option => raise IllFormed ("expected Type "^context^" in table (insert_activity_graph)");
	List.app (insert_transition table) (#transitions ag);
	insert_state table (#top ag)
    end

fun insert_dependency table dep =
    HashTable.insert table (#xmiid dep, Dependency dep)

fun insert_tagdefinition table (td:XMI.TagDefinition) =
    HashTable.insert table (#xmiid td, TagDefinition (#name td))

fun insert_classifierInState table cls_id cis_id =
    HashTable.insert table (cis_id,ClassifierInState cls_id)

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
                           (* this shouldn't be necessary: *) 
			   else if name = "Void"    then Rep_OclType.OclVoid 
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
	val ag    = nil
    in 
	HashTable.insert table (id,Type (ocltype,aends,class,ag));
	case class 
	 of XMI.Class c => (List.app (insert_attribute table path) (#attributes c);
			    List.app (insert_operation table path) (#operations c);
			    List.app (insert_classifierInState table id) (#classifierInState c);
			    ())
	  | XMI.Primitive c => (List.app (insert_operation table path) (#operations c); ())
	  | XMI.Enumeration c => (List.app (insert_operation table path) (#operations c); ())
	  | XMI.Interface c => (List.app (insert_operation table path) (#operations c); ())
	  | XMI.Collection c => (List.app (insert_operation table path) (#operations c); ())
	  | XMI.Sequence c => (List.app (insert_operation table path) (#operations c); ())
	  | XMI.Set c => (List.app (insert_operation table path) (#operations c); ())
	  | XMI.Bag c => (List.app (insert_operation table path) (#operations c); ())
	  | XMI.OrderedSet c => (List.app (insert_operation table path) (#operations c); ())
	  | _ => ()
    end
	


(* recursively insert mapping of xmi.id's to model elements into Hashtable *)
fun insert_package table package_prefix (XMI.Package p) =
    let val full_name = package_prefix @ [#name p]
    in 
	List.app (insert_generalization table)           (#generalizations p);
	List.app (insert_constraint     table)           (#constraints p);
	List.app (insert_stereotype     table)           (#stereotypes p);
	List.app (insert_classifier     table full_name) (#classifiers p);
	List.app (insert_package        table full_name) (#packages p);
	List.app (insert_activity_graph table)           (#activity_graphs p);
	List.app (insert_dependency     table)           (#dependencies p);
	List.app (insert_tagdefinition  table)           (#tag_definitions p);
	List.app (insert_event          table)           (#events p);
	HashTable.insert table (#xmiid p,Package full_name)
    end 

(* We do not want the name of the model to be part of the package hierarchy, *)
(* therefore we handle the top-level model seperately                        *)
fun insert_model table (XMI.Package p) = 
    let val full_name = nil
    in 
	List.app (insert_generalization table)           (#generalizations p);
	List.app (insert_constraint     table)           (#constraints p);
	List.app (insert_stereotype     table)           (#stereotypes p);
	List.app (insert_classifier     table full_name) (#classifiers p);
	List.app (insert_package        table full_name) (#packages p);
	List.app (insert_activity_graph table)           (#activity_graphs p);
	List.app (insert_dependency     table)           (#dependencies p);
	List.app (insert_tagdefinition  table)           (#tag_definitions p);
	List.app (insert_event          table)           (#events p);
	HashTable.insert table (#xmiid p,Package full_name)
    end 


fun initial_state_of table (XMI.mk_ActivityGraph ag) = 
    valOf (List.find (fn (XMI.PseudoState{kind,...})  => kind = XMI.initial
		       | _ => false) 
		     (XMI.state_subvertices_of (#top ag)))


fun successor_states_of table (st:XMI.StateVertex) =
    map  ((find_state table) o  XMI.transition_target_of o 
	  (find_transition table)) 
	 (XMI.state_outgoing_trans_of st)   

(* returns a list of tag-value pairs *)
fun class_taggedvalues_of table (XMI.Class c) = 
    map (fn x => (find_tagdefinition table (#tag_type x),#dataValue x)) 
	(#taggedValue c)
  | class_taggedvalues_of table (XMI.AssociationClass c) = 
    map (fn x => (find_tagdefinition table (#tag_type x),#dataValue x)) 
	(#taggedValue c)
  | class_taggedvalues_of table _ = raise IllFormed "class_taggedvalues_of called on an argument that doesn't support tagged values yet..."
	

(* returns the value of the given tag *)
fun class_taggedvalue_of table tag (XMI.Class c) =
    Option.map #2 ((List.find (fn (x,y) => x=tag)) 
		       (class_taggedvalues_of table (XMI.Class c)))
  | class_taggedvalue_of table tag (XMI.AssociationClass c) =
    Option.map #2 ((List.find (fn (x,y) => x=tag)) 
		       (class_taggedvalues_of table (XMI.AssociationClass c)))
  | class_taggedvalue_of table tag _ = raise IllFormed "class_taggedvalues_of called on an argument that doesn't support tagged values yet..."
	

(* returns a list of tag-value pairs *)
fun attribute_taggedvalues_of table (a:XMI.Attribute) = 
    map (fn x => (find_tagdefinition table (#tag_type x),#dataValue x)) 
	(#taggedValue a)
 

(* returns the value of the given tag *)
fun attribute_taggedvalue_of table tag (a:XMI.Attribute) =
    Option.map #2 ((List.find (fn (x,y) => x=tag)) 
		       (attribute_taggedvalues_of table a))
	

(* returns a list of tag-value pairs *)
fun state_taggedvalues_of table st = 
    map (fn x => (find_tagdefinition table (#tag_type x),#dataValue x)) 
	(XMI.state_taggedValue_of st)

(* returns the value of the given tag *)
fun state_taggedvalue_of table tag st =
    Option.map #2 ((List.find (fn (x,y) => x=tag)) 
		       (state_taggedvalues_of table st))
    

(* returns a list of tag-value pairs *)
fun package_taggedvalues_of table (XMI.Package p) = 
    map (fn x => (find_tagdefinition table (#tag_type x),#dataValue x)) 
	(#taggedValue p)

(* returns the value of the given tag *)
fun package_taggedvalue_of table tag (XMI.Package p) =
    Option.map #2 ((List.find (fn (x,y) => x=tag)) 
		       (package_taggedvalues_of table (XMI.Package p)))
    

(* check whether a class has the given stereotype *)
fun classifier_has_stereotype t st c = 
    List.exists (fn x => (find_stereotype t x) = st) 
		(XMI.classifier_stereotype_of c)

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
		if not (Option.isSome (HashTable.find t id)) then () else 
	    let val type_of_id  = find_classifier_type t id
		val cls_of_id   = find_classifier t id
		val aends_of_id = ae::(find_aends t id)
		val ags_of_id   = find_activity_graph_of t id
		val path_of_id  = path_of_classifier type_of_id 
		val path_of_ae  = path_of_id @ [case #name ae of SOME x => x
							       | NONE   => ""]
	    in 
		(HashTable.insert t (id,Type (type_of_id,aends_of_id,cls_of_id,ags_of_id));
		 HashTable.insert t (#xmiid ae, AssociationEnd path_of_ae))
	    end
    in 
	List.app add_aend_to_type mappings
    end

(* recursively transforms all associations in the package p, *)
fun transform_associations t (XMI.Package p) = 
    (List.app (transform_associations t) (#packages p);
    List.app (transform_assocation t) (#associations p))

end
