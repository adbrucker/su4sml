(*****************************************************************************
 * su4sml --- an SML repository for managing (Secure)UML/OCL models
 *             http://projects.brucker.ch/su4sml/
 *                                                                            
 * xmi_idtable.sml --- 
 * This file is part of su4sml.
 *
 * Copyright (c) 2005-2007 ETH Zurich, Switzerland
 *               2008-2009 Achim D. Brucker, Germany
 *
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *
 *     * Redistributions in binary form must reproduce the above
 *       copyright notice, this list of conditions and the following
 *       disclaimer in the documentation and/or other materials provided
 *       with the distribution.
 *
 *     * Neither the name of the copyright holders nor the names of its
 *       contributors may be used to endorse or promote products derived
 *       from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 ******************************************************************************)
(* $Id$ *)

structure Xmi_IDTable = 
struct
open Rep_Helper

(**
 * special keys:
 *    - aends introduced to handle the implicit aend of association 
 *      classes for their association: 
 *      (xmiid of the association class)_aend
 *    - associations split off from their association class:
 *      (xmiid of the association class)_association
 * 
 * 
 * 
 * 
 *)
datatype HashTableEntry = Package of Rep_OclType.Path
		        | Type of (Rep_OclType.OclType * 
				   (*(XMI.AssociationEnd list)* *)
				   (Rep_OclType.Path list)*  (* associations *)
				   (Rep_OclType.Path)*       (* association of an association class *)
				   XMI.Classifier * 
				   (XMI.ActivityGraph list)) 
			| Association of (Rep_OclType.Path * 
					 XMI.Association)
                        | Generalization of (string * string) 
			| Constraint of XMI.Constraint 
		        | Stereotype of string
		        | Variable of XMI.VariableDeclaration 
			| Attribute of Rep_OclType.Path
			| AssociationEnd of (Rep_OclType.Path *
					    XMI.AssociationEnd)
			| Operation of Rep_OclType.Path
			| State of XMI.StateVertex
			| Transition of XMI.Transition 
		        | Dependency of XMI.Dependency
		        | TagDefinition of string
		        | ClassifierInState of string 
		        | Event of XMI.Event
			| UniqueName of int (* xmiid=-1;for naming when a name is missing *)
 
fun next_unique_name t:string=
    let
	val number = case valOf (HashTable.find t "-1")
		      of UniqueName x => x
		       | _ => raise Option
    in
	(HashTable.insert t ("-1",UniqueName (number+1));
	 Int.toString number
	)
    end 
    handle Option => Logger.error ("expected UniqueName to be defined in table")
 
fun find_tagdefinition t xmiid =
    (case valOf (HashTable.find t xmiid) 
      of TagDefinition x => x
       | _                => raise Option) 
    handle Option => Logger.error ("expected TagDefinition "^xmiid^" in table")

fun find_state t xmiid = 
    (case valOf (HashTable.find t xmiid) 
      of State x => x
       | _                => raise Option) 
    handle Option => Logger.error ("expected State "^xmiid^" in table")

fun find_event t xmiid = 
    (case valOf (HashTable.find t xmiid) 
      of Event x => x
       | _                => raise Option) 
    handle Option => Logger.error ("expected Event "^xmiid^" in table")

fun find_transition t xmiid = 
    (case valOf (HashTable.find t xmiid) 
      of Transition x => x
       | _                => raise Option) 
    handle Option => Logger.error ("expected Transition "^xmiid^" in table")

fun find_dependency t xmiid = 
    (case valOf (HashTable.find t xmiid) 
      of Dependency x => x
       | _                => raise Option) 
    handle Option => Logger.error ("expected Dependency "^xmiid^" in table")

fun find_generalization t xmiid = 
    (case valOf (HashTable.find t xmiid) 
      of Generalization x => x
       | _                => raise Option) 
    handle Option => Logger.error ("expected Generalization "^xmiid^" in table")

fun find_stereotype t xmiid =
    (case valOf (HashTable.find t xmiid) 
      of Stereotype x => x
       | _                => raise Option) 
    handle Option => Logger.error ("expected Stereotype "^xmiid^" in table")

fun find_attribute t xmiid =
    (case valOf (HashTable.find t xmiid) 
      of Attribute x => x
       | _                => raise Option) 
    handle Option => Logger.error ("expected Attribute "^xmiid^" in table")

fun find_operation t xmiid =
    (case valOf (HashTable.find t xmiid) 
      of Operation x => x
       | _                => raise Option) 
    handle Option => Logger.error ("expected Operation "^xmiid^" in table")

fun find_type t xmiid = 
    (case valOf (HashTable.find t xmiid) 
      of Type x  => x
       | _                => raise Option) 
    handle Option => Logger.error ("expected Type "^xmiid^" in table (find_type)")

fun find_assoc t xmiid = 
    (case valOf (HashTable.find t xmiid) 
      of (Association(path,assoc))  => assoc
       | _                => raise Option) 
    handle Option => Logger.error ("expected Type "^xmiid^" in table (find_assocs)")

fun find_aend t xmiid =
    (case valOf (HashTable.find t xmiid) 
      of (AssociationEnd(path,aend)) => aend
       | _                => raise Option) 
    handle Option => Logger.error ("expected AssociationEnd "^xmiid^" in table (find_aend)")

fun find_variable_dec t xmiid =
    (case valOf (HashTable.find t xmiid) 
      of Variable x => x
       | _                => raise Option) 
    handle Option => Logger.error ("expected VariableDeclaration "^xmiid^" in table")

fun find_parent t xmiid = #2 (find_generalization t xmiid)

fun find_package t xmiid  = 
    (case valOf (HashTable.find t xmiid) 
      of Package path => path
       | _                => raise Option) 
    handle Option => Logger.error ("expected Path "^xmiid^" in table")
					
fun path_of_classifier (Rep_OclType.Classifier x) = x
  | path_of_classifier _ = Logger.error ("path_of_classifier called on non-Classifier argument")

fun find_constraint t xmiid =
    (case valOf (HashTable.find t xmiid) 
      of Constraint c => c
       | _                => raise Option) 
    handle Option => Logger.error ("expected Constraint "^xmiid^" in table")

fun find_associationend t xmiid  = 
    (case valOf (HashTable.find t xmiid) 
      of AssociationEnd (path,ae) => ae
       | _                   => raise Option) 
    handle Option => Logger.error ("expected AssociationEnd "^xmiid^" in table")


fun path_of_association t xmiid  = 
    (case valOf (HashTable.find t xmiid) 
      of Association (path,ae) => path
       | _                   => raise Option) 
    handle Option => Logger.error ("expected Association "^xmiid^" in table") 

fun path_of_associationend t xmiid  = 
    (case valOf (HashTable.find t xmiid) 
      of AssociationEnd (path,ae) => path
       | _                   => raise Option) 
    handle Option => Logger.error ("expected AssociationEnd "^xmiid^" in table") 


fun find_association t xmiid  = 
    (case valOf (HashTable.find t xmiid) 
      of Association (p,a) => a
       | _                   => raise Option) 
    handle Option => Logger.error ("expected Association "^xmiid^" in table")


fun find_association_of_associationend t xmiid =
    let 
	val aend = find_associationend t xmiid
	val assoc_xmiid = #association aend
    in
	find_association t assoc_xmiid
    end

fun find_classifier_associations t xmiid =
    (case valOf (HashTable.find t xmiid) 
      of Type(_,assocs,_,_,_) => assocs
       | _                   => raise Option) 
    handle Option => Logger.error ("expected Association "^xmiid^" in table")
		     
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

fun find_classifier_entries t xmiid =
    let
	val _ = Logger.debug2 "Xmi_IDTable.find_classifier_entries \n"
	val res = (case valOf (HashTable.find t xmiid) of 
		      Type c => c
		    | _  => raise Option
		  ) handle Option => Logger.error ("expected Classifier "^xmiid^" in table (in find_classifer_entries)")
	val _ = Logger.debug2 "end Xmi_IDTable.find_classifiers_entries \n"
    in
	res
    end
									  
fun find_classifier t xmiid =
    (case valOf (HashTable.find t xmiid) 
      of Type (_,_,_,c,_) => c
       | _                => raise Option) 
    handle Option =>  Logger.error ("expected Classifier with xmiid '"^xmiid^"' in table (in find_classifer)")
	 

fun exists_classifier t xmiid =
    (case valOf (HashTable.find t xmiid) 
      of Type (_,_,_,_,_) => true
       | _                => false) 

fun find_classifierInState_classifier t cis_id =
    (case valOf (HashTable.find t cis_id)
      of ClassifierInState c => find_classifier t c
       | Type (_,_,_,c,_)      => c
	   | _                   => raise Option)
    handle Option =>  Logger.error ("expected ClassifierInState "
				       ^cis_id^" in table")
fun find_association_of_associationclass t xmiid =
    (case valOf (HashTable.find t xmiid) 
      of Type (_,_,ac,_,_) => ac
       | _                => raise Option) 
    handle Option => Logger.error ("expected associationclass "^xmiid^" in table (in find_association_of_associationclass)") 

fun find_activity_graph_of t xmiid = 
    (case valOf (HashTable.find t xmiid) 
      of Type (_,_,_,_,ag) => ag
       | _                => raise Option) 
    handle Option => Logger.error ("expected Classifier "^xmiid^" in table (in find_activity_graph_of)")

			
fun find_classifier_type t xmiid
  = let val ocltype = case valOf (HashTable.find t xmiid) of (Type (x,xs,ac,_,_)) => x
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
		      (* <ArgoUML-Hack> *)
		      | Rep_OclType.Collection (Rep_OclType.Classifier c) => 
			let val _ = Logger.warn ("ArgoUML workaround for supporting collection types ...")
			in Rep_OclType.Collection (Rep_OclType.Classifier c) end
		      | Rep_OclType.Sequence   (Rep_OclType.Classifier c) => 
			let val _ = Logger.warn ("ArgoUML workaround for supporting collection types ...")
			in Rep_OclType.Sequence (Rep_OclType.Classifier c) end 
		      | Rep_OclType.Set        (Rep_OclType.Classifier c) => 
			let val _ = Logger.warn ("ArgoUML workaround for supporting collection types ...")
			in Rep_OclType.Set (Rep_OclType.Classifier c) end
		      | Rep_OclType.Bag        (Rep_OclType.Classifier c) => 
			let val _ = Logger.warn ("ArgoUML workaround for supporting collection types ...")
			in Rep_OclType.Bag (Rep_OclType.Classifier c) end 
		      | Rep_OclType.OrderedSet (Rep_OclType.Classifier c) => 
			let val _ = Logger.warn ("ArgoUML workaround for supporting collection types ...")
			in Rep_OclType.OrderedSet (Rep_OclType.Classifier c) end 
		      (* </ArgoUML-Hack> *)
		      | _ => Logger.error ("unexpected Classifier-Type "^xmiid^" in table")
	 end
	handle Option => Logger.error ("expected Classifier with xmiid '"^xmiid^"' in table (in find_classifer_type)")		    

fun find_association_path t xmiid = 
    case valOf (HashTable.find t xmiid) of (Association (x,xs)) => x
					 | _  => raise Option
    handle Option => Logger.error ("expected Association "^xmiid^" in table (in find_association_path)")	    

fun find_association_name t xmiid = 
    case valOf (HashTable.find t xmiid) of (Association (_,{xmiid,name,connection})) => name
					 | _  => raise Option
    handle Option => Logger.error ("expected Association "^xmiid^" in table (in find_association_name)")

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
	  of (Type (c,xs,assocs,ac,ags))  => HashTable.insert 
						 table (context, Type (c,xs,assocs,ac,
XMI.mk_ActivityGraph ag::ags))
	   | _                => raise Option) 
	      handle Option => Logger.error ("expected Type "^context^" in table (insert_activity_graph)");
	List.app (insert_transition table) (#transitions ag);
	insert_state table (#top ag)
    end

fun insert_dependency table dep =
    HashTable.insert table (#xmiid dep, Dependency dep)

fun insert_tagdefinition table (td:XMI.TagDefinition) =
    HashTable.insert table (#xmiid td, TagDefinition (#name td))

fun insert_classifierInState table cls_id cis_id =
    HashTable.insert table (cis_id,ClassifierInState cls_id)

(** insert an association into the hashtable *)
fun insert_association table package_prefix (association:XMI.Association) = 
    let 
	val _ = Logger.debug2 ("Xmi_IDTable.insert_association\n")
	val id      = #xmiid association
	val name    = #name association
	val path    = if (isSome name) then package_prefix@[valOf name]
		      else package_prefix@["association_"^(next_unique_name table)]
	val res = HashTable.insert table (id,Association(path,association))
	val _ = Logger.debug2  ("Xmi_IDTable.insert_association\n")
    in
	res
	
    end


fun insert_classifier table package_prefix class = 
    let val _ = Logger.debug2 ("Xmi_IDTable.insert_classifier\n")
	val id      = XMI.classifier_xmiid_of class
	val name    = XMI.classifier_name_of class
	val path    = package_prefix @ [name]
		      
	fun argoUMLWorkaround name = (* ugly hack *) 
	    let
	      val _ = Logger.warn ("Warning: argument ("^name
			     ^") is not a collection value,\n"
			     ^"assuming ArgoUML workaround for supporting collection types ...")
	      val [colType,elementtype,tail] = String.fields (fn c => c = #"(" orelse c= #")") name
		  handle _ =>  ["","",""]
	    in 
	      if elementtype = "" 
	      then NONE 
	      else SOME (Rep_OclType.Classifier (String.fields (fn c => #"."  = c) elementtype))
	    end  
	    
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
			   else if String.isPrefix "Collection(" name 
			   then Rep_OclType.Collection (Rep_OclType.Classifier [
							XMI.classifier_elementtype_of class])
				handle ex => case argoUMLWorkaround name of 
					      SOME c => Rep_OclType.Collection (c)
					    | NONE => raise ex
			   else if String.isPrefix "Sequence("   name 
			   then Rep_OclType.Sequence   (Rep_OclType.Classifier [
							XMI.classifier_elementtype_of class])
				handle ex => case argoUMLWorkaround name of 
					      SOME c => Rep_OclType.Sequence c
					    | NONE => raise ex
			   else if String.isPrefix "Set("        name 
			   then Rep_OclType.Set        (Rep_OclType.Classifier [
							XMI.classifier_elementtype_of class])
				handle ex => case argoUMLWorkaround name of 
					      SOME c => Rep_OclType.Set c
					    | NONE => raise ex
			   else if String.isPrefix "Bag("        name 
			   then Rep_OclType.Bag        (Rep_OclType.Classifier [
							XMI.classifier_elementtype_of class])
				handle ex => case argoUMLWorkaround name of 
					      SOME c => Rep_OclType.Bag c
					    | NONE => raise ex
			   else if String.isPrefix "OrderedSet(" name 
			   then Rep_OclType.OrderedSet (Rep_OclType.Classifier [
							XMI.classifier_elementtype_of class])
				handle ex => case argoUMLWorkaround name of 
					      SOME c => Rep_OclType.OrderedSet c
					    | NONE => raise ex
			   else Logger.error ("didn't recognize ocltype "^name) 
		      else Rep_OclType.Classifier path
	(* This function is called before the associations are handled, *)
	(* so we do not have to take care of them now...                *)
	val assocs = nil 
	(* The association denoted by an association class, however, is *)
	(* known at this time *)
	val (acAssoc,acPath) = case class of
				   XMI.AssociationClass c => 
				   let 
				       val acAssocName =  name^"_association_"^(next_unique_name table)
				       val acAssoc = {xmiid = id^"_association",
						      name = SOME acAssocName,
						      connection = #connection c}:XMI.Association
				   in
				       ([acAssoc],package_prefix @[acAssocName])
				   end
				 | _ => ([],[])
	val ag    = nil
	val res = 
	    let
		val _ = HashTable.insert table (id,Type (ocltype,assocs,acPath,class,ag))
	    in
		(case class 
		  of XMI.Class c => (Logger.debug2 "insert_classifier: Class\n";
				     List.app (insert_attribute table path) (#attributes c);
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
		   | XMI.AssociationClass c => (Logger.debug2 "insert_classifier: AssociationClass\n";
						List.app (insert_attribute table path) (#attributes c);   
						List.app (insert_operation table path) (#operations c);              
						List.app (insert_classifierInState table id) [];
						insert_association table package_prefix (hd acAssoc);
						()
					       )
		   | _ => ()
		)
	    end
	val _ = Logger.debug2 ("end Xmi_IDTable.insert_classifier \n")
    in
	res
    end
    
    
(* recursively insert mapping of xmi.id's to model elements into Hashtable *)
fun insert_package table package_prefix (XMI.Package p) =
    let
	val _ = Logger.debug2 ("Xmi_IDTable.insert_package\n")
	val full_name = package_prefix @ [#name p]
	val res = 
	    let
		val _ = List.app (insert_generalization table)           (#generalizations p)
		val _ = Logger.debug3 "insert_package: constraints\n"
		val _ = List.app (insert_constraint     table)           (#constraints p)
		val _ = Logger.debug3 "insert_package: stereotypes\n"
		val _ = List.app (insert_stereotype     table)           (#stereotypes p)
		val _ = Logger.debug3 "insert_package: classifiers\n"
		val _ = List.app (insert_classifier     table full_name) (#classifiers p)
		val _ = Logger.debug3 "insert_package: associations\n"
		val _ = List.app (insert_association    table full_name) (#associations p)
		val _ = Logger.debug3 "insert_package: packages\n"
		val _ = List.app (insert_package        table full_name) (#packages p)
		val _ = Logger.debug3 "insert_package: activity_graphs\n"
		val _ = List.app (insert_activity_graph table)           (#activity_graphs p)
		val _ = Logger.debug3 "insert_package: dependencies\n"
		val _ = List.app (insert_dependency     table)           (#dependencies p)
		val _ = Logger.debug3 "insert_package: tag defenitions\n"
		val _ = List.app (insert_tagdefinition  table)           (#tag_definitions p)
		val _ = Logger.debug3 "insert_package: events\n"
		val _ = List.app (insert_event          table)           (#events p)
		val _ = Logger.debug3 "insert_package: insert package\n"
	    in
		HashTable.insert table (#xmiid p,Package full_name)
	    end
	val _ = Logger.debug2 ("Xmi_IDTable.insert_package \n")
    in
	res
    end 
    
(* We do not want the name of the model to be part of the package hierarchy, *)
(* therefore we handle the top-level model seperately                        *)
fun insert_model table (XMI.Package p) = 
    let 
	val _ = Logger.debug2 ("insert_model\n")
	val full_name = nil
	val res = 
	    let
		val _ = List.app (insert_generalization table)           (#generalizations p)
		val _ = List.app (insert_constraint     table)           (#constraints p)
		val _ = List.app (insert_stereotype     table)           (#stereotypes p)
		val _ = List.app (insert_classifier     table full_name) (#classifiers p)
		val _ = List.app (insert_association    table full_name) (#associations p)
		val _ = List.app (insert_package        table full_name) (#packages p)
		val _ = List.app (insert_activity_graph table)           (#activity_graphs p)
		val _ = List.app (insert_dependency     table)           (#dependencies p)
		val _ = List.app (insert_tagdefinition  table)           (#tag_definitions p)
		val _ = List.app (insert_event          table)           (#events p)
	    in
		HashTable.insert table (#xmiid p,Package full_name)
	    end
	val _ = Logger.debug2 ("Xmi_IDTable.insert_model\n")
    in
	res
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
  | class_taggedvalues_of table (XMI.Primitive c) =
    map (fn x => (find_tagdefinition table (#tag_type x),#dataValue x))
        (#taggedValue c)
  | class_taggedvalues_of table _ = Logger.error "in class_taggedvalues_of: \
                                          \argument doesn't support tagged values"
	

(* returns the value of the given tag *)
fun class_taggedvalue_of table tag (XMI.Class c) =
    Option.map #2 ((List.find (fn (x,y) => x=tag)) 
		       (class_taggedvalues_of table (XMI.Class c)))
  | class_taggedvalue_of table tag (XMI.AssociationClass c) =
    Option.map #2 ((List.find (fn (x,y) => x=tag)) 
		       (class_taggedvalues_of table (XMI.AssociationClass c)))
  | class_taggedvalue_of table tag (XMI.Primitive c) =
    Option.map #2 ((List.find (fn (x,y) => x=tag))
                       (class_taggedvalues_of table (XMI.Primitive c)))
  | class_taggedvalue_of table tag _ = Logger.error "in class_taggedvalues_of: \
                                             \argument doesn't support tagged values"
	

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


fun fix_associationend t (assoc_path:Rep_OclType.Path) (aend:XMI.AssociationEnd) =
    let	
	val id = #xmiid aend
	val participant_id = #participant_id aend
	val (cls_type,assocs,assoc,cls,ags)  = find_classifier_entries t participant_id
        val name = Option.getOpt(#name aend,
				 (StringHandling.uncapitalize o XMI.classifier_name_of o
				  find_classifier t) participant_id)
    in 
	((if not (List.exists (fn x => x = assoc_path) assocs) then 
	    (* add the association to the participant *)
	    (HashTable.insert t (participant_id, Type (cls_type,assoc_path::assocs,assoc,cls,ags)))
	  else 
	      ());
         HashTable.insert t (#xmiid aend, AssociationEnd (List.concat [assoc_path, [name]], aend)))
    end

(** 
 * This handles only regular associations. An association class's belonging
 * association is handled at insert_classifier. However, the normal classes
 * that part of that association class's association still need to add the 
 * association to their list of associations.
 * Traverse the list of aends and update all listed classifiers with the path
 * of the current association.
 *)
fun fix_association t (assoc as {xmiid,name,connection}:XMI.Association) =
    let
	fun updateTableEntry t pathUpdate typeId =
	    let
		val (oclType,associations,association,class,ag) = find_type t typeId
		(* prefix version refused to work *)
		fun existsPath assocList = List.exists (fn x => x=pathUpdate) assocList
		val modifiedAssociations = if(existsPath associations)
					   then
					       associations
					   else
					       pathUpdate::associations					       					       
	    in
		HashTable.insert t (typeId,Type(oclType,modifiedAssociations,association,class,ag))
	    end
	val assocPath = find_association_path t xmiid
	val participantIds = map (#participant_id) connection
    in
	List.app (updateTableEntry t assocPath) participantIds
    end

fun fixAssociationFromAsssociationClass table (XMI.AssociationClass{xmiid,
								    ...}) =
    let
	val association = find_association table (xmiid ^ "_association")
    in
	fix_association table association
    end

(** 
 * Handel the broken association references, meaning updating the 
 * association path list for classes and association classes.
 * Since classifiers do not store their belonging aends, we traverse the 
 * associations: 
 *         We skip the aends and instead process the set of associations. For
 *         each association, we traverse the connection part and search for 
 *         the classifier listed as participant_id. Then we simply add the 
 *         association path to the found classifier.
 * For the classifiers part of an association class's class, no association
 * construct is present in the package p, while those constructs are already
 * in the hashtable. To traverse them as well, we extract all association
 * classes and reconstruct the associations.
 *)
fun fix_associations t (XMI.Package p) =
    let
	    val associationClasses = filter (fn (XMI.AssociationClass x) => true
					                              | _ => false) (#classifiers p)
    in
	    (* All association ends are stored in associations, so we will 
	     * traverse them an update affected Classes and AssociationClasses *)
	     (List.app (fix_associations t) (#packages p);
	      List.app (fix_association t) (#associations p);
	      List.app (fixAssociationFromAsssociationClass t) associationClasses)
    end


end
