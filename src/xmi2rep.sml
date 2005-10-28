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
open library
exception IllFormed of string

open Xmi_IDTable

exception NotYetImplemented




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
			   constraints,ownerScope} =
    {name=name,
     arguments = map (transform_parameter t)
		     (filter (fn x => #kind x <> XMI.Return) parameter),
     precondition = map ((transform_constraint t) o (find_constraint t)) 
			(filter_precondition t constraints),
     postcondition = map ((transform_constraint t) o (find_constraint t)) 
			 (filter_postcondition t constraints), 
     result = find_classifier_type t ((#type_id o hd)(filter (fn x => #kind x = XMI.Return) parameter)),
     visibility = visibility,
     scope = ownerScope,
     isQuery = isQuery      (* FIX *)
     }

     

fun transform_attribute t ({xmiid,name,type_id,changeability,visibility,ordering,
			    multiplicity,taggedValue,ownerScope,targetScope}) =
    let val cls_type = find_classifier_type t type_id 
    in
	(name,
	 if multiplicity = [(1,1)] 
	 then cls_type
	 else if ordering = XMI.Ordered then Rep_OclType.Sequence cls_type
	 else Rep_OclType.Set cls_type,
	 visibility,
	 ownerScope
	 )
    end

fun transform_aend t ({xmiid,name,ordering,multiplicity,participant_id,
		       isNavigable,aggregation,changeability,visibility,targetScope})
  = {name         = valOf name,
     aend_type    = find_classifier_type t participant_id,
     multiplicity = multiplicity,
     ordered      = if ordering = XMI.Ordered then true else false,
     visibility   = visibility }

val filter_named_aends  = List.filter (fn {name=SOME _,...}:XMI.AssociationEnd => true
					| _ => false)

(* FIX *)
fun transform_state t (XMI.CompositeState {xmiid,outgoing,incoming,subvertex,
					   isConcurrent,...}) =
    Rep.State_CompositeState {state_id = xmiid,
			      outgoing = outgoing,
			      incoming = incoming,
			      subvertex = map (transform_state t) subvertex,
			      isConcurrent = isConcurrent  }
  | transform_state t (XMI.SimpleState {xmiid,outgoing,incoming,...}) =
    Rep.State_SimpleState {state_id = xmiid,
			   outgoing = outgoing,
			   incoming = incoming }
  | transform_state t (XMI.ActionState {xmiid,outgoing,incoming,isDynamic,...}) =
    Rep.SimpleState_ActionState {state_id = xmiid,
				 outgoing = outgoing,
				 incoming = incoming,
				 isDynamic = isDynamic}
  | transform_state t (XMI.FinalState {xmiid,incoming,...}) =
    Rep.State_FinalState {state_id = xmiid,
			  incoming = incoming}
  | transform_state t (XMI.PseudoState {xmiid,incoming,outgoing,kind,...}) = 
    Rep.PseudoState {state_id = xmiid,
		     outgoing = outgoing,
		     incoming = incoming,
		     kind = kind }

fun transform_transition t (XMI.mk_Transition trans) 
  = Rep.T_mk { trans_id = #xmiid trans ,
	       source = #source trans,
	       target = #target trans,
	       guard  = NONE, (* FIX *)
	       trigger = NONE, (* FIX *)
	       effect = NONE} (* FIX *)

fun transform_activitygraph t (XMI.mk_ActivityGraph act) = 
    Rep_StateMachine.SM_mk {top = transform_state t (#top act), 
			    transition = map (transform_transition t) (#transitions act) }

fun transform_classifier t (XMI.Class {xmiid,name,isActive,visibility,isLeaf,
					   generalizations,attributes,operations,
					   invariant,stereotype,clientDependency,
					   supplierDependency,taggedValue,
					   classifierInState,activity_graphs}) =
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
			stereotypes = map (find_stereotype t) stereotype, 
			interfaces = nil, (* FIX *)
                        activity_graphs = map (transform_activitygraph t) activity_graphs, 
			thyname = NONE}
    end
  | transform_classifier t (XMI.AssociationClass {xmiid,name,isActive,visibility,
						  isLeaf,generalizations,attributes,
						  operations,invariant,stereotype,
						  clientDependency,connection,
						  supplierDependency,taggedValue}) =
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
			stereotypes = map (find_stereotype t) stereotype, 
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
	val _ = HashTable.insert xmiid_table ("DummyT",Type (Rep_OclType.DummyT,nil,XMI.Primitive{name="DummyT",xmiid="DummyT",operations=[],generalizations=[],invariant=[]},nil))
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




