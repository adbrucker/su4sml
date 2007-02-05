(*****************************************************************************
 *          su4sml - a SecureUML repository for SML              
 *                                                                            
 * rep_parser.sml - an xmi-parser for the import interface for su4sml
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


structure RepParser : 
          sig
              val transformXMI : XMI.XmiContent -> Rep.Classifier list
              val readFile      : string -> Rep.Classifier list
              val test: (string * string list) -> OS.Process.status
              (* generic exception if something is wrong *)
          end  =
struct
open library

open Xmi_IDTable

(** thrown when something is not yet implemented *)
exception NotYetImplemented

val triv_expr = Rep_OclTerm.Literal ("true",Rep_OclType.Boolean)

fun lowercase s = let val sl = String.explode s
		  in
		      String.implode ((Char.toLower (hd sl))::(tl sl))
		  end

(** transform an xmi ocl expression into a rep ocl term *)
fun transform_expression t (XMI.LiteralExp {symbol,expression_type}) = 
    Rep_OclTerm.Literal (symbol,find_classifier_type t expression_type)
  | transform_expression t (XMI.CollectionLiteralExp {parts,expression_type}) = 
    Rep_OclTerm.CollectionLiteral (map (transform_collection_part t) parts,
				   find_classifier_type t expression_type)
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
	val name     = #name var_dec
	val var_name = if name = "" 
		       then "anonIterVar_"^ (#xmiid var_dec)
		       else name
    in 
	Rep_OclTerm.Variable (var_name,find_classifier_type t expression_type)
    end
  | transform_expression t (XMI.AssociationEndCallExp {source, referredAssociationEnd, expression_type}) = 
    let fun find_type exp = let val cls = find_classifier_type
                                              t (XMI.expression_type_of exp)
                            in case cls of
                                   Rep_OclType.Classifier _ => cls
                                 | OclAny => find_type (XMI.expression_source_of exp)
                            end
        (* this is a bit problematic: an associationendcall should always 
         * have a (user-defined) classifier as source type. However, the 
         * atPre() operation call returns OclAny, which is not a classifier. 
         * Therefore, we look (recursively), at the source of the expression 
         * source until we find a user-defined classifier type and take this type. 
         * This works for the case of atPre(), but I'm not sure if there are other 
         * cases where this hack has unwanted consequences.
         *)
        val classifier_type = find_type source
        val path_of_classifier = Rep_OclType.path_of_OclType classifier_type
        val aend = find_associationend t referredAssociationEnd
        val aend_name = Option.getOpt(#name aend,
                                      (lowercase o XMI.classifier_name_of o
                                       find_classifier t) 
                                          (#participant_id aend))
    in Rep_OclTerm.AssociationEndCall 
           (transform_expression t source,
            classifier_type,
            path_of_classifier @ [aend_name],
            find_classifier_type t expression_type)
    end
  | transform_expression t (XMI.IteratorExp {name,iterators,body,source,expression_type}) = 
    let val _ = map (insert_variable_dec t) iterators 
    in
	Rep_OclTerm.Iterator (name,
			      map (fn x => (if #name x = ""
					    then "anonIterVar_" ^ (#xmiid x)
					    else #name x, 
					    find_classifier_type t (#declaration_type x))) iterators,
			      transform_expression t source, find_classifier_type t (XMI.expression_type_of source),
			      transform_expression t body, find_classifier_type t (XMI.expression_type_of body),
			      find_classifier_type t expression_type
			     )
    end
  | transform_expression t (XMI.IterateExp {result,iterators,body,source,expression_type}) = 
    let val _ = map (insert_variable_dec t) (result::iterators )
    in
	Rep_OclTerm.Iterate (  map (fn x => (#name x, find_classifier_type t (#declaration_type x))) iterators,
			       #name result,
			       find_classifier_type t (#declaration_type result),
			       transform_expression t (valOf (#init result)),
			       transform_expression t source, find_classifier_type t (XMI.expression_type_of source),
			       transform_expression t body, find_classifier_type t (XMI.expression_type_of body),
			       find_classifier_type t expression_type
			    )
    end
  | transform_expression t (XMI.LetExp {variable, inExpression, expression_type}) = 
    let val _ = insert_variable_dec t variable
    in 
        Rep_OclTerm.Let (#name variable, 
                         find_classifier_type t (#declaration_type variable),
                         transform_expression t (Option.valOf (#init variable)),
                         find_classifier_type t (XMI.expression_type_of 
                                                     (Option.valOf (#init variable))),
                         transform_expression t inExpression,
                         find_classifier_type t expression_type
                        )
    end
  | transform_expression t _ = error "unsupported OCL expression type"
and transform_collection_part t (XMI.CollectionItem {item,expression_type}) =
    Rep_OclTerm.CollectionItem (transform_expression t item,
				find_classifier_type t expression_type)
  | transform_collection_part t (XMI.CollectionRange {first,last,expression_type}) =
    Rep_OclTerm.CollectionRange (transform_expression t first,
				 transform_expression t last,
				 find_classifier_type t expression_type)


fun transform_constraint t ({xmiid,name,body,...}:XMI.Constraint) = 
    let val n_name = case name of 
		         (SOME s) => if (s = "") then NONE else (SOME(s))
	               | NONE     => NONE
    in	
    	(n_name,transform_expression t body)
	handle ex => (print ("Warning: in RepParser.transform_constraint: \
                             \Could not parse Constraint: "^General.exnMessage ex^"\n");
                      (NONE, triv_expr))
    end

fun transform_bodyconstraint result_type t ({xmiid,name,body,...}:XMI.Constraint) = 
    let	val result = Rep_OclTerm.Variable ("result",result_type)
	val equal = ["oclLib","OclAny","="]
	val body = transform_expression t body
	val body_type = result_type
    in 
	(SOME "body",Rep_OclTerm.OperationCall (result, result_type,
						equal,[(body,body_type)],
						Rep_OclType.Boolean))
    end
    handle ex => (print ("Warning: in RepParser.transform_bodyconstraint: \
                         \Could not parse Constraint: "^General.exnMessage ex^"\n");
                  (NONE, triv_expr))

fun transform_parameter t {xmiid,name,kind,type_id} =
    (name, find_classifier_type t type_id)

fun transform_operation t {xmiid,name,isQuery,parameter,visibility,
			   constraints,ownerScope} =
    let val result_type =  find_classifier_type t 
						((#type_id o hd) (filter (fn x => #kind x = XMI.Return) 
									 parameter))
	val checked_constraints = filter_exists t constraints
    in
	{name=name,
	 arguments = (map (transform_parameter t)
			  (filter (fn x => #kind x <> XMI.Return) parameter)),
	 precondition = (map ((transform_constraint t) o (find_constraint t)) 
			     (filter_precondition t checked_constraints)),
	 postcondition = List.concat [map ((transform_constraint t) o 
					   (find_constraint t))
					  (filter_postcondition t constraints), 
				      map ((transform_bodyconstraint result_type t) o
					   (find_constraint t))
					  (filter_bodyconstraint t checked_constraints)],
	 result = result_type,
	 visibility = visibility,
	 scope = ownerScope,
	 isQuery = isQuery      (* FIX *)
	}
    end
    

fun transform_attribute t ({xmiid,name,type_id,changeability,visibility,ordering,
			    multiplicity,taggedValue,ownerScope,targetScope,stereotype,initialValue}) =
    let val cls_type = find_classifier_type t type_id 
    in
	{name= name,
	 attr_type = if multiplicity = [(1,1)] 
		     then cls_type
		     else if ordering = XMI.Ordered then Rep_OclType.Sequence cls_type
		     else Rep_OclType.Set cls_type,
	 visibility = visibility,
	 scope = ownerScope,
	 stereotypes = map (find_stereotype t) stereotype,
	 init = Option.map (transform_expression t) initialValue
	}
    end

    
fun transform_aend t ({xmiid,name,ordering,multiplicity,participant_id,
		       isNavigable,aggregation,changeability,visibility,targetScope})
  = {name         = Option.getOpt(name,
				  (lowercase o XMI.classifier_name_of o
				   find_classifier t) participant_id),
     aend_type    = find_classifier_type t participant_id,
     multiplicity = multiplicity,
     ordered      = if ordering = XMI.Ordered then true else false,
     visibility   = visibility,
     init         = NONE (* FIX *)
    }

val filter_named_aends  = List.filter (fn {name=SOME _,...}:XMI.AssociationEnd => true
					| _ => false)

(* FIX *)
fun transform_state t (XMI.CompositeState {xmiid,outgoing,incoming,subvertex,
					   isConcurrent,name,...}) =
    Rep.State_CompositeState { name     = name,
			       state_id = xmiid,
			       outgoing = outgoing,
			       incoming = incoming,
			       subvertex = map (transform_state t) subvertex,
			       isConcurrent = isConcurrent  }
  | transform_state t (XMI.SimpleState {xmiid,outgoing,incoming,name,...}) =
    Rep.State_SimpleState { state_id = xmiid,
			    outgoing = outgoing,
			    incoming = incoming,
			    name     = name}
  | transform_state t (XMI.ActionState {xmiid,outgoing,incoming,isDynamic,
					name,...}) =
    Rep.SimpleState_ActionState { state_id = xmiid,
				  outgoing = outgoing,
				  incoming = incoming,
				  isDynamic = isDynamic,
				  name      = name}
  | transform_state t (XMI.FinalState {xmiid,incoming,...}) =
    Rep.State_FinalState { state_id = xmiid,
			   incoming = incoming}
  | transform_state t (XMI.PseudoState {xmiid,incoming,outgoing,kind,...}) = 
    Rep.PseudoState { state_id = xmiid,
		      outgoing = outgoing,
		      incoming = incoming,
		      kind = kind }
  | transform_state t _ = error ("in transform_state: unsupported StateVertex type \
                                 \(Subactivity states, object flow states and \
                                 \sync states are not supported).") 
(* a primitive hack: we take the body of the guard g as the name of an *)
(* operation to be called in order to check whether the guard is true  *)
fun transform_guard t (XMI.mk_Guard g) =
    let val self_type = Rep_OclType.DummyT (* FIX *)
	val package_path = nil (* FIX *) 
    in
	case #expression g of
	    NONE => Rep_OclTerm.OperationCall ( Rep_OclTerm.Variable ("self",self_type),
						self_type,
						List.concat [package_path,[Option.valOf(#body g)]],nil,
						Rep_OclType.Boolean )
	  | SOME exp => transform_expression t exp
    end

fun transform_event t (XMI.CallEvent ev) =
    Rep.CallEvent (find_operation t (#operation ev),
		   map (transform_parameter t) (#parameter ev))
  | transform_event t (XMI.SignalEvent ev) =
    Rep.SignalEvent (map (transform_parameter t) (#parameter ev))

fun transform_proc t (XMI.mk_Procedure proc) = 
    Rep.Proc_mk { proc_id    = #xmiid proc,
		  language   = #language proc,
		  body       = #body proc,
		  expression = #expression proc }

fun transform_transition t (XMI.mk_Transition trans) 
  = Rep.T_mk { trans_id = #xmiid trans ,
	       source = #source trans,
	       target = #target trans,
	       guard  = Option.map (transform_guard t) (#guard trans),
	       trigger = Option.map ((transform_event t) o (find_event t)) 
				    (#trigger trans),
	       effect = Option.map (transform_proc t) (#effect trans)}

fun transform_activitygraph t (XMI.mk_ActivityGraph act) = 
    Rep_StateMachine.SM_mk {top = transform_state t (#top act), 
			    transition = map (transform_transition t) (#transitions act) }

fun transform_statemachine t (XMI.mk_StateMachine st) = 
    Rep_StateMachine.SM_mk {top = transform_state t (#top st), 
			    transition = map (transform_transition t) (#transitions st) }

(** transform a XMI.Classifier classifier into a Rep.Classifier *)
fun transform_classifier t (XMI.Class {xmiid,name,isActive,visibility,isLeaf,
				       generalizations,attributes,operations,
				       invariant,stereotype,clientDependency,
				       supplierDependency,taggedValue,
				       classifierInState,activity_graphs,
				       state_machines}) =
    let val parents = map ((find_classifier_type t) o (find_parent t)) 
			  generalizations 
	val filtered_parents   = filter (fn x => x <> Rep_OclType.OclAny) parents
 	val checked_invariants = filter_exists t invariant
        val navigable_aends    = filter #isNavigable (find_aends t xmiid)
    in
	Rep.Class {name = (* path_of_classifier *) (find_classifier_type t xmiid), 
		   parent = case filtered_parents 
			     of [] => NONE
			      | xs => SOME ((* path_of_classifier *) (hd xs)),
		   attributes = map (transform_attribute t) attributes,
		   operations = map (transform_operation t) operations,
		   invariant  = map ((transform_constraint t) o 
				     (find_constraint t)) checked_invariants, 
		   associationends = map (transform_aend t) navigable_aends,
		   stereotypes = map (find_stereotype t) stereotype, 
		   interfaces = nil, (* FIX *)
                   activity_graphs = List.concat [map (transform_activitygraph t) activity_graphs,
						  map (transform_statemachine t) state_machines], 
		   thyname = NONE}
    end
  | transform_classifier t (XMI.AssociationClass {xmiid,name,isActive,visibility,
						  isLeaf,generalizations,attributes,
						  operations,invariant,stereotype,
						  clientDependency,connection,
						  supplierDependency,taggedValue}) =
    let val parents = map ((find_classifier_type t) o (find_parent t)) 
			  generalizations 
        (* FIXME: filter for classes vs. interfaces *)  
	val filtered_parents = filter (fn x => x <> Rep_OclType.OclAny) parents 
	val checked_invariants = filter_exists t invariant
    in
	Rep.Class {name = (* path_of_classifier *) (find_classifier_type t xmiid), 
		   parent = case filtered_parents 
			     of [] => NONE
			      | xs => SOME ((*path_of_classifier *) (hd xs)),
		   attributes = map (transform_attribute t) attributes,
		   operations = map (transform_operation t) operations,
		   invariant  = map ((transform_constraint t) o 
				     (find_constraint t)) checked_invariants, 
		   associationends = map (transform_aend t) 
					 (find_aends t xmiid), 
		   stereotypes = map (find_stereotype t) stereotype, 
		   interfaces = nil, (* FIX *)
                   activity_graphs = nil,
		   thyname = NONE}
    end
  | transform_classifier t (XMI.Primitive {xmiid,name,generalizations,
					   operations,invariant}) =
    let val checked_invariants = filter_exists t invariant
    in
        Rep.Primitive {name = (* case *) find_classifier_type t xmiid (*of Rep_OclType.Classifier x => x
								         | _ => raise Option*) , 
		       parent = NONE,    (* FIX *)
		       operations = map (transform_operation t) operations,
		       associationends = map (transform_aend t) 
					     (find_aends t xmiid), 
		       invariant = map ((transform_constraint t) o 
					(find_constraint t)) checked_invariants,
		       stereotypes = nil, (*FIX *)
		       interfaces = nil, (* FIX *)
		       thyname = NONE}
    end
  | transform_classifier t (XMI.Enumeration {xmiid,name,generalizations,
					     operations,literals,invariant}) =
    let val checked_invariants = filter_exists t invariant
    in
        Rep.Enumeration {name = (* case *) find_classifier_type t xmiid (* of Rep_OclType.Classifier x => x
								            | _ => raise Option *), 
			 parent = NONE,    (* FIX *)
			 literals = literals,
			 operations = map (transform_operation t) operations,
			 invariant =   map ((transform_constraint t) o 
					    (find_constraint t)) checked_invariants,
			 stereotypes = nil, (* FIX *)
			 interfaces = nil, (* FIX *)
		         thyname = NONE}
    end
  | transform_classifier t (XMI.Interface { xmiid, name, generalizations, operations, invariant,
		                            ...}) =
    let 
        val checked_invariants = filter_exists t invariant
    in 
        Rep.Interface { name        = find_classifier_type t xmiid,
	                parents     = map ((find_classifier_type t) o (find_parent t)) 
			                  generalizations,
	                operations  = map (transform_operation t) operations,
	                stereotypes = [], (* map (find_stereotype t) stereotype,*)
	                invariant   = map ((transform_constraint t) o 
					   (find_constraint t)) checked_invariants,
	                thyname     = NONE 
                      }
    end
  | transform_classifier t (_) = error "Not supported Classifier type found."
			               

(** recursively transform all classes in the package. *)
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



(** transform a UML model into a list of Rep classes.              
 *
 * 1. traverse package hierarchy and put xmi.id of all interesting 
 *    model elements into the hashtable                            
 *
 * 2. traverse again to find all associations, transform them into 
 *    association ends and map the correct classes to them         
 *    (We have to handle associations seperately because there is  
 *     no direct link from classes to their association ends in    
 *     the xmi file)                                               
 *
 * 3. traverse again, transforming all remaining model elements,   
 *    i.e., classes with their operations, attributes,             
 *    constraints, etc                                             *)
fun transformXMI ({classifiers,constraints,packages,
		   stereotypes,variable_declarations,state_machines, activity_graphs}) =
    let val (xmiid_table: (string,HashTableEntry) HashTable.hash_table) =
	    HashTable.mkTable (HashString.hashString, (op =)) (101, Option)
	(* hack: insert a dummy type into the table *)
	val _ = HashTable.insert xmiid_table ("DummyT",
                                              Type (Rep_OclType.DummyT,
                                                    nil,
                                                    XMI.Primitive{name="DummyT",
                                                                  xmiid="DummyT",
                                                                  operations=[],
                                                                  generalizations=[],
                                                                  invariant=[]},
                                                    nil))
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
	transform_package xmiid_table model (* transform classes   *)
    end


(** 
 * read and transform a .xmi file.
 * @return a list of rep classifiers, or nil in case of problems
 *) 
fun readFile f = map Rep.normalize ((transformXMI o XmiParser.readFile) f)
(*    handle ex as (IllFormed msg) => raise ex *)

fun printStackTrace e =
    let val ss = CompilerExt.exnHistory e
    in
        print_stderr ("uncaught exception " ^ (General.exnMessage e) ^ " at:\n");
        app (fn s => print_stderr ("\t" ^ s ^ "\n")) ss
    end
    
(**
 * Test function.
 *)
fun test (_,filename::_) = (Rep2String.printList (readFile filename); OS.Process.success)
    handle ex => (printStackTrace ex; OS.Process.failure)

end

