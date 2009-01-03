(*****************************************************************************
 * su4sml --- a SML repository for managing (Secure)UML/OCL models
 *             http://projects.brucker.ch/su4sml/
 *                                                                            
 * rep_parser.sml --- an xmi-parser for the import interface for su4sml
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

structure RepParser : 
          sig
              val transformXMI : XMI.XmiContent -> Rep.Classifier list
              val transformXMI_ext : XMI.XmiContent -> Rep.transform_model
              val readFile      : string -> Rep.Model
              val importArgoUML : string -> Rep.Model
        (*      val test: (string * string list) -> OS.Process.status *)
              (* generic exception if something is wrong *)
          end  =
struct

open Xmi_IDTable

(* billk_tag *)
open Rep_OclTerm
open Rep_OclType
open Rep_OclHelper
open Rep_Core

(** thrown when something is not yet implemented *)
exception NotYetImplemented

val triv_expr = Rep_OclTerm.Literal ("true",Rep_OclType.Boolean)

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
			       Xmi_IDTable.find_attribute t referredAttribute,
			       find_classifier_type t expression_type)
  | transform_expression t (XMI.OperationCallExp {source,arguments,
						  referredOperation,
						  expression_type}) =
    let val arglist = map (transform_expression t) arguments
	val argtyplist = map ((find_classifier_type t) o XMI.expression_type_of) arguments
    in
	Rep_OclTerm.OperationCall (transform_expression t source,
			           find_classifier_type t (XMI.expression_type_of source),
			           Xmi_IDTable.find_operation t referredOperation,
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
                                      (StringHandling.uncapitalize o XMI.classifier_name_of o
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
  | transform_expression t _ = Logger.error "unsupported OCL expression type"
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
	handle ex => (Logger.warn ("Warning: in RepParser.transform_constraint: \
                             \Could not parse Constraint: "^General.exnMessage ex^"\n"^
                            "using the trivial constraint 'true' instead");
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
    handle ex => (Logger.warn ("Warning: in RepParser.transform_bodyconstraint: \
                         \Could not parse Constraint: "^
                         General.exnMessage ex^"\n"^
                         "using the trivial constraint 'true' instead");
                  (NONE, triv_expr))
                 
fun transform_parameter t {xmiid,name,kind,type_id} =
    (name, find_classifier_type t type_id
           handle _ => (Logger.warn ("no type found for parameter '"^name^
                              "', defaulting to OclVoid"); 
                        Rep_OclType.OclVoid)
    )
    
fun transform_operation t {xmiid,name,isQuery,parameter,visibility,
			   constraints,ownerScope} =
    let val result_type   = (
            case filter (fn x => #kind x = XMI.Return) parameter
             of []      => (Logger.warn ("no return type found for operation '"^name^
                                  "', defaulting to OclVoid"); 
                            Rep_OclType.OclVoid)
              | [x]     => (find_classifier_type t (#type_id x)
                            handle _ => (Logger.warn ("return parameter for \
                                               \operation '"^name^
                                               "' has no declared type, \
                                               \defaulting to OclVoid"); 
                                         Rep_OclType.OclVoid))
              | x::y::_ => 
                let 
                  val ret_type = find_classifier_type t (#type_id x)
                      handle _ => (Logger.warn ("return parameter for operation '"
                                         ^name^"' has no declared type, \
                                               \defaulting to OclVoid"); 
                                   Rep_OclType.OclVoid)
                in 
                  (Logger.warn ("operation '"^name^
                         "' has multiple return parameters. Using only '"^
                         (Rep_OclType.string_of_OclType ret_type)^"'.");
                   ret_type)
                end)
                            
      val checked_constraints = filter_exists t constraints
    in
      {name=name,
       arguments = (map (transform_parameter t)
			(filter (fn x => #kind x <> XMI.Return) parameter)),
       precondition = (map ((transform_constraint t) o (find_constraint t)) 
			   (filter_precondition t checked_constraints)),
       postcondition = List.concat 
                           [map ((transform_constraint t)o(find_constraint t))
				(filter_postcondition t constraints), 
			    map ((transform_bodyconstraint result_type t) o 
                                 (find_constraint t))
				(filter_bodyconstraint t checked_constraints)],
       result = result_type,
       body = [],
       visibility = visibility,
	 stereotypes = [],      (* FIX *)
       scope = ownerScope,
       isQuery = isQuery      (* FIX *)
      }
    end
    

fun transform_attribute t ({xmiid,name,type_id,changeability,visibility,ordering,
			    multiplicity,taggedValue,ownerScope,targetScope,stereotype,initialValue}) =
    let val cls_type = find_classifier_type t type_id 
            handle _ => (Logger.warn ("no type found for attribute '"^name^
                               "', defaulting to OclVoid"); 
                         Rep_OclType.OclVoid)
    in
	{name = name,
	 attr_type = if multiplicity = [(1,1)] orelse multiplicity = [(0,1)] 
		     then cls_type
		     else if ordering = XMI.Ordered then Rep_OclType.Sequence cls_type
		     else Rep_OclType.Set cls_type,
	 visibility = visibility,
	 scope = ownerScope,
	 stereotypes = map (find_stereotype t) stereotype,
	 init = Option.map (transform_expression t) initialValue
	}
    end

fun transform_aend t assocPath ({xmiid,name,association,ordering,multiplicity,
                                 participant_id,isNavigable,aggregation,
                                 qualifier,changeability,visibility,
                                 targetScope}:XMI.AssociationEnd):
    (Rep.associationend * (string * Rep.attribute list)) =
    let
      val participant = find_classifier t participant_id
      val participantType = find_classifier_type t participant_id
      val role = if (isSome name) then valOf name
		 else 
                   let
                     val participantName = XMI.classifier_name_of participant
                   in
                     StringHandling.uncapitalize participantName
                   end
      val aendPath = assocPath@[role]
                     
    in
      ({name         = aendPath,
        aend_type    = participantType,
        multiplicity = multiplicity,
        ordered      = if ordering = XMI.Ordered then true else false,
        visibility   = visibility,
        init         = NONE (* FIXME *)
       }:associationend,
       (role, map (transform_attribute t) qualifier)
      )
    end
    
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
  | transform_state t _ = Logger.error ("in transform_state: unsupported StateVertex type \
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
    Rep.CallEvent (Xmi_IDTable.find_operation t (#operation ev),
		   map (transform_parameter t) (#parameter ev))
  | transform_event t (XMI.SignalEvent ev) =
    Rep.SignalEvent (map (transform_parameter t) (#parameter ev))

fun transform_proc t (XMI.mk_Procedure proc) = 
    { proc_id    = #xmiid proc,
      language   = #language proc,
      body       = #body proc,
      expression = #expression proc }
    
fun transform_transition t (XMI.mk_Transition trans) =
    { trans_id = #xmiid trans ,
      source   = #source trans,
      target   = #target trans,
      guard    = Option.map (transform_guard t) (#guard trans),
      trigger  = Option.map ((transform_event t) o (find_event t)) 
			   (#trigger trans),
      effect   = Option.map (transform_proc t) (#effect trans)}
    
fun transform_activitygraph t (XMI.mk_ActivityGraph act) = 
    {top        = transform_state t (#top act), 
     transition = map (transform_transition t) (#transitions act) }

fun transform_statemachine t (XMI.mk_StateMachine st) = 
    {top        = transform_state t (#top st), 
     transition = map (transform_transition t) (#transitions st) }

(** transform a XMI.Classifier classifier into a Rep.Classifier *)
fun transform_classifier t (XMI.Class {xmiid,name,isActive,visibility,isLeaf,
				       generalizations,attributes,operations,
				       invariant,stereotype,clientDependency,
				       supplierDependency,taggedValue,
				       classifierInState,activity_graphs,
				       state_machines}) =
    let 
	val _ = Logger.debug2 ("RepParser.transform_classifier: Class\n")
	val _ = Logger.debug2 ("class name: "^ name ^"\n")
        val assocs = find_classifier_associations t xmiid
	val _ = Logger.debug1 ("number of associations added: "^(Int.toString (List.length assocs))^"\n")
        val parents = map ((find_classifier_type t) o (find_parent t)) 
			  generalizations 
	val filtered_parents   = filter (fn x => x <> Rep_OclType.OclAny) parents
        val filtered_parent = case filtered_parents
                               of []  => NONE
                                | [x] => SOME x
                                | x::y::_ => (Logger.warn ("Class '"^name^"' has multiple parents."^
                                                    " Using only '"^
                                                    (Rep_OclType.string_of_OclType x)^"'."); 
                                              SOME x)
 	val checked_invariants = filter_exists t invariant
(*        val navigable_aends    = filter #isNavigable (find_aends t xmiid)*)
	val class_type = find_classifier_type t xmiid
	val _ = Logger.info ("transform_classifier: adding "^name^"\n")
	val res = 
	    Rep.Class {name = (* type_of_classifier *) class_type,
		       parent = case filtered_parents 
				 of [] => NONE
				  | xs => SOME ((* path_of_classifier *) (hd xs)),
		       attributes = map (transform_attribute t) attributes,
		       operations = map (transform_operation t) operations,
		       invariant  = map ((transform_constraint t) o 
					 (find_constraint t)) checked_invariants, 
		       (* associationends = map (transform_aend t) navigable_aends, *)
		       associations = assocs,
		       stereotypes = map (find_stereotype t) stereotype, 
		       interfaces = nil, (* FIX *)
		       visibility = visibility:Rep_Core.Visibility,
                       activity_graphs = List.concat [map (transform_activitygraph t) activity_graphs,
						      map (transform_statemachine t) state_machines], 
		       thyname = NONE}
	val _ = Logger.debug2 ("RepParser.transform_classifier\n")
    in
	res
    end
  | transform_classifier t (XMI.AssociationClass {xmiid,name,isActive,visibility,
						  isLeaf,generalizations,attributes,
						  operations,invariant,stereotype,
						  clientDependency,connection,
						  supplierDependency,taggedValue}) =
    let 
	val _ = Logger.debug2 ("RepParser.transform_classifier: AssociationClass\n")
	val _ = Logger.debug2 ("associationclass name: "^ name ^"\n")
	val (_,assocs,assoc,_,_) = find_classifier_entries t xmiid
	val _ = Logger.debug1 ("number of associations added: "^(Int.toString (List.length assocs))^"\n")
	val _ = Logger.debug1 ("ac association found: "^(Bool.toString (assoc <> []))^"\n")
	val _ = Logger.info  "associations retrieved\n"
	val parents = map ((find_classifier_type t) o (find_parent t)) 
			  generalizations 
	val _ = Logger.debug1 "parents retrieved\n"
        (* FIXME: filter for classes vs. interfaces *)  
	val filtered_parents = filter (fn x => x <> Rep_OclType.OclAny) parents 
	val checked_invariants = filter_exists t invariant
	(*val navigable_aends    = filter #isNavigable connection *)
	val class_type = find_classifier_type t xmiid
	val _ = Logger.debug1 ("transform_classifier: adding "^name^"\n")
	val res = 
	    Rep.AssociationClass {name = (* type_of_classifier *)class_type,
				  parent = case filtered_parents 
					    of [] => NONE
					     | xs => SOME ((*path_of_classifier *) (hd xs)),
				  attributes = map (transform_attribute t) attributes,
				  operations = map (transform_operation t) operations,
				  invariant  = map ((transform_constraint t) o 
						    (find_constraint t)) checked_invariants, 
				  stereotypes = map (find_stereotype t) stereotype, 
				  interfaces = nil (* FIX *),
				  thyname = NONE,
				  activity_graphs = [] (* FIXME *),
				  associations = assocs,
				  visibility = visibility,
				  association = assoc}
	val _ = Logger.debug2 ("RepParser.transform_classifier\n")
    in
	res
    end
  | transform_classifier t (XMI.Primitive {xmiid,name,generalizations,operations,invariant,taggedValue}) =
    let 
	val _ = Logger.debug2 ("RepParser.transform_classifier: Primitive\n")
	val _ = Logger.debug2 ("primitive name: "^ name ^"\n")
	val (_,assocs,_,_,_) = find_classifier_entries t xmiid
	val _ = Logger.debug1 ("number of associations added: "^(Int.toString (List.length assocs))^"\n")
	val checked_invariants = filter_exists t invariant
	val res = 
            Rep.Primitive {name = (* case *) find_classifier_type t xmiid (*of Rep_OclType.Classifier x => x
									     | _ => raise Option*) , 
			   parent = NONE (* FIX *),
			   operations = map (transform_operation t) operations,
			   associations = assocs
			 (*associations = map (transform_aend t) 
					      (find_aends t xmiid), *),
											 invariant = map ((transform_constraint t) o 
													  (find_constraint t)) checked_invariants,
			   stereotypes = nil (*FIX *),
			   interfaces = nil (* FIX *),
			   thyname = NONE}
	val _ = Logger.debug2 ("RepParser.transform_classifier\n")
    in
	res
    end
  | transform_classifier t (XMI.Enumeration {xmiid,name,generalizations,
					     operations,literals,invariant}) =
    let 
	val _ = Logger.debug2 ("RepParser.transform_classifier: Enumeration\n")
	val checked_invariants = filter_exists t invariant
	val res = 
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
	val _ = Logger.debug2 ("RepParser.transform_classifier\n")
    in
	res
    end
  | transform_classifier t (XMI.Interface { xmiid, name, generalizations, operations, invariant,
		                            ...}) =
    let 
	val _ = Logger.debug2 ("RepParser.transform_classifier: Interface\n")
        val checked_invariants = filter_exists t invariant
	val res = 
            Rep.Interface { name        = find_classifier_type t xmiid,
	                    parents     = map ((find_classifier_type t) o (find_parent t)) 
			                      generalizations,
	                    operations  = map (transform_operation t) operations,
	                    stereotypes = [], (* map (find_stereotype t) stereotype,*)
	                    invariant   = map ((transform_constraint t) o 
					       (find_constraint t)) checked_invariants,
	                    thyname     = NONE 
			  }
	val _ = Logger.debug2 ("RepParser.transform_classifier\n")
    in
	res
    end
  | transform_classifier t (_) = Logger.error "Not supported Classifier type found."
			               

(** transform an XMI.Association into a Rep.association *)
fun transform_association t ({xmiid,name,connection}:XMI.Association):
    Rep.association =
    let 
      val _ = Logger.debug2 ("RepParser.transform_association\n")
      val _ = Logger.debug2 ("transform_association xmiid: "
                                        ^xmiid^"\n")
      val associationPath = find_association_path t xmiid
      val _ = Logger.info ("transform_association path: "^(string_of_path 
                                                         associationPath)^
                     "\n")
      val _ = Logger.info ("transform_association path length: "^
                     (Int.toString (List.length associationPath)) ^"\n")
      val (associationEnds,qualifierPairs) = 
          ListPair.unzip (map (transform_aend t associationPath) connection)
      val res = 
	  {name = associationPath (* path_of_association *),
	   aends = associationEnds,
	   qualifiers = qualifierPairs,
	   aclass = NONE (* regular association *)}
      val _ = Logger.debug2 ("RepParser.transform_association\n")
    in
	res
    end

fun transformAssociationFromAssociationClass t (XMI.AssociationClass 
                                                    {xmiid,connection,...}):
    Rep.association =
    let
      val _ = Logger.debug2 ("RepParser.transformAssociationFromAassociation Class\n")
      val id = xmiid^"_association"
      val associationPath = find_association_path t id
      val _ = Logger.debug4 ("transform_association path: "^
                         (string_of_path associationPath)^"\n")
      val _ = Logger.debug4 ("transform_association path length: "^
                         (Int.toString (List.length associationPath)) ^"\n")
      val (associationEnds,qualifierPairs) = 
          ListPair.unzip (map (transform_aend t associationPath) connection)
      val aClass =  SOME (path_of_OclType (find_classifier_type t xmiid))
      val _  = Logger.debug2 ("RepParser.transformAssociationFromAssociationClass\n")
    in
      {name = associationPath (* path_of_association *),
       aends = associationEnds,
       qualifiers = qualifierPairs,
       aclass = aClass}:Rep.association
    end

(** recursively transform all classes in the package. *)
fun transform_package t (XMI.Package p) :transform_model =
    let 
      (* we do not transform the ocl library *)
      val _ = Logger.debug2 ("RepParser.transform_package\n")
      val filteredPackages = 
          filter (fn (XMI.Package x) => 
		     ((#name x <> "oclLib") andalso (#name x <> "UML_OCL")))
		 (#packages p)
      val aClasses = filter (fn (XMI.AssociationClass _ ) => true 
			      | _ => false ) (#classifiers p)
      val local_associations = 
          map (transform_association t) (#associations p) @
	  (map (transformAssociationFromAssociationClass t) aClasses)
      val local_classifiers = map (transform_classifier t) (#classifiers p)
      val (res_classifiers,res_associations) = 
          ListPair.unzip (map (transform_package t) filteredPackages)
      val associations = local_associations @ (List.concat res_associations)
      val classifiers =local_classifiers @ (List.concat res_classifiers)
      val res  = (classifiers, associations )
      val _ = Logger.debug2 ("RepParser.transform_package\n")
    in
	res
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
fun transformXMI_ext ({classifiers,constraints,packages,stereotypes,
                       variable_declarations,state_machines, 
                       activity_graphs}):transform_model =
    let 
      val (xmiid_table: (string,HashTableEntry) HashTable.hash_table) =
	  HashTable.mkTable (HashString.hashString, (op =)) (101, Option)
      (* hack: insert a dummy type into the table *)
      val _ = HashTable.insert xmiid_table 
                               ("DummyT",
                                Type (Rep_OclType.DummyT,nil,nil,
                                      XMI.Primitive{name="DummyT",
                                                    xmiid="DummyT",
                                                    operations=[],
                                                    generalizations=[],
                                                    invariant=[],
						    taggedValue=[]},
                                      nil))
     (* arbitrary startnumber *)
      val _ = HashTable.insert xmiid_table ("-1",UniqueName(123456))
     (* for some reasons, there are model elements outside of the top-level *)      (* model the xmi-file. So we have to handle them here seperately:      *)
      val _ = map (insert_classifier xmiid_table nil) classifiers
      val _ = map (insert_constraint xmiid_table) constraints
      val _ = map (insert_stereotype xmiid_table) stereotypes
      val _ = map (insert_variable_dec xmiid_table) variable_declarations
      (* "hd packages" is supposed to be the first model in the xmi-file *)
      val model = hd packages
                  
      fun test2 (classifiers,associations) =
	  let
	    val _ = Logger.info "test2\n"
	    val _ = Logger.info "classifiers\n"
	    val _ = map (Logger.info o (fn x => x^"\n")  o string_of_path o name_of) 
                        classifiers
	    val _ = Logger.info "associations\n"
	    val _ = map (Logger.info o (fn x => x^"\n")  o string_of_path o 
                         (fn {name,aends,qualifiers,aclass} => name)) 
                        associations
	    val _ = Logger.info "operations\n"
	    fun printClassifier cls = 
		let
		  val _ = Logger.info ("output of transformXMI_ext:\n")
		  val _ = Logger.info ("classifier: "^ (string_of_path (name_of cls))
                                 ^"\n") 
		  val _ = Logger.info ("associations: \n")
		  val _ = map (Logger.info o(fn x => x ^ "\n") o string_of_path ) 
                              (associations_of cls)
		  val _ = Logger.info ("operations: \n")
		  val _ = map (Logger.info o (fn {name,...} => name)) 
                              (operations_of cls) 
		in
		  print "\n"
		end
	    val _ = map printClassifier classifiers
	  in
	    Logger.debug2 "\n### transformXMI_ext done\n\n";
	    (classifiers,associations)
	  end
    in 
      insert_model xmiid_table model        (* fill xmi.id table *);
      fix_associations xmiid_table model    (* handle associations *);  
      test2 (transform_package xmiid_table model) (* transform classifiers *)
    end

fun transformXMI x:Classifier list = fst (transformXMI_ext x)


(** 
 * read and transform a .xmi file.
 * @return a list of rep classifiers, or nil in case of problems
 *) 

fun normalize_ext ((clsses,accs):transform_model):Rep.Model =
    (map (Rep.normalize accs) clsses,accs)

fun readFile f = (Logger.info ("opening "^f);
                  (normalize_ext o transformXMI_ext o XmiParser.readFile) f)
(*    handle ex as (IllFormed msg) => raise ex *)


exception FileNotFound of string

fun importArgoUML file =
    let 
        fun basename  f = ((hd o rev) o (String.fields (fn x => x = #"/"))) f
	    
	val tmpFile = OS.FileSys.tmpName ()
	val base =  if String.isSuffix ".zargo" file 
		    then String.substring(file,0, (String.size file) -6)
		    else file
        val _ = print ("*** Syscall: unzip -p -ca "^base^".zargo "^(basename base)^".xmi > "^tmpFile^"\n")
        val _ = OS.Process.system ("unzip -p -ca "^base^".zargo "^(basename base)^".xmi > "^tmpFile)
	val model = readFile tmpFile
	val _ = OS.FileSys.remove tmpFile

    in
        model
    end





    
(****************************************************
 *****************************************************
 * Test function.
 *)
(* fun test (_,filename::_) = (Rep2String.printList (fst (readFile filename)); OS.Process.success) *)
(*     handle ex => (printStackTrace ex; OS.Process.failure) *)



end


