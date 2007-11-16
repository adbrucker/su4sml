(*****************************************************************************
 * su4sml --- a SML repository for managing (Secure)UML/OCL models
 *             http://projects.brucker.ch/su4sml/
 *                                                                            
 * rep_parser.sml --- an xmi-parser for the import interface for su4sml
 * This file is part of su4sml.
 *
 * Copyright (c) 2005-2007, ETH Zurich, Switzerland
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
              val test: (string * string list) -> OS.Process.status
              (* generic exception if something is wrong *)
          end  =
struct
open library

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
    handle ex => (print ("Warning: in RepParser.transform_bodyconstraint: \
                         \Could not parse Constraint: "^General.exnMessage ex^"\n"^
                         "using the trivial constraint 'true' instead");
                  (NONE, triv_expr))

fun transform_parameter t {xmiid,name,kind,type_id} =
    (name, find_classifier_type t type_id
           handle _ => (warn ("no type found for parameter '"^name^
                              "', defaulting to OclVoid"); 
                        Rep_OclType.OclVoid)
    )

fun transform_operation t {xmiid,name,isQuery,parameter,visibility,
			   constraints,ownerScope} =
    let val result_type   = case filter (fn x => #kind x = XMI.Return) parameter
                             of []      => (warn ("no return type found for operation '"^name^
                                                  "', defaulting to OclVoid"); 
                                            Rep_OclType.OclVoid)
                              | [x]     => (find_classifier_type t (#type_id x)
                                            handle _ => (warn ("return parameter for operation '"^name^
                                                               "' has no declared type, defaulting to OclVoid"); 
                                                         Rep_OclType.OclVoid))
                              | x::y::_ => let val ret_type = find_classifier_type t (#type_id x)
                                                   handle _ => (warn ("return parameter for operation '"^name^
                                                                      "' has no declared type, defaulting to OclVoid"); 
                                                                Rep_OclType.OclVoid)
                                           in 
                                               (warn ("operation '"^name^
                                                  "' has multiple return parameters. Using only '"^
                                                      (Rep_OclType.string_of_OclType ret_type)^"'.");
                                                ret_type)
                                           end

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
	 body = [],
	 visibility = visibility,
	 scope = ownerScope,
	 isQuery = isQuery      (* FIX *)
	}
    end
    

fun transform_attribute t ({xmiid,name,type_id,changeability,visibility,ordering,
			    multiplicity,taggedValue,ownerScope,targetScope,stereotype,initialValue}) =
    let val cls_type = find_classifier_type t type_id 
            handle _ => (warn ("no type found for attribute '"^name^
                               "', defaulting to OclVoid"); 
                         Rep_OclType.OclVoid)
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

(* old    
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
*)
fun transform_aend t ({xmiid,name,association,ordering,multiplicity,participant_id,
		       isNavigable,aggregation,changeability,visibility,targetScope}:XMI.AssociationEnd):Rep.associationend =
    let
	val aend_path = path_of_associationend t xmiid
    in
	{name         = aend_path,
	 aend_type    = find_classifier_type t participant_id,
	 multiplicity = multiplicity,
	 ordered      = if ordering = XMI.Ordered then true else false,
	 visibility   = visibility,
	 init         = NONE (* FIXME *)
	}
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
        val assocs = find_classifier_associations t xmiid
        val parents = map ((find_classifier_type t) o (find_parent t)) 
			  generalizations 
	val filtered_parents   = filter (fn x => x <> Rep_OclType.OclAny) parents
        val filtered_parent = case filtered_parents
                               of []  => NONE
                                | [x] => SOME x
                                | x::y::_ => (warn ("Class '"^name^"' has multiple parents."^
                                                    " Using only '"^
                                                    (Rep_OclType.string_of_OclType x)^"'."); 
                                              SOME x)
 	val checked_invariants = filter_exists t invariant
(*        val navigable_aends    = filter #isNavigable (find_aends t xmiid)*)
	val class_type = find_classifier_type t xmiid
    in
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
                   activity_graphs = List.concat [map (transform_activitygraph t) activity_graphs,
						  map (transform_statemachine t) state_machines], 
		   thyname = NONE}
    end
  | transform_classifier t (XMI.AssociationClass {xmiid,name,isActive,visibility,
						  isLeaf,generalizations,attributes,
						  operations,invariant,stereotype,
						  clientDependency,connection,
						  supplierDependency,taggedValue}) =
    let 
	val (_,assocs,assoc,_,_) = find_classifier_entries t xmiid
	val parents = map ((find_classifier_type t) o (find_parent t)) 
			  generalizations 
        (* FIXME: filter for classes vs. interfaces *)  
	val filtered_parents = filter (fn x => x <> Rep_OclType.OclAny) parents 
	val checked_invariants = filter_exists t invariant
	(*val navigable_aends    = filter #isNavigable connection *)
	val class_type = find_classifier_type t xmiid			 
    in
	Rep.AssociationClass {name = (* type_of_classifier *)class_type,
		   parent = case filtered_parents 
			     of [] => NONE
			      | xs => SOME ((*path_of_classifier *) (hd xs)),
		   attributes = map (transform_attribute t) attributes,
		   operations = map (transform_operation t) operations,
		   invariant  = map ((transform_constraint t) o 
				     (find_constraint t)) checked_invariants, 
		   stereotypes = map (find_stereotype t) stereotype, 
		   interfaces = nil, (* FIX *)
			      thyname = NONE,
			      activity_graphs = [] (* FIXME *),
			      (*connection = map (transform_aend t) navigable_aends*)
			      associations = assocs,
			      association = assoc}

    end
  | transform_classifier t (XMI.Primitive {xmiid,name,generalizations,operations,invariant,taggedValue}) =
    let 
	val (_,assocs,assoc,_,_) = find_classifier_entries t xmiid
	val checked_invariants = filter_exists t invariant
    in
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
			               
(* billk_tag *)
(** transform an XMI.Association into a Rep.association *)
fun transform_association t ({xmiid,name,connection}:XMI.Association):Rep.association =
    let 
	val association_path = find_association_path t xmiid
	val association_ends = map (transform_aend t) connection
    in
	{name = (* path_of_association *) association_path,
	 aends = association_ends,
	 aclass = NONE} (* FIXME *)
    end

(** recursively transform all classes in the package. *)
fun transform_package t (XMI.Package p) :transform_model=
    let (* we do not transform the ocl library *)
	val filteredPackages = 
	    filter (fn (XMI.Package x) => 
		       ((#name x <> "oclLib") andalso (#name x <> "UML_OCL")))
		   (#packages p) 
	val local_associations = map (transform_association t) (#associations p)
	val local_classifiers = map (transform_classifier t) (#classifiers p)
	val (res_classifiers,res_associations) = ListPair.unzip (map (transform_package t) filteredPackages)
	val associations = local_associations @ (List.concat res_associations)
	val classifiers =local_classifiers @ (List.concat res_classifiers)
    in 
	(classifiers, associations )
    end


(***********************
(* billk_tag *)
(* recursively transforms all associations in the package p. *)
fun transform_associations t (XMI.Package p) = 
    (List.app (transform_associations t) (#packages p);
     List.app (transform_assocation t) (#associations p);
     List.app (transform_associationclass_as_association t)
	      (List.filter (fn (XMI.AssociationClass x) => true
			     | _                        => false)
			   (#classifiers p))
    )

(* billk_tag *)
(* The new class retains the original xmi-id. *)
fun transform_association_class_into_class table (XMI.AssociationClass ac) =
    XMI.Class { xmiid = #xmiid ac,
		name = #name ac,
		isActive = #isActive ac,
		visibility = #visibility ac,
		isLeaf = #isLeaf ac,
		generalizations = #generalizations ac,
		attributes = #attributes ac,
		operations = #operations ac,
		invariant = #invariant ac,
		stereotype = #stereotype ac,
		taggedValue = #taggedValue ac,
		clientDependency = #clientDependency ac,
		supplierDependency = #supplierDependency ac,
		classifierInState = [], (* FIXME: better dummy? *)
		activity_graphs = [], (* FIXME: better dummy? *)
		state_machines = []} (* FIXME: better dummy? *)

(* billk_tag *)
fun transform_association_class_into_association table (XMI.AssociationClass ac) =
    let
        val new_aend= {xmiid = #xmiid ac ^ "0",
		       name = SOME (#name ac),
		       isNavigable = true,
		       ordering = XMI.Unordered,
		       aggregation = XMI.NoAggregation, 
		       targetScope = XMI.InstanceScope,
		       multiplicity = [(1,1)], (* injective *)
		       changeability = XMI.Changeable,
		       visibility = #visibility ac,
		       participant_id = #xmiid ac  (* the new class retains the id *)
		      }:XMI.AssociationEnd
    in
	{xmiid = #xmiid ac ^ "1", 
	 name = NONE, (* FIXME: proper value? *)
	 connection = new_aend :: (#connection ac)}:XMI.Association
    end

(* billk_tag *)
fun transform_association_classes table (XMI.Package p) =
    let
	val (association_classes,other_classifiers) = List.partition (fn (XMI.AssociationClass x) => true
							       | _                        => false) 
							     (#classifiers p)
    in
	XMI.Package {xmiid = #xmiid p,
		     name = #name p,
		     visibility = #visibility p,
		     packages = map (transform_association_classes table) (#packages p),
		     classifiers = map (transform_association_class_into_class table) association_classes @ other_classifiers ,
		     state_machines = #state_machines p,
		     activity_graphs = #activity_graphs p,
		     associations =  map (transform_association_class_into_association table) association_classes @ (#associations p),
		     generalizations = #generalizations p,
		     constraints = #constraints p,
		     stereotypes = #stereotypes p,
		     dependencies = #dependencies p,
		     tag_definitions = #tag_definitions p,
		     stereotype = #stereotype p,
		     taggedValue = #taggedValue p,
		     events = #events p}
    end

(* billk_tag *)
(* multiplicities -> constraints *)
fun transform_multiplicities table (XMI.Package p) =
    XMI.Package {xmiid = #xmiid p,
		 name = #name p,
		 visibility = #visibility p,
		 packages = #packages p,
		 classifiers = #classifiers p,
		 state_machines = #state_machines p,
		 activity_graphs = #activity_graphs p,
		 associations =  map (transform_association_multiplicities table) (#associations p),
		 generalizations = #generalizations p,
		 constraints = #constraints p,
		 stereotypes = #stereotypes p,
		 dependencies = #dependencies p,
		 tag_definitions = #tag_definitions p,
		 stereotype = #stereotype p,
		 taggedValue = #taggedValue p,
		 events = #events p}


fun add_constraint_to_class table (Rep_Core.Class cls) (name:string option,constr:OclTerm) =
    let
	val cls_type = find_classifier_type table (#xmiid cls)
	val aends = find_aends table (#xmiid cls)
	val agraphs = find_activity_graph_of table (#xmiid cls)
	val modified_cls = {xmiid = #xmiid cls,
			    name = #name cls,
			    isActive = #isActive cls,
			    visibility = #visibility cls,
			    isLeaf = #isLeaf cls,
			    generalizations = #generalizations cls,
			    attributes = #attributes cls,
			    operations = #operations cls,
			    invariant =(name,constr)::(#invariant cls) ,
			    stereotype = #stereotype cls,
			    taggedValue = #taggedValue cls,
			    clientDependency = #clientDependency cls,
			    supplierDependency = #supplierDependency cls,
			    classifierInState = #classifierInState cls,
			    activity_graphs = #activity_graphs cls,
			    state_machines = #state_machines cls}
    in
	HashTable.insert table (#xmiid cls,Type (cls_type,aends,modified_cls,agraphs))
    end

fun generate_n_ary_constraint table (ac:XMI.Association) = 
    let
	(* use side-effects to manipulate the table *)
	val association_xmiids = map #xmiid (#connection ac)
	val classifiers = map (find_classifier table) association_xmiids
	val multiplicities = map #multiplicity (#connection ac)
	fun generate_local_match_constraint others (XMI.Class cls)=
	    let
		val aend = name_of classifier
		val var = Rep_OclTerm.Variable ("n"^(#xmiid cls), type_of cls)
		fun get_collection cls = ocl_aendcall var aend (Collection (Classifier (name_of classifier)))
		fun collection_equality coll1 coll2 = ocl_and (ocl_includes coll1 coll2) (ocl_includes coll2 coll1)
		val sample = get_collection (head others)
		fun append_match (current,partial_expression) = ocl_and partial_expression (collection_equality sample (get_collection current))
		fun match_ocl_expression = foldr1 append_match (tail others)
		fun nest_allInstances (current, partial:OclTerm):OclTerm = ocl_forAll (ocl_allInstances current) ("n"^(#xmiid current)) partial
	    in
		foldr nest_allInstances  match_ocl_expression others
	    end
	(* multipliciteis are handled when they are removed later on *)
        fun iterate_over_connection done (cls::todo)=
	    ( add_constraint_to_class table cls (generate_local_match_constraint (done@todo) cls);
	      iterate_over_connection (cls::done) todo;
	      ())
	    | iterate_over_connection done []=  ()
	    
    in
	ac
    end

*********)

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
fun transformXMI_ext ({classifiers,constraints,packages,
		       stereotypes,variable_declarations,state_machines, activity_graphs}):transform_model=
    let val (xmiid_table: (string,HashTableEntry) HashTable.hash_table) =
	    HashTable.mkTable (HashString.hashString, (op =)) (101, Option)
	(* hack: insert a dummy type into the table *)
	val _ = HashTable.insert xmiid_table ("DummyT",
                                              Type (Rep_OclType.DummyT,
                                                    nil,
						    nil,
                                                    XMI.Primitive{name="DummyT",
                                                                  xmiid="DummyT",
                                                                  operations=[],
                                                                  generalizations=[],
                                                                  invariant=[],
																  taggedValue=[]},
                                                    nil))
	val _ = HashTable.insert xmiid_table ("-1",UniqueName(123456)) (* arbitrary startnu,ber *)
	(* for some reasons, there are model elements outside of the top-level *) 
	(* model the xmi-file. So we have to handle them here seperately:      *)
	val _ = map (insert_classifier xmiid_table nil) classifiers
	val _ = map (insert_constraint xmiid_table) constraints
	val _ = map (insert_stereotype xmiid_table) stereotypes
	val _ = map (insert_variable_dec xmiid_table) variable_declarations
	(* "hd packages" is supposed to be the first model in the xmi-file *)
	val model = hd packages
    in 
	insert_model xmiid_table model        (* fill xmi.id table *);
	fix_associations xmiid_table model    (* handle associations *);
        transform_package xmiid_table model   (* transform classifiers *)
    end

fun transformXMI x:Classifier list = fst (transformXMI_ext x)


(** 
 * read and transform a .xmi file.
 * @return a list of rep classifiers, or nil in case of problems
 *) 

fun normalize_ext ((clsses,accs):transform_model):Rep.Model =
    (map (Rep.normalize accs) clsses,accs)

fun readFile f = (info ("opening "^f);
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



fun printStackTrace e =
    let val ss = CompilerExt.exnHistory e
    in
        print_stderr ("uncaught exception " ^ (General.exnMessage e) ^ " at:\n");
        app (fn s => print_stderr ("\t" ^ s ^ "\n")) ss
    end
    





    
(****************************************************
 *****************************************************
 * Test function.
 *)
fun test (_,filename::_) = (Rep2String.printList (fst (readFile filename)); OS.Process.success)
    handle ex => (printStackTrace ex; OS.Process.failure)

end

