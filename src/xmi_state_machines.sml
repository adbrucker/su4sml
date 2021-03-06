(*****************************************************************************
 * su4sml --- an SML repository for managing (Secure)UML/OCL models
 *             http://projects.brucker.ch/su4sml/
 *                                                                            
 * xmi_state_machines.sml --- XMI-UML-StateMachine datatypes for the import 
 *                            interface for su4sml
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

(* ---------------------------------------------------------------------------
 * The types in these structures are supposed to define a representation of 
 * the XML elements found in UML-XMI files. It is reasonably close to the UML 
 * metamodel and the XMI representation of it, while simplifying some kinds 
 * of references.
 * --------------------------------------------------------------------------*)

structure XMI_CommonBehavior = 
struct
end

structure XMI_StateMachines =
struct
open XMI_ExtensionMechanisms XMI_CommonBehavior

type     StateVertex_Id = string
type     Transition_Id  = string
type     Stereotype_Id  = string

datatype Procedure = mk_Procedure of 
                                 {xmiid            : string,
                                  isSpecification : bool,
                                  name             : string,
                                  isAsynchronous   : bool,
                                  language         : string,
                                  body             : string,
                                  expression       : string 
                          (*      method     : Method list, NOT YET IMPLEMENTED *)
                          (*      isList     : bool NOT SUPPORTED BY POSEIDON *)}

datatype Guard  = mk_Guard of    {xmiid            : string,
                                  isSpecification  : bool,
                                  name             : string,
                                  visibility       : VisibilityKind, 
                                  language         : string,
                                  body             : string option,
                                  expression       : XMI_OCL.OCLExpression option}

datatype Event  = SignalEvent  of {xmiid : string, name: string, parameter: Parameter list}
                | CallEvent    of { xmiid : string,
				    name: string,
				    operation: string, (* xmi.idref *)
				    parameter:  Parameter list
					       }
				 (*   | TimeEvent    of Parameter list  *)
				 (*   | ChangeEvent  of Parameter list  *)
				 
				 
datatype Transition   = mk_Transition of  
                                 {isSpecification : bool,
                                  xmiid   : string,
                                  source  : StateVertex_Id,
                                  target  : StateVertex_Id,
			 	  guard   : Guard  option,
				  trigger : string  option, (* xmi.idref to Event *)
				  effect  : Procedure option,
                                  taggedValue  : TaggedValue list
				 (* mmm    : StateVertexId option *)
			         }
				 
fun transition_source_of (mk_Transition{source,...}) = source
fun transition_target_of (mk_Transition{target,...}) = target
				   
datatype PseudoStateVars = initial  | deep   | shallow |
                           join     | fork   | 
                           junction | choice 
				  
datatype StateVertex = 
         CompositeState of { xmiid        : string,
                             name         : string,
                             stereotype   : Stereotype_Id list,
                             isSpecification : bool,
                             isConcurrent : bool,
                             entry        : Procedure option,
                             exit         : Procedure option,
                             doActivity   : Procedure option,
                             outgoing     : Transition_Id list,
	                         incoming     : Transition_Id list, 
                             taggedValue  : TaggedValue list,
	                         subvertex    : StateVertex list}
       | SubactivityState of { xmiid        : string,
                               name         : string,
                               stereotype   : Stereotype_Id list,
                               isSpecification : bool,
                               isConcurrent : bool,
                               entry        : Procedure option,
                               exit         : Procedure option,
                               doActivity   : Procedure option,
                               outgoing     : Transition_Id list,
	                           incoming     : Transition_Id list, 
	                           subvertex    : StateVertex list,
                               submachine   : StateMachine,
                               taggedValue  : TaggedValue list,
                               isDynamic    : bool}
       | SimpleState of {xmiid        : string,
                         name         : string,
                         stereotype   : Stereotype_Id list,
                         isSpecification : bool,
                         entry        : Procedure option,
                         exit         : Procedure option,
                         doActivity   : Procedure option,
                         taggedValue  : TaggedValue list,
                         outgoing     : Transition_Id list,
	                     incoming     : Transition_Id list}
       | ActionState of { xmiid        : string,
                          name         : string,
                          stereotype   : Stereotype_Id list,
                          isSpecification : bool,
                          entry        : Procedure option,
                          outgoing     : Transition_Id list,
	                      incoming     : Transition_Id list, 
                          taggedValue  : TaggedValue list,
                          isDynamic    : bool 
                        (* + dynamicArguments + dynamicMultiplicity *)}
       | ObjectFlowState of { xmiid        : string,
                              name         : string,
                              stereotype   : Stereotype_Id list,
                              isSpecification : bool,
                              entry        : Procedure option,
                              exit         : Procedure option, 
                              doActivity   : Procedure option,
                              outgoing     : Transition_Id list,
	                          incoming     : Transition_Id list, 
                              isSynch      : bool,
                              parameter    : Parameter list,
                              taggedValue  : TaggedValue list,
                              type_        : string}
       | FinalState of { xmiid        : string,
                         name         : string,
                         stereotype   : Stereotype_Id list,
                         isSpecification : bool,
                         entry        : Procedure option,
                         exit         : Procedure option,
                         doActivity   : Procedure option,
                         taggedValue  : TaggedValue list,
	                 incoming     : Transition_Id list }
       | PseudoState of { xmiid        : string,
                          name         : string,
                          stereotype   : Stereotype_Id list,
                          isSpecification : bool,
                          kind         : PseudoStateVars,
                          taggedValue  : TaggedValue list,
                          outgoing     : Transition_Id list,
	                  incoming     : Transition_Id list }
       | SyncState of { xmiid        : string,
                        name         : string,
                        stereotype   : Stereotype_Id list,
                        isSpecification : bool,
                        outgoing     : Transition_Id list,
	                incoming     : Transition_Id list,
                        taggedValue  : TaggedValue list,
	                bound        : int}
(*     | StubState  *)	    
and	StateMachine = mk_StateMachine of 
                           {xmiid            : string,
                            contextxmiid     : string,
                            isSpecification : bool,
                            top              : StateVertex,
                            transitions      : Transition list}

fun state_type_of  (ObjectFlowState{type_,...}) = type_
  | state_type_of  _ = Logger.error "in state_type_of: argument is not an ObjectFlow state"

fun state_entry_of (CompositeState{entry,...}) = entry
  | state_entry_of (SubactivityState{entry,...}) = entry
  | state_entry_of (SimpleState{entry,...}) = entry
  | state_entry_of (ActionState{entry,...}) = entry
  | state_entry_of (ObjectFlowState{entry,...}) = entry
  | state_entry_of (FinalState{entry,...}) = entry
  | state_entry_of _ = Logger.error "in state_entry_of: argument does not have entry actions"

fun state_xmiid_of (CompositeState{xmiid,...}) = xmiid
  | state_xmiid_of (SubactivityState{xmiid,...}) = xmiid
  | state_xmiid_of (SimpleState{xmiid,...}) = xmiid
  | state_xmiid_of (ActionState{xmiid,...}) = xmiid
  | state_xmiid_of (ObjectFlowState{xmiid,...}) = xmiid
  | state_xmiid_of (FinalState{xmiid,...}) = xmiid
  | state_xmiid_of (PseudoState{xmiid,...}) = xmiid
  | state_xmiid_of (SyncState{xmiid,...}) = xmiid

fun state_name_of (CompositeState{name,...}) = name
  | state_name_of (SubactivityState{name,...}) = name
  | state_name_of (SimpleState{name,...}) = name
  | state_name_of (ActionState{name,...}) = name
  | state_name_of (ObjectFlowState{name,...}) = name
  | state_name_of (FinalState{name,...}) = name
  | state_name_of (PseudoState{name,...}) = name
  | state_name_of (SyncState{name,...}) = name

fun state_subvertices_of (CompositeState{subvertex,...}) = subvertex
  | state_subvertices_of (SubactivityState{subvertex,...}) = subvertex
  | state_subvertices_of _ = Logger.error "in state_subvertices_of: argument is \
                                           \not a composite state" 

fun state_outgoing_trans_of (CompositeState{outgoing,...}) = outgoing
  | state_outgoing_trans_of (SubactivityState{outgoing,...}) = outgoing
  | state_outgoing_trans_of (SimpleState{outgoing,...}) = outgoing
  | state_outgoing_trans_of (ActionState{outgoing,...}) = outgoing
  | state_outgoing_trans_of (ObjectFlowState{outgoing,...}) = outgoing
  | state_outgoing_trans_of (PseudoState{outgoing,...}) = outgoing
  | state_outgoing_trans_of (SyncState{outgoing,...}) = outgoing
  | state_outgoing_trans_of (FinalState _) = Logger.error "in state_outgoing_trans_of: \
                                                           \argument is a final state"

fun state_incoming_trans_of (CompositeState{incoming,...}) = incoming
  | state_incoming_trans_of (SubactivityState{incoming,...}) = incoming
  | state_incoming_trans_of (SimpleState{incoming,...}) = incoming
  | state_incoming_trans_of (ActionState{incoming,...}) = incoming
  | state_incoming_trans_of (ObjectFlowState{incoming,...}) = incoming
  | state_incoming_trans_of (FinalState{incoming,...}) = incoming
  | state_incoming_trans_of (PseudoState{incoming,...}) = incoming
  | state_incoming_trans_of (SyncState{incoming,...}) = incoming


fun state_stereotype_of (CompositeState{stereotype,...}) = stereotype
  | state_stereotype_of (SubactivityState{stereotype,...}) = stereotype
  | state_stereotype_of (SimpleState{stereotype,...}) = stereotype
  | state_stereotype_of (ActionState{stereotype,...}) = stereotype
  | state_stereotype_of (ObjectFlowState{stereotype,...}) = stereotype
  | state_stereotype_of (FinalState{stereotype,...}) = stereotype
  | state_stereotype_of (PseudoState{stereotype,...}) = stereotype
  | state_stereotype_of (SyncState{stereotype,...}) = stereotype


fun state_taggedValue_of (CompositeState{taggedValue,...}) = taggedValue
  | state_taggedValue_of (SubactivityState{taggedValue,...}) = taggedValue
  | state_taggedValue_of (SimpleState{taggedValue,...}) = taggedValue
  | state_taggedValue_of (ActionState{taggedValue,...}) = taggedValue
  | state_taggedValue_of (ObjectFlowState{taggedValue,...}) = taggedValue
  | state_taggedValue_of (FinalState{taggedValue,...}) = taggedValue
  | state_taggedValue_of (PseudoState{taggedValue,...}) = taggedValue
  | state_taggedValue_of (SyncState{taggedValue,...}) = taggedValue

fun state_is_fork (PseudoState{kind=fork,...}) = true
  | state_is_fork _                            = false

fun state_is_join (PseudoState{kind=join,...}) = true
  | state_is_join _                            = false

end



