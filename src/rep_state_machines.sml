(*****************************************************************************
 * su4sml --- an SML repository for managing (Secure)UML/OCL models
 *             http://projects.brucker.ch/su4sml/
 *                                                                            
 * rep_state_machines.sml --- 
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

(** repository datatypes and helper functions for UML statemachines *)
signature REP_STATE_MACHINE = 
sig
    
type     StateVertex_Id
type     Transition_Id

type Procedure   
type Guard        = Rep_OclTerm.OclTerm
                    
type Parameter    = string * Rep_OclType.OclType 

datatype Event  = SignalEvent  of Parameter list
		| CallEvent    of Rep_OclType.Path * Parameter list
(*   | TimeEvent    of Parameter list  *)
(*   | ChangeEvent  of Parameter list  *)
				  
type Transition 
				 
				   
type PseudoStateVars = XMI_StateMachines.PseudoStateVars
				
type StateMachine     
datatype StateVertex  =
         State_CompositeState 
	 of {name         : string,
	     state_id     : StateVertex_Id,
	     outgoing     : Transition_Id list,
	     incoming     : Transition_Id list, 
	     subvertex    : StateVertex list,
             isConcurrent : bool
            (*             submachine   : StateMachine * 
                                          {isDynamic : bool
                                          (* + dynamicArguments 
                                           + dynamicMultiplicity *)} option *)}
       (* variant for Subactivity State *)
       | State_SimpleState
	 of {name         : string,
	     state_id     : StateVertex_Id,
	     outgoing     : Transition_Id list,
	     incoming     : Transition_Id list}
       | SimpleState_ActionState (* from ActivityGraphs *)
         of {name         : string,
	     state_id     : StateVertex_Id,
	     outgoing     : Transition_Id list,
	     incoming     : Transition_Id list,
	     isDynamic    : bool 
             (* + dynamicArguments + dynamicMultiplicity *)}
       | SimpleState_ObjectflowState (* from ActivityGraphs *)
         of {state_id     : StateVertex_Id,
	     outgoing     : Transition_Id list,
	     incoming     : Transition_Id list,
	     isSynch      : bool,
             parameter    : Parameter list,
             types        : Rep_OclType.Path list (* Classifier_Id *)}
       | State_FinalState
	 of {state_id     : StateVertex_Id,
	     incoming     : Transition_Id list}
       | PseudoState
	 of {state_id     : StateVertex_Id,
	     kind         : PseudoStateVars,
             outgoing     : Transition_Id list,
	     incoming     : Transition_Id list}
       | SyncState
	 of {state_id     : StateVertex_Id,
	     outgoing     : Transition_Id list,
	     incoming     : Transition_Id list}
(*	  | StubState  *)
                                 
	    
val isInit         : StateVertex -> bool
val isPseudo       : StateVertex -> bool
val isFinal        : StateVertex -> bool
val isComposite    : StateVertex -> bool

val isTriggered    : Transition -> bool


val id_of_state    : StateVertex -> string
val name_of_state  : StateVertex -> string
val subvertices_of : StateVertex -> StateVertex list

val outgoing_of    : StateVertex -> Transition_Id list
val incoming_of    : StateVertex -> Transition_Id list

val pseudo_state_kind_of : StateVertex -> PseudoStateVars

val add_outgoing : StateVertex -> Transition_Id list -> StateVertex
val add_incoming : StateVertex -> Transition_Id list -> StateVertex
     
end

structure Rep_StateMachine : REP_STATE_MACHINE = 
struct
    
type     StateVertex_Id = string
type     Transition_Id  = string

type Procedure = {proc_id          : string,
		  language         : string,
		  body             : string,
		  expression       : string }
                 
type Guard        = Rep_OclTerm.OclTerm
                    
type Parameter    = string * Rep_OclType.OclType 

datatype Event  = SignalEvent  of Parameter list
		| CallEvent    of Rep_OclType.Path * Parameter list
(*   | TimeEvent    of Parameter list  *)
(*   | ChangeEvent  of Parameter list  *)

				 
type Transition   = {trans_id : Transition_Id,
		     source  : StateVertex_Id,
                     target  : StateVertex_Id,
		     guard   : Guard  option,
		     trigger : Event  option,
		     effect  : Procedure option
		    (* mmm    : StateVertexId option *)
		    }
			
type PseudoStateVars = XMI_StateMachines.PseudoStateVars
				  
datatype StateVertex  = 
         State_CompositeState 
	 of {name         : string,
	     state_id     : StateVertex_Id,
	     outgoing     : Transition_Id list,
	     incoming     : Transition_Id list, 
	     subvertex    : StateVertex list,
             isConcurrent : bool
             (*submachine   : StateMachine * 
                            {isDynamic : bool
                             (* + dynamicArguments 
                                + dynamicMultiplicity *)} option *)}
                            (* variant for Subactivity State *)
       | State_SimpleState
	 of {name         : string,
	     state_id     : StateVertex_Id,
	     outgoing     : Transition_Id list,
	     incoming     : Transition_Id list}
       | SimpleState_ActionState (* from ActivityGraphs *)
         of {name         : string,
	     state_id     : StateVertex_Id,
	     outgoing     : Transition_Id list,
	     incoming     : Transition_Id list,
	     isDynamic    : bool 
             (* + dynamicArguments + dynamicMultiplicity *)}
       | SimpleState_ObjectflowState (* from ActivityGraphs *)
         of {state_id     : StateVertex_Id,
	     outgoing     : Transition_Id list,
	     incoming     : Transition_Id list,
	     isSynch      : bool,
             parameter    : Parameter list,
             types        : Rep_OclType.Path list (* Classifier_Id *)}
       | State_FinalState
	 of {state_id     : StateVertex_Id,
	     incoming     : Transition_Id list}
       | PseudoState
	 of {state_id     : StateVertex_Id,
	     kind         : PseudoStateVars,
             outgoing     : Transition_Id list,
	     incoming     : Transition_Id list}
       | SyncState
	 of {state_id     : StateVertex_Id,
	     outgoing     : Transition_Id list,
	     incoming     : Transition_Id list}
(*	  | StubState  *)
withtype StateMachine = {top : StateVertex,
                         transition : Transition list}



(* StateVertex -> StateVertex_Id *)
fun id_of_state (State_SimpleState{state_id,...})           = state_id
  | id_of_state (State_CompositeState{state_id,...})        = state_id
  | id_of_state (SimpleState_ActionState{state_id,...})     = state_id
  | id_of_state (SimpleState_ObjectflowState{state_id,...}) = state_id
  | id_of_state (State_FinalState{state_id,...})            = state_id
  | id_of_state (PseudoState{state_id,...})                 = state_id
  | id_of_state (SyncState{state_id,...})                   = state_id

fun name_of_state (State_SimpleState{name,...})       = name
  | name_of_state (State_CompositeState{name,...})    = name
  | name_of_state (SimpleState_ActionState{name,...}) = name
  | name_of_state (State_FinalState{...})             = "Final"
  | name_of_state (S as PseudoState{kind,...})        = case kind
						         of XMI.initial  => "INIT"^id_of_state(S)
						          | XMI.junction => "ERROR"
						          | _            => "WRONG"

(** returns the list of subvertices. *)
fun subvertices_of (State_CompositeState{subvertex,...}) = subvertex
  | subvertices_of _                                     = []


fun pseudo_state_kind_of (PseudoState{kind,...}) = kind

(* cough. *)
fun isPseudo (PseudoState{kind,...}) = not(kind=XMI.initial)
  | isPseudo _                       = false

fun isInit (PseudoState{kind,...}) = kind=XMI.initial
  | isInit _                       = false

fun isFinal (State_FinalState{...}) = true
  | isFinal _                       = false

fun isComposite (State_CompositeState{...}) = true
  | isComposite _                           = false

fun outgoing_of (State_SimpleState{outgoing,...})           = outgoing
  | outgoing_of (State_CompositeState{outgoing,...})        = outgoing
  | outgoing_of (SimpleState_ActionState{outgoing,...})     = outgoing
  | outgoing_of (SimpleState_ObjectflowState{outgoing,...}) = outgoing
  | outgoing_of (State_FinalState{...})                     = [] 
  | outgoing_of (PseudoState{outgoing,...})                 = outgoing
  | outgoing_of (SyncState{outgoing,...})                   = outgoing

fun incoming_of (State_SimpleState{incoming,...})           = incoming
  | incoming_of (State_CompositeState{incoming,...})        = incoming
  | incoming_of (SimpleState_ActionState{incoming,...})     = incoming
  | incoming_of (SimpleState_ObjectflowState{incoming,...}) = incoming
  | incoming_of (State_FinalState{incoming,...})            = incoming
  | incoming_of (PseudoState{incoming,...})                 = incoming
  | incoming_of (SyncState{incoming,...})                   = incoming

fun isTriggered (t:Transition) = Option.isSome (#trigger t)


fun add_outgoing (State_CompositeState {name,state_id,outgoing,incoming,
                                        subvertex,isConcurrent})                 newOut = 
    State_CompositeState{name=name,
                         state_id=state_id,
                         outgoing=outgoing@newOut,
                         incoming=incoming, 
                         subvertex=subvertex, 
                         isConcurrent=isConcurrent}
  | add_outgoing (State_SimpleState{name=n,state_id=sid,outgoing=ol,incoming=il}) newOut = 
    State_SimpleState{ name=n,
                       state_id=sid,
                       outgoing=ol@newOut,
                       incoming=il} 
  | add_outgoing (SimpleState_ActionState{name=n,state_id=sid,outgoing=ol,
                                         incoming=il,isDynamic=d})               newOut = 
    SimpleState_ActionState{ name=n,
                             state_id=sid,
                             outgoing=ol@newOut,
                             incoming=il,
                             isDynamic=d}
  | add_outgoing (SimpleState_ObjectflowState{state_id=sid,outgoing=ol,incoming=il,
                                             isSynch=s,parameter=p,types=t})     newOut = 
    SimpleState_ObjectflowState{ state_id=sid,
                                 outgoing=ol@newOut,
                                 incoming=il,
                                 isSynch=s,
                                 parameter=p,
                                 types=t}
  | add_outgoing (s as State_FinalState{state_id=sid,incoming=il})                newOut = s
  | add_outgoing (PseudoState{state_id=sid,kind=k,outgoing=ol,incoming=il})       newOut = 
    PseudoState{ state_id=sid,
                 kind=k,
                 outgoing=ol@newOut,
                 incoming=il}
  | add_outgoing (SyncState{state_id=sid,outgoing=ol,incoming=il})                newOut = 
    SyncState {state_id=sid,
               outgoing=ol@newOut,
               incoming=il}
    
fun add_incoming (State_CompositeState{name=n,state_id=sid,outgoing=ol,
                                       incoming=il,subvertex=sv,isConcurrent=c}) newIn = 
    State_CompositeState{ name=n,
                          state_id=sid,
                          outgoing=ol,
                          incoming=il@newIn,
                          subvertex=sv,
                          isConcurrent=c}
  | add_incoming (State_SimpleState{name=n,state_id=sid,outgoing=ol,incoming=il}) newIn = 
    State_SimpleState{name=n,
                      state_id=sid,
                      outgoing=ol,
                      incoming=il@newIn} 
  | add_incoming (SimpleState_ActionState{name=n,state_id=sid,outgoing=ol,
                                         incoming=il,isDynamic=d})               newIn = 
    SimpleState_ActionState{name=n,
                            state_id=sid,
                            outgoing=ol,
                            incoming=il@newIn,
                            isDynamic=d}
  | add_incoming (SimpleState_ObjectflowState{state_id=sid,outgoing=ol,incoming=il,
                                             isSynch=s,parameter=p,types=t})     newIn = 
    SimpleState_ObjectflowState{state_id=sid,
                                outgoing=ol,
                                incoming=il, (* FIXME? *)
                                isSynch=s,
                                parameter=p,
                                types=t}
  | add_incoming (State_FinalState{state_id=sid,incoming=il})                     newIn = 
    State_FinalState{ state_id=sid,
                      incoming=il@newIn}
  | add_incoming (PseudoState{ state_id=sid,kind=k,outgoing=ol,incoming=il})      newIn = 
    PseudoState{ state_id=sid,
                 kind=k,
                 outgoing=ol,
                 incoming=il@newIn}
  | add_incoming (SyncState{state_id=sid,outgoing=ol,incoming=il})                newIn = 
    SyncState{ state_id=sid,
               outgoing=ol,
               incoming=il@newIn}
				 
end
