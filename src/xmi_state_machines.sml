(*****************************************************************************
 *          su4sml - a SecureUML repository for SML              
 *                                                                            
 * xmi_umlcore.sig - XMI-UML-Core datatypes for the import interface for su4sml
 * Copyright (C) 2005  Achim D. Brucker <brucker@inf.ethz.ch>
 *                     Jürgen Doser <doserj@inf.ethz.ch>
 *                     Burkhart Wolff
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
open XMI_Core XMI_CommonBehavior


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
                                  expression       : string list
                          (*      method     : Method list, NOT YET IMPLEMENTED *)
                          (*      isList     : bool NOT SUPPORTED BY POSEIDON *)}

datatype Guard  = mk_Guard of    {xmiid            : string,
                                  isSpecification  : bool,
                                  name             : string,
                                  visibility       : VisibilityKind, 
                                  language         : string,
                                  body             : string,
                                  expression       : string list}

datatype Event  = SignalEvent  of Parameter list
                | CallEvent    of Parameter list
				 (*   | TimeEvent    of Parameter list  *)
				 (*   | ChangeEvent  of Parameter list  *)
				 
				 
datatype Transition   = mk_Transition of  
                                 {isSpecification : bool,
                                  xmiid   : string,
                                  source  : StateVertex_Id,
                                  target  : StateVertex_Id,
			 	  guard   : Guard  option,
				  trigger : Event  option,
				  effect  : Procedure option,
                                  taggedValue  : TaggedValue list
				 (* mmm    : StateVertexId option *)
			         }
				 
fun transition_source_of (mk_Transition{source,...}) = source
fun transition_target_of (mk_Transition{target,...}) = target
				   
datatype PseudoStateVars = initial  | deep   | shallow |
                           join     | fork   | 
                           junction | choice 
				  
datatype StateVertex  = 
         CompositeState 
	 of {xmiid        : string,
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
       | SubactivityState
	 of {xmiid        : string,
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
       | SimpleState
	 of {xmiid        : string,
             name         : string,
             stereotype   : Stereotype_Id list,
             isSpecification : bool,
             entry        : Procedure option,
             exit         : Procedure option,
             doActivity   : Procedure option,
             taggedValue  : TaggedValue list,
             outgoing     : Transition_Id list,
	     incoming     : Transition_Id list}
       | ActionState (* from ActivityGraphs *)
         of {xmiid        : string,
             name         : string,
             stereotype   : Stereotype_Id list,
             isSpecification : bool,
             entry        : Procedure option,
             exit         : Procedure option,
             doActivity   : Procedure option,
             outgoing     : Transition_Id list,
	     incoming     : Transition_Id list, 
             taggedValue  : TaggedValue list,
             isDynamic    : bool 
             (* + dynamicArguments + dynamicMultiplicity *)}
       | ObjectFlowState (* from ActivityGraphs *)
         of {xmiid        : string,
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
             type_        : Rep_OclType.Path option}
       | FinalState
	 of {xmiid        : string,
             name         : string,
             stereotype   : Stereotype_Id list,
             isSpecification : bool,
             entry        : Procedure option,
             exit         : Procedure option,
             doActivity   : Procedure option,
             taggedValue  : TaggedValue list,
             outgoing     : Transition_Id list,
	     incoming     : Transition_Id list }
       | PseudoState
	 of {xmiid        : string,
             name         : string,
             stereotype   : Stereotype_Id list,
             isSpecification : bool,
             kind         : PseudoStateVars,
             entry        : Procedure option,
             exit         : Procedure option,
             doActivity   : Procedure option,
             taggedValue  : TaggedValue list,
             outgoing     : Transition_Id list,
	     incoming     : Transition_Id list }
       | SyncState
	 of {xmiid        : string,
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


fun state_xmiid_of (CompositeState{xmiid,...}) = xmiid
  | state_xmiid_of (SubactivityState{xmiid,...}) = xmiid
  | state_xmiid_of (SimpleState{xmiid,...}) = xmiid
  | state_xmiid_of (ActionState{xmiid,...}) = xmiid
  | state_xmiid_of (ObjectFlowState{xmiid,...}) = xmiid
  | state_xmiid_of (FinalState{xmiid,...}) = xmiid
  | state_xmiid_of (PseudoState{xmiid,...}) = xmiid
  | state_xmiid_of (SyncState{xmiid,...}) = xmiid

fun state_subvertices_of (CompositeState{subvertex,...}) = subvertex
  | state_subvertices_of (SubactivityState{subvertex,...}) = subvertex
  | state_subvertices_of _ = raise IllFormed "state_subvertices_of called on a non-composite state"

fun state_outgoing_trans_of (CompositeState{outgoing,...}) = outgoing
  | state_outgoing_trans_of (SubactivityState{outgoing,...}) = outgoing
  | state_outgoing_trans_of (SimpleState{outgoing,...}) = outgoing
  | state_outgoing_trans_of (ActionState{outgoing,...}) = outgoing
  | state_outgoing_trans_of (ObjectFlowState{outgoing,...}) = outgoing
  | state_outgoing_trans_of (FinalState{outgoing,...}) = outgoing
  | state_outgoing_trans_of (PseudoState{outgoing,...}) = outgoing
  | state_outgoing_trans_of (SyncState{outgoing,...}) = outgoing


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

end



