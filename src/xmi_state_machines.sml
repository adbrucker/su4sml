(*****************************************************************************
 *          su4sml - a SecureUML repository for SML              
 *                                                                            
 * xmi_umlcore.sig - XMI-UML-Core datatypes for the import interface for su4sml
 * Copyright (C) 2005  Achim D. Brucker <brucker@inf.ethz.ch>
 *                     J�rgen Doser <doserj@inf.ethz.ch>
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
	 
datatype Action = create 
		| call 
		| return
		| send
		| terminate
		| uninterpreted
		| destroy
		| sequence

datatype Guard  = mk_Guard of  {expression : Rep_OclTerm.OclTerm}

datatype Event  = SignalEvent  of Parameter list
                | CallEvent    of Parameter list
				 (*   | TimeEvent    of Parameter list  *)
				 (*   | ChangeEvent  of Parameter list  *)
				 
				 
datatype Transition   = mk_Transition of  
                                 {is_specification : bool,
                                  xmiid   : string,
                                  source  : StateVertex_Id,
                                  target  : StateVertex_Id,
			 	  guard   : Guard  option,
				  trigger : Event  option,
				  effect  : Action option
				 (* mmm    : StateVertexId option *)
			         }
				 
				   
datatype PseudoStateVars = initial  | deep   | shallow |
                           join     | fork   | 
                           junction | choice 
				  
datatype StateVertex  = 
         State_CompositState 
	 of {xmiid        : string,
             name         : string,
             is_specification : bool,
             isConcurrent : bool,
             entry        : Action option,
             exit         : Action option,
             doActivity   : Action option,
             outgoing     : Transition_Id list,
	     incoming     : Transition_Id list, 
	     subvertex    : StateVertex list}
       | CompositState_SubactivityState
	 of {xmiid        : string,
             name         : string,
             is_specification : bool,
             isConcurrent : bool,
             entry        : Action option,
             exit         : Action option,
             doActivity   : Action option,
             outgoing     : Transition_Id list,
	     incoming     : Transition_Id list, 
	     subvertex    : StateVertex list,
             submachine   : StateMachine ,
             isDynamic    : bool}
       | State_SimpleState
	 of {xmiid        : string,
             name         : string,
             is_specification : bool,
             entry        : Action option,
             exit         : Action option,
             doActivity   : Action option,
             outgoing     : Transition_Id list,
	     incoming     : Transition_Id list}
       | SimpleState_ActionState (* from ActivityGraphs *)
         of {xmiid        : string,
             name         : string,
             is_specification : bool,
             entry        : Action option,
             exit         : Action option,
             doActivity   : Action option,
             outgoing     : Transition_Id list,
	     incoming     : Transition_Id list, 
             isDynamic    : bool 
             (* + dynamicArguments + dynamicMultiplicity *)}
       | SimpleState_ObjectflowState (* from ActivityGraphs *)
         of {xmiid        : string,
             name         : string,
             is_specification : bool,
             entry        : Action option,
             exit         : Action option,
             doActivity   : Action option,
             outgoing     : Transition_Id list,
	     incoming     : Transition_Id list, 
             isSynch      : bool,
             parameter    : Parameter list,
             types        : Rep_OclType.Path list (* Classifier_Id *)}
       | State_FinalState
	 of {xmiid        : string,
             name         : string,
             is_specification : bool,
             entry        : Action option,
             exit         : Action option,
             doActivity   : Action option,
             outgoing     : Transition_Id list,
	     incoming     : Transition_Id list }
       | PseudoState
	 of {xmiid        : string,
             name         : string,
             is_specification : bool,
             kind         : PseudoStateVars,
             entry        : Action option,
             exit         : Action option,
             doActivity   : Action option,
             outgoing     : Transition_Id list,
	     incoming     : Transition_Id list }
       | SyncState
	 of {xmiid        : string,
             name         : string,
             is_specification : bool,
             outgoing     : Transition_Id list,
	     incoming     : Transition_Id list,
	     bound        : int}
(*     | StubState  *)
	    
and	StateMachine = mk_StateMachine of 
                           {xmiid            : string,
                            contextxmiid     : string,
                            is_specification : bool,
                            top              : StateVertex,
                            transitions      : Transition list}


end



