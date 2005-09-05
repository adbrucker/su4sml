(*****************************************************************************
 *          su4sml - a SecureUML repository for SML              
 *                                                                            
 * state_machine.sig - generic state machines
 * Copyright (C) 2005  Achim D. Brucker <brucker@inf.ethz.ch>   
 *                     Jürgen Doser <doserj@inf.ethz.ch>    
 *                     Burkhart Wolff   <bwolff@inf.ethz.ch>    
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

signature STATE_MACHINE = 
sig
    
type     StateVertex_Id
type     Transition_Id
	 
datatype Action = create 
		| call 
		| return
		| send
		| terminate
		| uninterpreted
		| destroy
		| sequence

datatype StateMachine = SM_mk of {top : StateVertex_Id,
                                  transition : Transition_Id list}

datatype Guard        = G_mk of  {expression : ocl_term.OclTerm}

type     Parameter    = ocl_type.OclType
	      
datatype Event  = SignalEvent  of Parameter list
                | CallEvent    of Parameter list
				 (*   | TimeEvent    of Parameter list  *)
				 (*   | ChangeEvent  of Parameter list  *)
				 
				 
datatype Transition   = T_ml of  {source  : StateVertex_Id,
                                  target  : StateVertex_Id,
			 	  guard   : Guard  option,
				  trigger : Event  option,
				  effect  : Action option
				 (* mmm    : StateVertexId option *)
			         }
				 
				   
datatype PseudoStateVars = initial | (* deep | shallow | *)  
                           join | fork | 
                           junction | choice 
				  
datatype StateVertex  = 
         State_CompositState 
	 of {outgoing     : Transition_Id list,
	     incoming     : Transition_Id list, 
	     container    : StateVertex_Id option,
	     subvertex    : StateVertex_Id list,
             isConcurrent : bool,
             submachine   : StateMachine * 
                            {isDynamic : bool
                             (* + dynamicArguments 
                                + dynamicMultiplicity *)} option}
                            (* variant for Subactivity State *)
       | State_SimpleState
	 of {outgoing     : Transition_Id list,
	     incoming     : Transition_Id list,
	     container    : StateVertex_Id option}
       | SimpleState_ActionState (* from ActivityGraphs *)
         of {isDynamic    : bool 
             (* + dynamicArguments + dynamicMultiplicity *)}
       | SimpleState_ObjectflowState (* from ActivityGraphs *)
         of {isSynch      : bool,
             parameter    : Parameter list,
             types        : ocl_type.Path list (* Classifier_Id *)}
       | State_FinalState
	 of {outgoing     : Transition_Id list,
	     incoming     : Transition_Id list,
	     container    : StateVertex_Id option}
       | PseudoState
	 of {kind         : PseudoStateVars,
             outgoing     : Transition_Id list,
	     incoming     : Transition_Id list,
	     container    : StateVertex_Id option}
       | SyncState
	 of {outgoing     : Transition_Id list,
	     incoming     : Transition_Id list,
	     container    : StateVertex_Id option}
(*	  | StubState  *)
	    
	    

				 
end
