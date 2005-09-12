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
				  effect  : Procedure option
				 (* mmm    : StateVertexId option *)
			         }
				 
				   
datatype PseudoStateVars = initial  | deep   | shallow |
                           join     | fork   | 
                           junction | choice 
				  
datatype StateVertex  = 
         CompositeState 
	 of {xmiid        : string,
             name         : string,
             isSpecification : bool,
             isConcurrent : bool,
             entry        : Procedure option,
             exit         : Procedure option,
             doActivity   : Procedure option,
             outgoing     : Transition_Id list,
	     incoming     : Transition_Id list, 
	     subvertex    : StateVertex list}
       | SubactivityState
	 of {xmiid        : string,
             name         : string,
             isSpecification : bool,
             isConcurrent : bool,
             entry        : Procedure option,
             exit         : Procedure option,
             doActivity   : Procedure option,
             outgoing     : Transition_Id list,
	     incoming     : Transition_Id list, 
	     subvertex    : StateVertex list,
             submachine   : StateMachine,
             isDynamic    : bool}
       | SimpleState
	 of {xmiid        : string,
             name         : string,
             isSpecification : bool,
             entry        : Procedure option,
             exit         : Procedure option,
             doActivity   : Procedure option,
             outgoing     : Transition_Id list,
	     incoming     : Transition_Id list}
       | ActionState (* from ActivityGraphs *)
         of {xmiid        : string,
             name         : string,
             isSpecification : bool,
             entry        : Procedure option,
             exit         : Procedure option,
             doActivity   : Procedure option,
             outgoing     : Transition_Id list,
	     incoming     : Transition_Id list, 
             isDynamic    : bool 
             (* + dynamicArguments + dynamicMultiplicity *)}
       | ObjectFlowState (* from ActivityGraphs *)
         of {xmiid        : string,
             name         : string,
             isSpecification : bool,
             entry        : Procedure option,
             exit         : Procedure option, 
             doActivity   : Procedure option,
             outgoing     : Transition_Id list,
	     incoming     : Transition_Id list, 
             isSynch      : bool,
             parameter    : Parameter list,
             type_        : Rep_OclType.Path option}
       | FinalState
	 of {xmiid        : string,
             name         : string,
             isSpecification : bool,
             entry        : Procedure option,
             exit         : Procedure option,
             doActivity   : Procedure option,
             outgoing     : Transition_Id list,
	     incoming     : Transition_Id list }
       | PseudoState
	 of {xmiid        : string,
             name         : string,
             isSpecification : bool,
             kind         : PseudoStateVars,
             entry        : Procedure option,
             exit         : Procedure option,
             doActivity   : Procedure option,
             outgoing     : Transition_Id list,
	     incoming     : Transition_Id list }
       | SyncState
	 of {xmiid        : string,
             name         : string,
             isSpecification : bool,
             outgoing     : Transition_Id list,
	     incoming     : Transition_Id list,
	     bound        : int}
(*     | StubState  *)
	    
and	StateMachine = mk_StateMachine of 
                           {xmiid            : string,
                            contextxmiid     : string,
                            isSpecification : bool,
                            top              : StateVertex,
                            transitions      : Transition list}


end



