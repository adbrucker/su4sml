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

(** Repository datatypes and helper functions for activity graphs. *)
signature REP_ACTIVITY_GRAPH = 
sig
include  REP_STATE_MACHINE  

type     ActivityGraph = StateMachine
    
end

structure Rep_ActivityGraph : REP_ACTIVITY_GRAPH = 
struct
open     Rep_StateMachine 
type     ActivityGraph = StateMachine
    
end
