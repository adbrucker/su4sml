(*****************************************************************************************)
(* 		   su4sml - State Machine generator (SMG)				 *)
(* 			    based upon GCG						 *)
(* 											 *)
(* c#sm_cartridge.sml - implementation of the Statechart->StateMachine cartridge.	 *)
(* 											 *)
(* 	    Copyright (C) 2005 by Rolf Simon Adelsberger (RSA)				 *)
(* 			<rsa@student.ethz.ch>						 *)
(* 											 *)
(* This file is part of the StateMachine cartridge for su4sml 				 *)
(*                                                                            		 *)
(* su4sml is free software; you can redistribute it and/or modify it under   		 *)
(* the terms of the GNU General Public License as published by the Free       		 *)
(* Software Foundation; either version 2 of the License, or (at your option)  		 *)
(* any later version.                                                         		 *)
(*                                                                            		 *)
(* su4sml is distributed in the hope that it will be useful, but WITHOUT ANY 		 *)
(* WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS 		 *)
(* FOR A PARTICULAR PURPOSE. See the GNU General Public License for more 		 *)
(* details.                                                              		 *)
(*                                                                            		 *)
(* You should have received a copy of the GNU General Public License along    		 *)
(* with this program; if not, write to the Free Software Foundation, Inc.,    		 *)
(* 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.				 *)
(*****************************************************************************************)

(** A cartridge supporting state machines. *)
signature SM_CARTRIDGE =
sig 
include CARTRIDGE
end
