(*****************************************************************************************)
(* 		   su4sml - State Machine generator (SMG)				 *)
(* 			    based upon GCG						 *)
(* 											 *)
(*		    types.sml - types used by SMG.                                       *)
(* 											 *)
(* 	    Copyright (C) by Rolf Simon Adelsberger (RSA)				 *)
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

structure StateMachineTypes =
struct

open Rep
open Rep_OclType
open Rep_OclTerm
open Rep_StateMachine
open Rep_SecureUML_ComponentUML.Security

type Pair = string * string
fun VarN((Name,Value):Pair) = Name
fun VarV((Name,Value):Pair) = Value

(* Event-Guard-Path *)
datatype GEPath = GELeaf of Guard option * Event option * StateVertex_Id
		| GEBranch of Guard option * Event option * GEPath list

type SM_Trans = {
		 trans_id : Transition_Id,
		 source : StateVertex_Id,
		 target : StateVertex_Id,
		 guards : Guard list,
		 triggers : Event list,
		 effects : Procedure list
		 }

end
