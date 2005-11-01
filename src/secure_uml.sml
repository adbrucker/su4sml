(*****************************************************************************
 *          su4sml - a SecureUML repository for SML              
 *                                                                            
 * secure_uml.sml - a security language implementing mds.sig
 * Copyright (C) 2005 Achim D. Brucker <brucker@inf.ethz.ch>   
 *                    Juergen Doser    <doserj@inf.ethz.ch>
 *                    Burkhart Wolff   <bwolff@inf.ethz.ch>
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

(* SecureUML is a simple security language, based on RBAC, where permissions *)
(* can be further constrained using OCL: *)
functor SecureUML(structure Design: DESIGN_LANGUAGE):SECURITY_LANGUAGE =
struct

structure Design : DESIGN_LANGUAGE = Design

type User = string
fun name_of (u:User) = u
 

datatype Subject = Group of string * (string list)
                 | User of User


type Role = string
type SubjectAssignment = (Subject * (Role list)) list


(* fun actions_of (p:Permission) = #actions p*)
				 
type Permission = {name: string,
		   roles: Role list,
		   constraints: Rep_OclTerm.OclTerm list,
		   actions: Design.Action list }

fun actions_of (p:Permission) = #actions p

type Config_Type = string

type 'a partial_order = ('a * 'a) list

(* unclear yet how this will look like: 
fun domain_of (x:'a partial_order) = ...
fun closure_of (x:'a partial_order) = ...
*) 

type Configuration = { config_type: Config_Type,
		       permissions: Permission list,
		       subjects: Subject list,
		       (* groups: Group partial_order,*)
		       roles: Role partial_order,
		       sa: SubjectAssignment }

fun type_of  (c:Configuration) = #config_type c

fun is_empty (c:Configuration) = List.null (#permissions c) andalso 
				 List.null (#subjects c)


(* the following functions have yet to be implemented *)
fun users_of p = nil
fun check_permission (u,p) = false
fun permissions_of u = nil
fun parse (cs:Rep_Core.Classifier list) = (cs,{config_type = "SecureUML",
					       permissions = nil,
					       subjects = nil,
					       roles = nil,
					       sa = nil})



end


