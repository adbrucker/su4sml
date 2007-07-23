(*****************************************************************************
 * su4sml --- a SML repository for managing (Secure)UML/OCL models
 *             http://projects.brucker.ch/su4sml/
 *                                                                            
 * rep_secureuml.sml --- 
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

structure Rep_SecureUML : REP_SECUREUML =
struct
 
datatype Subject = User of string
	         | Group of Group
withtype Group = string * Subject list

(* perhaps we find the need for a more elaborate type later *)
type Role = string 

type RoleAssignment = (Subject * Role) list

type RoleHierarchy = (Role * Role) list

(* computes the reflexiv and transitive closure of rh starting from *)
(* the given role                                                   *)
(* fun inherited_roles rh role = ... *)

(* Resources according to ComponentUML. This will have to be adapted when we *)
(* support something like ControllerUML.                                     *)
datatype Resource = Entity of Rep_OclType.Path
	          | EntityMethod of Rep_OclType.Path
                  | EntityAttribute of Rep_OclType.Path
(* | EntityAssociationEnd of Rep.Path ??? *)

(* FIX: *)
fun contained_resources e = nil

datatype ActionName = Create | Read | Update | Delete | FullAccess | Execute

datatype ProtectedAction = SimpleAction of ActionName * Resource 
		         | CompositeAction of ActionName * Resource

fun subordinated_actions (SimpleAction _) = nil
  | subordinated_actions (CompositeAction (Read,Entity c)) = nil
    (* let val read_attributes = ...
	val read_methods = ...
    in 
	List.concat [read_attributes,read_methods]
    end *)
(*  | subordinated_actions (CompositeAction (_,_)) = ...*)
    

type Permission = { name:        string,
		    roles:       Role list,
		    constraints: Rep_OclTerm.OclTerm list,
		    actions:     ProtectedAction list
		    }

end
