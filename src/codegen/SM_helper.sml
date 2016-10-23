(*****************************************************************************
 * su4sml --- an SML repository for managing (Secure)UML/OCL models
 *             http://projects.brucker.ch/su4sml/
 *                                                                            
 * SM_helper.sml --- 
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

structure SM_Helper : sig 
    val alwaysTrigger  : Rep.Event
    val alwaysGuard    : Rep.Guard
    val emptyEvent     : Rep.Event
    val emptyGuard     : Rep.Guard
    val emptyState     : Rep.StateVertex
    val emptyTransition: Rep.Transition
    val lastGuard      : Rep.Guard
    val emptyEffect    : Rep.Procedure
end =
struct
open Rep
open Rep_OclType
open Rep_OclTerm

(* open Rep_SecureUML_ComponentUML.Security*)
open ComponentUML
open XMI_DataTypes


val emptyEvent = CallEvent(["","","EMPTY"],[])
val emptyGuard = OperationCall(Variable("",DummyT),DummyT,["EMPTY"],[],DummyT):Guard
val emptyState = (Rep_StateMachine.State_SimpleState({ state_id="",
				      outgoing=[],
				      incoming=[],
				      name=""}))

val emptyTransition = {effect=NONE,
		       guard=NONE,
		       source="",
		       target="",
		       trans_id="",
		       trigger=NONE}
                      
val lastGuard = OperationCall(Variable("self",DummyT),DummyT,["else"],[],Boolean):Guard
val alwaysTrigger = CallEvent(["auto","auto","auto"],[])
val alwaysGuard = OperationCall(Variable("self",DummyT), DummyT, ["alwaysG"],[],Boolean):Guard
val emptyEffect = {proc_id="",
		   language="",
		   body="",
		   expression=""}
end
