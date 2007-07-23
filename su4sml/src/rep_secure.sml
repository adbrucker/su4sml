(*****************************************************************************
 * su4sml --- a SML repository for managing (Secure)UML/OCL models
 *             http://projects.brucker.ch/su4sml/
 *                                                                            
 * rep_secure.sml --- repository structure for uml models with security 
 *                    specifications
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

(** a repository for uml models equipped with a security language *)
signature REP_SECUREUML =
sig
	
	(** the security language used. *)
    structure Security : SECUREUML
						 
	(** 
	 * a "secure repository" model consist of a repository model 
	 * plus a security configuration.
	 *)
    type Model = Rep_Core.Classifier list * Security.Configuration
				 
	(** *) 
    val readXMI: string -> Model

    val test: (string * string list) -> OS.Process.status
			   
end

functor Rep_SecureUML(structure Security : SECUREUML) : REP_SECUREUML  =
struct

    structure Security = Security

    type Model = Rep_Core.Classifier list * Security.Configuration

    val readXMI = Security.parse o RepParser.readFile

    fun test (_,filename::_) = (Rep2String.printList (#1 (readXMI filename)); OS.Process.success)
end

structure Rep_SecureUML_ComponentUML 
  = Rep_SecureUML(structure Security = SecureUML(structure Design=ComponentUML))
			

(* structure Rep_SecureUML_ControllerUML
  = Rep_Secure(structure Security = SecureUML(structure Design=ControllerUML))*)
