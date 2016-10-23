(*****************************************************************************
 * su4sml --- an SML repository for managing (Secure)UML/OCL models
 *             http://projects.brucker.ch/su4sml/
 *                                                                            
 * cartridge.sig --- the minimal signature every cartridge has to implement
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

(** the minimal signature every code-generator cartridge has to implement. *)
signature CARTRIDGE =
sig
	
	(** 
	 * the environment in which template-file statements are to be evaluated. 
	 * Ususally this will contain lists of model elements and
	 * "pointers" to the "current" elements
	 *)
	type environment

	(** 
	 * The particular model from which model element information is
	 * taken. 
	 * This can be cartridge specific.
	 *)
	type Model
		 
	(** 
	 * returns the model information as it is part of the current
	 * environment. 
	 *) 
	(* val getModel : environment -> Model *)

	(** initialze the environment by parsing the given classifier list *)
	val initEnv : Rep.Model ->  environment
													   
	(** look up string-valued variables in the environment by name. *)
	val lookup : environment -> string -> string

	(** evaluate boolean-valued predicates in the environment by name. *)
	val test : environment -> string -> bool

	(** 
	 * return a list of environment, where the "current" element
	 * iterates over a given list. 
	 *)
	val foreach : string ->  environment -> environment list
end
