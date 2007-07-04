(*****************************************************************************
 * su4sml --- a SML repository for managing (Secure)UML/OCL models
 *             http://projects.brucker.ch/su4sml/
 *                                                                            
 * smlnj.sml --- interactive eval stub (not supported by MLton)
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

structure CompilerExt : COMPILER_EXT = 
struct
exception EvalNotSupported

fun eval verbose txt =
    let 
	fun eval_fh (print, err) verbose txt =
	    let
		val ref out_orig = Control.Print.out;
		    
		val out_buffer = ref ([]: string list);
		val out = {say = (fn s => out_buffer := s :: ! out_buffer), 
			   flush = (fn () => ())};
		fun output () =
		    let val str = SML90.implode (rev (! out_buffer))
	    in String.substring (str, 0, Int.max (0, size str - 1)) end;
	    in
		Control.Print.out := out;
		Backend.Interact.useStream (TextIO.openString txt) 
		handle exn =>
		       (Control.Print.out := out_orig; err (output ()); raise exn);
		Control.Print.out := out_orig;
		if verbose then print (output ()) else ()
	    end
    in	
	eval_fh (fn s => print (s^"\n"), fn s => library.error s) verbose txt
    end

fun exnHistory e = SMLofNJ.exnHistory e
 
end
