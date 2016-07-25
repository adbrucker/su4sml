(*****************************************************************************
 * su4sml --- a SML repository for managing (Secure)UML/OCL models
 *             http://projects.brucker.ch/su4sml/
 *                                                                            
 * polyml.sml --- interactive eval for poly/ml 5.2 and later 
 * This file is part of su4sml.
 *
 * Copyright (c) 2005-2007 ETH Zurich, Switzerland
 *               2008      Achim D. Brucker, Germany
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
(* $Id: polyml.sml 7549 2008-03-27 21:54:01Z brucker $ *)

structure CompilerExt : COMPILER_EXT = 
struct

exception EvalNotSupported
fun drop_last [] = []
  | drop_last [x] = []
  | drop_last (x :: xs) = x :: drop_last xs;

fun eval verbose txt =
    let 
      fun drop_newline s =
	  if String.isSuffix "\n" s then String.substring (s, 0, size s - 1)
	  else s;
	fun eval_fh (print, err) verbose txt =
	    let
		val in_buffer = ref (String.explode txt);
		val out_buffer = ref ([]: string list);
(*		fun output () = SML90.implode (drop_last (rev (! out_buffer))); *)
		fun output () = drop_newline (SML90.implode (rev (! out_buffer)));
		fun get () =
		    (case ! in_buffer of
			 [] => NONE
		       | c :: cs => (in_buffer := cs; SOME c));
		fun put s = out_buffer := s :: ! out_buffer;
		    
		val parameters =
		    [PolyML.Compiler.CPOutStream put]

		fun exec () =
		    (case ! in_buffer of
			 [] => ()
		       | _ => (PolyML.compiler (get, parameters) (); exec ()));
	    in
		exec () handle exn => (err (output ()); raise exn);
		if verbose then print (output ()) else ()
	    end
    in	
	eval_fh (fn s => print (s^"\n"), fn s => Logger.error (s^"\n")) verbose txt
    end

fun exnHistory _ = []

end
