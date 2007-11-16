(*****************************************************************************
 * su4sml --- a SML repository for managing (Secure)UML/OCL models
 *             http://projects.brucker.ch/su4sml/
 *                                                                            
 * library.sml --- 
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

structure library =
struct
infix |>
fun (x |> f) = f x;


(* HOLOCL_HOME resp. SU4SML_HOME should point to the top-level directory *)
(* of the corresponding library.  The semantics of UML2CDL_HOME should   *)
(* probably be fixed                                                     *)
fun su4sml_home () = case OS.Process.getEnv "HOLOCL_HOME" of
			 SOME p => p^"/lib/su4sml/src"
		       | NONE   => (case OS.Process.getEnv "SU4SML_HOME" of
				        SOME p => p^"/src"
				      | NONE => (case OS.Process.getEnv "UML2CDL_HOME" of 
                                                     SOME p => p^"../../../src"
                                                   | NONE => ".")
                                   )


fun filter (pred: 'a->bool) : 'a list -> 'a list =
    let fun filt [] = []
          | filt (x :: xs) = if pred x then x :: filt xs else filt xs
	in filt end;
    
    
fun exists (pred: 'a -> bool) : 'a list -> bool =
	let fun boolf [] = false
	      | boolf (x :: xs) = pred x orelse boolf xs
	in boolf end;
    
    
fun append xs ys = xs @ ys;
    
fun find _ []        = Option.NONE
  | find p (x :: xs) = if p x then Option.SOME x else find p xs;
    


(* fun getenv var =
  (case OS.Process.getEnv var of
      NONE => ""
        | SOME txt => txt);
*)
(*	                                
fun print_depth n = 
   (Control.Print.printDepth := n div 2;
    Control.Print.printLength := n); 

val cd = OS.FileSys.chDir;
val pwd = OS.FileSys.getDir;
*)

fun foldr1 f l =
    let fun itr [x] = x
          | itr (x::l) = f(x, itr l)
    in  itr l  end;

(* use Option.map instead 
fun ap_some f (SOME x) = SOME(f x)
  |ap_some f NONE     = NONE
*)

fun separate s (x :: (xs as _ :: _)) = x :: s :: separate s xs
  | separate _ xs = xs;
(* fun suffix sfx s = s ^ sfx;*)

fun take (n, []) = []
  | take (n, x :: xs) =
    if n > 0 then x :: take (n - 1, xs) else [];
    
fun space_implode a bs = implode (separate a bs);

fun print_stderr s = (TextIO.output (TextIO.stdErr, s); TextIO.flushOut TextIO.stdErr);

exception ERROR;
   
(* val writeln = std_output o suffix "\n";*)
(* fun error_msg s = writeln(s) *)

(** output an informational message about what is going on. *)
fun info s  = print (s^"\n")

(** output a warning that something is wrong, 
 * but it is dealt with somehow.  *)
fun warn s  = print ("Warning: "^s^"\n")

(** output an error message *)
fun error_msg s = print (s^"\n")

(** output an error message and Fail *)
fun error s = (print (s^"\n"); raise Fail s)


fun fst (x, y) = x
                 
fun snd (x, y) = y

fun join s nil = ""
  | join s (h::nil) = h
  | join s (h::t) = h^s^(join s t)

end
