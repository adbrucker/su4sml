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
signature REP_LOGGER =
sig
    val trace                      : int -> string -> unit
    val init_offset                : unit -> unit

    val error                      : string -> 'a
    val error_msg                  : string -> unit
    val print_stderr               : TextIO.vector -> unit
    val warn                       : string -> unit
    val info                       : string -> unit
    val log_level                  : int ref

    val line_offset                   : int ref
    (** 
     * log_levels 
     *)
    val zero                       : int
    val exce                       : int
    val high                       : int
    val medium                     : int
    val function_calls             : int
    val function_ends              : int
    val function_arguments         : int
    val important                  : int
    val wgen                       : int
    val type_checker               : int
    val preprocessor               : int
    val rep_core                   : int
    val low                        : int
    val development                : int

    val su4sml_home                : unit -> string

end
structure Rep_Logger:REP_LOGGER =
struct




(* minimal tracing support (modifed version of ocl_parser tracing *)
val log_level = ref 6

val line_offset = ref 4

fun get_spaces 0 = ""
  | get_spaces x = (" ")^(get_spaces (x-1))

fun init_offset () = line_offset:=4
 

fun get_offset () = get_spaces (!line_offset)

fun inc_offset () = line_offset := (!line_offset)+2

fun dec_offset () = line_offset := (!line_offset)-2

(* debugging-levels *)
val zero = 0
val exce = 6
val high = 10
val medium = 20
val function_calls = 25
val function_ends = 26
val function_arguments = 27
val important = 40
val wgen = 50
val type_checker = 60
val preprocessor = 61
val rep_core = 80
val low = 100
val development = 200


fun trace lev s = 
    case lev of
	6 => 
	let
	    val s1 = ("\n\n\n##################################################\n")
	    val s2 =       ("##############  EXCEPTION MESSAGE ################\n")
	    val s3 =       ("##################################################\n\n")
	in
	    if (lev  <= !log_level ) then print(s1^s2^s3^s) else ()
	end
      |	25 => 
	let
	    val _ = if (lev  <= !log_level ) then print((get_offset())^s) else ()
	in
	    inc_offset()
	end
      | 26 => 
	let
	    val x = dec_offset()
	in
	    if (lev  <= !log_level ) then print((get_offset())^s) else ()
	end
      | x => 
	if x < 20 
	then (if (lev  <= !log_level ) then print(s) else ())
	else (if (lev  <= !log_level ) then print((get_offset())^s) else ())


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

fun real_path x = List.rev (List.tl (List.rev x))    


fun optlist2list [] = []
  | optlist2list (h::tail) =
    (
     case h  of
	 NONE => optlist2list (tail)
       | SOME (e) => (e::(optlist2list tail))
    )
    
fun exists (pred: 'a -> bool) : 'a list -> bool =
	let fun boolf [] = false
	      | boolf (x :: xs) = pred x orelse boolf xs
	in boolf end;
    
    
fun append xs ys = xs @ ys;
    
fun find _ []        = Option.NONE
  | find p (x :: xs) = if p x then Option.SOME x else find p xs;
    
fun member x [] = false
| member x (h::tail) = 
    if (x = h) then
	true
    else 
	member x tail

fun swap1 f a b c = f c b a

(* fun getenv var =
  (case OS.Process.getEnv var of
      NONE => ""
        | SOME txt => txt);
*)
(*	                                
fun print_depth n = 
   (Control.Print.printDepth := n div 2;
    Control.Print.printLength := n); 
*)

val cd = OS.FileSys.chDir
val pwd = OS.FileSys.getDir

(* use Option.map instead 
fun ap_some f (SOME x) = SOME(f x)
  |ap_some f NONE     = NONE
*)

fun separate s (x :: (xs as _ :: _)) = x :: s :: separate s xs
  | separate _ xs = xs;
(* fun suffix sfx s = s ^ sfx;*)
    
fun space_implode a bs = implode (separate a bs);

fun print_stderr s = (TextIO.output (TextIO.stdErr, s); TextIO.flushOut TextIO.stdErr);

exception ERROR;
   
(** output an informational message about what is going on. *)
fun info s  = print (s^"\n")

(** output a warning that something is wrong, 
 * but it is dealt with somehow.  *)
fun warn s  = print ("Warning: "^s^"\n")

(** output an error message *)
fun error_msg s = print (s^"\n")

(** output an error message and Fail *)
fun error s = (print (s^"\n"); raise Fail s)

fun remove_dup [] = []
  | remove_dup (h::tail) = if (member h tail) then (remove_dup tail) else ((h)::(remove_dup tail))

end
