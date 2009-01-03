(*****************************************************************************
 * su4sml --- a SML repository for managing (Secure)UML/OCL models
 *             http://projects.brucker.ch/su4sml/
 *                                                                            
 * logger.sml --- 
 * This file is part of su4sml.
 *
 * Copyright (c) 2005-2007 ETH Zurich, Switzerland
 *               2008-2009 Achim D. Brucker, Germany
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

signature LOGGER = 
sig
   type log_level
   val set_log_level   : log_level -> unit
   val get_log_level   : unit -> log_level

   val set_strict_logging   : bool -> unit
   val get_strict_logging   : unit -> bool


   val ERROR : log_level
   val WARN : log_level
   val INFO : log_level

   val DEBUG_1 : log_level
   val DEBUG_2 : log_level
   val DEBUG_3 : log_level
   val DEBUG_4 : log_level
   val DEBUG_5 : log_level

   val get_log_level_str : unit -> string
  
   val error:            string -> 'a 
   val errorExn:         exn -> string -> 'a
   val warn:             string -> unit
   val info:             string -> unit
   val debug1:             string -> unit
   val debug2:             string -> unit
   val debug3:             string -> unit
   val debug4:             string -> unit
   val debug5:             string -> unit
end   

structure Logger:>LOGGER = 
struct 
  type log_level = int 
  
  fun std_output s = print s
  fun std_error s = (TextIO.output (TextIO.stdErr, s); TextIO.flushOut TextIO.stdErr )

  infix 1 |>
  fun x |> f = f x

  val ERROR   =  0
  val WARN    = 10
  val INFO    = 20
  val DEBUG_1 = 30
  val DEBUG_2 = 40
  val DEBUG_3 = 50
  val DEBUG_4 = 60
  val DEBUG_5 = 70

  val logLevel = ref WARN
  fun set_log_level l = (logLevel := l;()) 
  fun get_log_level () = !logLevel	

  val strictLogging = ref false
  fun set_strict_logging l = (strictLogging := l;()) 
  fun get_strict_logging () = !strictLogging	

		
  fun get_log_level_str () = case !logLevel of 
			        0 => "error"
			     | 10 => "warn" 
			     | 20 => "info"
			     | 30 => "debug 1"
			     | 40 => "debug 2"
			     | 50 => "debug 3"
			     | 60 => "debug 4"
			     | _  => "debug 5"

  fun separate s (x :: (xs as _ :: _)) = x :: s :: separate s xs
    | separate _ xs = xs;

  fun space_implode a bs = String.concat (separate a bs)
  fun space_explode _ "" = []
    | space_explode sep s = String.fields (fn c => str c = sep) s
  val split_lines = space_explode "\n"
  val cat_lines = space_implode "\n"

  fun prefix_lines "" txt = txt
    | prefix_lines prfx txt = txt |> split_lines |> map (fn s => prfx ^ s) |> cat_lines;
  fun prefix prfx s = prfx ^ s
  fun suffix sffx s = s ^ sffx
      
		     
   fun mk_error_string s = s |> prefix_lines "*** " |> suffix "\n"
   fun mk_warn_string s  = s |> prefix_lines "### " |> suffix "\n"
   fun mk_info_string s  = s |> prefix_lines "+++ " |> suffix "\n"
   fun mk_debug_string s = s |> prefix_lines "::: " |> suffix "\n"

   fun trace log msg = if (!strictLogging andalso log = !logLevel) orelse (not (!strictLogging) andalso log <= !logLevel)
		       then print msg
		       else ()


   fun error s = ((print o mk_error_string) s; raise Fail s)
   fun errorExn ex s = ((print o mk_error_string) s; raise ex)
   fun warn   msg = msg |> mk_warn_string |> trace WARN
   fun info   msg = msg |> mk_info_string |> trace INFO
   fun debug1 msg = msg |> mk_debug_string |> trace DEBUG_1
   fun debug2 msg = msg |> mk_debug_string |> trace DEBUG_2
   fun debug3 msg = msg |> mk_debug_string |> trace DEBUG_3
   fun debug4 msg = msg |> mk_debug_string |> trace DEBUG_4
   fun debug5 msg = msg |> mk_debug_string |> trace  DEBUG_5
 	    
(*   fun printStackTrace e =
       let val ss = CompilerExt.exnHistory e
       in
         print("uncaught exception " ^ (General.exnMessage e) ^ " at:\n");
         app (fn s => print ("\t" ^ s ^ "\n")) ss
       end
*)
end
