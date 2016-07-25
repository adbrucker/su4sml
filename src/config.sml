(*****************************************************************************
 * su4sml --- a SML repository for managing (Secure)UML/OCL models
 *             http://projects.brucker.ch/su4sml/
 *                                                                            
 * config.sml --- 
 * This file is part of su4sml.
 *
 * Copyright (c) 2009 Achim D. Brucker, Germany
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

signature CONFIG = 
sig
    val su4sml_home                : unit -> string
    val su4sml_share               : unit -> string
    val umlocl_dtd                 : string
    val unzip                      : string
    val check_umlocl_dtd           : unit -> bool
    val check_unzip                : unit -> bool
    val check_argo_import          : unit -> bool
    val check_xmi_import           : unit -> bool
end

structure Config:>CONFIG = 
struct
 
val umlocl_dtd = "UML15OCL.xmi"
val unzip      = "unzip"

(* HOLOCL_HOME resp. SU4SML_HOME should point to the top-level directory *)
(* of the corresponding library.  The semantics of UML2CDL_HOME should   *)
(* probably be fixed                                                     *)

fun su4sml_share () = case OS.Process.getEnv "HOLOCL_HOME" of
			 SOME p => p^"/lib/su4sml/share"
		       | NONE   => (case OS.Process.getEnv "SU4SML_HOME" of
				        SOME p => p^"/share"
				      | NONE => (case OS.Process.getEnv "UML2CDL_HOME" of 
                                                     SOME p => p^"../../../share"
                                                   | NONE => ".")
                                   )

fun su4sml_home () = case OS.Process.getEnv "HOLOCL_HOME" of
			 SOME p => p^"/lib/su4sml"
		       | NONE   => (case OS.Process.getEnv "SU4SML_HOME" of
				        SOME p => p
				      | NONE => (case OS.Process.getEnv "UML2CDL_HOME" of 
                                                     SOME p => p^"../../.."
                                                   | NONE => ".")
                                   )

fun check_umlocl_dtd () = (OS.FileSys.chDir (su4sml_share());OS.FileSys.access (umlocl_dtd,[]))
			  handle _ => false


fun check_unzip () = let 
  val tmpFile = OS.FileSys.tmpName ()
  val result = (Posix.Process.fromStatus(OS.Process.system (unzip^" > "^tmpFile)) = Posix.Process.W_EXITED)
  val _ = OS.FileSys.remove tmpFile       
in 
  result
end
		     
fun check_argo_import () =  (check_umlocl_dtd ()) andalso (check_unzip ())
val check_xmi_import = check_umlocl_dtd 

end
