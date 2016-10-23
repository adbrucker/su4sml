(*****************************************************************************
 * su4sml --- an SML repository for managing (Secure)UML/OCL models
 *             http://projects.brucker.ch/su4sml/
 *                                                                            
 * gcg_helper.sml --- helper library for su4sml-gcg
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

structure Gcg_Helper =
struct

fun fieldSplit d s = String.fields (fn c => (c = d)) s
fun tokenSplit d s = String.tokens (fn c => (c = d)) s
                                   
local
	fun endsWithEscape "" = false
	  | endsWithEscape s = (substring(s,size(s)-1,1) = "\\")
in
 fun joinEscapeSplitted d [] 	   = []
   | joinEscapeSplitted d [l]	   = [l]
   | joinEscapeSplitted d (h::s::t) = if endsWithEscape(h)
  				      then (substring(h,0,size(h)-1)^d^s)::t
  				      else h::(joinEscapeSplitted d (s::t))
end 

(* FIXME: move to library.sml? *)
val curry = fn f => fn x => fn y => f (x, y)
val uncurry = fn f => fn (x, y) => f x y
 
(* FIXME: move to ListEq (and rename to isPrefix...) (is this even used somewhere?) *)
fun isSuffix  [] _ = true
 |  isSuffix  _ [] = false
 |  isSuffix (h1::t1) (h2::t2) = (h1=h2) andalso (isSuffix t1 t2)

(* FIXME: cleanup *)
fun assureDir file = 
    let val dirList = rev (tl (rev (String.tokens (fn c => c = #"/") file)))

	fun assert1 "" d = ((if (OS.FileSys.isDir d) then () else ())
			    handle SysErr => (OS.FileSys.mkDir d) )
	  | assert1 prefix d = (if (OS.FileSys.isDir (prefix^"/"^d)) then () else ())
	    handle SysErr => (OS.FileSys.mkDir (prefix^"/"^d))
			     

	fun assertDList _ [] = ()
	  | assertDList prefix [d] = assert1 prefix d
	  | assertDList "" (h::t) = (assert1 "" h ; assertDList h t)
	  | assertDList prefix (h::t) = (assert1 prefix h; 
			   		 assertDList (prefix^"/"^h) t ) 
    in
	assertDList "" dirList
    end


end
