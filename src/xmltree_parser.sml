(*****************************************************************************
 * su4sml --- an SML repository for managing (Secure)UML/OCL models
 *             http://projects.brucker.ch/su4sml/
 *                                                                            
 * xmltree_parser.sml --- an xmi-parser for the import interface for su4sml
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

structure XmlTreeParser : sig
    val readFile : string -> XmlTree.Tree
end = 
struct
open XmlTree
exception FileNotFound of string

structure Parser = Parse (structure Dtd = Dtd
                          structure Hooks = XmlTreeHooks
                          structure ParserOptions = ParserOptions ()
			  structure Resolve = ResolveNull)
		   
fun readFile filename = 
    let val currentDir = OS.FileSys.getDir()

	(* how to do the following in a clean/portable way? *)
	fun read_dtd dtd = 
	    (OS.FileSys.chDir (Config.su4sml_share());
	     (* check to see if the DTD file exists. *)
	     if OS.FileSys.access (Config.umlocl_dtd,[]) then 
	         (Parser.parseDocument 
		      (SOME (Uri.String2Uri ("file:"^(Config.umlocl_dtd))))
		      (SOME dtd) (dtd,nil,nil))
	     else Logger.error ("Error while reading UML/OCL DTD `"^(Config.umlocl_dtd)^"': "^
                         "no such file or directory");
	     OS.FileSys.chDir currentDir)

	fun read_file dtd filename = 
	    if filename = "-" 
            then Parser.parseDocument
		     (NONE)
		     (SOME dtd) (dtd,nil,nil)
	    else let (* dummy check to see if the file exists...*)
		    val _ = OS.FileSys.fileSize filename 
	        in 
		    Parser.parseDocument
		        (SOME (Uri.String2Uri filename))
		        (SOME dtd) (dtd,nil,nil)
	        end
                 handle ex => (Logger.warn ("Error while reading file " ^filename^": "^
                                          General.exnMessage ex);
                               raise ex) 
                              
	val dtd = Dtd.initDtdTables()
    in  ( read_dtd dtd; 
	  read_file dtd filename )
    end
end


