(*****************************************************************************
 * su4sml --- an SML repository for managing (Secure)UML/OCL models
 *             http://projects.brucker.ch/su4sml/
 *                                                                            
 * xmltree_writer.sml --- a module for writing xml trees
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

(* supposed to print a XmlTree to a xml file.        *)
(* Works in principle, but is not UTF-8 clean        *)
structure WriteXmlTree: sig
    val writeFile : string -> XmlTree.Tree -> unit
    val writeStdOut : XmlTree.Tree -> unit
end =
struct
open XmlTree

val escape = String.translate(fn #"'"  => "&apos;"
			       | #"<"  => "&lt;"
			       | #">"  => "&gt;"
			       | #"\"" => "&quot;"
			       | #"&"  => "&amp;"
			       | c     => str(c))  

val escape2 = String.translate(fn #"<"  => "&lt;"
				| #">"  => "&gt;"
				| #"\"" => "&quot;"
				| #"&"  => "&amp;"
				| c     => str(c))  

fun writeAttribute stream (name,value) =
    TextIO.output (stream, " "^(escape name)^"=\""^(escape2 value)^"\"")

fun writeEndTag stream name = TextIO.output (stream,"</"^(escape name)^">\n")

fun writeStartTag stream tree = 
    (TextIO.output (stream,"<"^escape (tagname tree));
     List.app (writeAttribute stream) (attributes tree);
     TextIO.output (stream,">\n"))

fun writeStartEndTag stream tree =
    (TextIO.output (stream,"<"^escape (tagname tree));
     List.app (writeAttribute stream) (attributes tree);
     TextIO.output (stream, " />\n"))

fun writeIndent stream 0 = ()
  | writeIndent stream n = (TextIO.output (stream, "  "); writeIndent stream (n-1))
    

fun writeXmlTree indent stream (Text s) =  
    (writeIndent stream indent;
     TextIO.output (stream, s^"\n"))
  | writeXmlTree indent stream tree = 
    let val elemName = escape (tagname tree)
    in 
	writeIndent stream indent;
	if children tree = nil 
	then writeStartEndTag stream tree
	else (writeStartTag stream tree;
	      List.app (writeXmlTree (indent+1) stream) (children tree);
	      writeIndent stream indent;
	      writeEndTag stream elemName)
    end


fun writeFile filename tree = 
    let val stream = TextIO.openOut filename 
    in 
	TextIO.output (stream,"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");
	writeXmlTree 0 stream tree;
	TextIO.closeOut stream
    end

fun writeStdOut tree = 
    let val stream = TextIO.stdOut
    in 
	TextIO.output (stream,"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");
	writeXmlTree 0 stream tree;
	TextIO.closeOut stream
    end
end
