
(*****************************************************************************
 *          su4sml - a SecureUML repository for SML              
 *                                                                            
 * xmltree_writer.sml - a module for writing xml trees
 * Copyright (C) 2005  Achim D. Brucker <brucker@inf.ethz.ch>
 *                     Jürgen Doser <doserj@inf.ethz.ch>
 *                                                                            
 * This file is part of su4sml.                                              
 *                                                                            
 * su4sml is free software; you can redistribute it and/or modify it under   
 * the terms of the GNU General Public License as published by the Free       
 * Software Foundation; either version 2 of the License, or (at your option)  
 * any later version.                                                         
 *                                                                            
 * su4sml is distributed in the hope that it will be useful, but WITHOUT ANY 
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS 
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more 
 * details.                                                              
 *                                                                            
 * You should have received a copy of the GNU General Public License along    
 * with this program; if not, write to the Free Software Foundation, Inc.,    
 * 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.                  
 ******************************************************************************)



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
