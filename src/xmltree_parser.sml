(*****************************************************************************
 *          su4sml - a SecureUML repository for SML              
 *                                                                            
 * xmltree_parser.sml - an xmi-parser for the import interface for su4sml
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


structure XmlTreeParser : sig
    val readFile : string -> XmlTree.Tree
end = 
struct
open XmlTree
open library
exception FileNotFound of string

structure Parser = Parse (structure Dtd = Dtd
                          structure Hooks = XmlTreeHooks
                          structure ParserOptions = ParserOptions ()
			  structure Resolve = ResolveNull)
		   
fun readFile filename = 
    let val currentDir = OS.FileSys.getDir()

	(* how to do the following in a clean/portable way? *)
	fun read_dtd dtd = 
	    (OS.FileSys.chDir (su4sml_home());
	     (* dummy check to see if the file exists...*)	     
	     OS.FileSys.fileSize "UML15OCL.xmi" ;
	     (Parser.parseDocument 
		  (SOME (Uri.String2Uri ("file:UML15OCL.xmi")))
		  (SOME dtd) (dtd,nil,nil)
	      handle ex => (error_msg ("Error while reading file UML15OCL.xmi: "^
                                       General.exnMessage ex);
                            raise ex));
	     OS.FileSys.chDir currentDir )

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
                 handle ex => (error_msg ("Error while reading file " ^filename^": "^
                                          General.exnMessage ex);
                               raise ex) 
                              
	val dtd = Dtd.initDtdTables()
    in  ( read_dtd dtd; 
	  read_file dtd filename )
    end
end


