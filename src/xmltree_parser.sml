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


structure XmlTree :
sig
    type Attribute
    type Tag = string * Attribute list
    datatype Tree = Node of Tag * Tree list 
                  | Text of string

    val text_of     : Tree -> string
    val tag_of      : Tree -> Tag
    val attributes_of : Tree -> Attribute list
    val children_of : Tree -> Tree list
    val node_children_of : Tree -> Tree list
    val text_children_of : Tree -> Tree list
    val tagname_of  : Tree -> string
    val attvalue_of : string -> Attribute list -> string option

    val skip        : string -> Tree -> Tree list 
    val filter      : string -> Tree list -> Tree list
    val filter_children : string -> Tree -> Tree list
    val find_some   : string -> Tree list -> Tree option
    val find        : string -> Tree list -> Tree
    val find_child  : string -> Tree -> Tree
    val dfs         : string -> Tree -> Tree option
    val exists      : string -> Tree list -> bool
    val has_child   : string -> Tree -> bool
    val follow      : string -> Tree list -> Tree list
    val follow_all  : string -> Tree list -> Tree list list

    val apply_on    : string -> (Attribute list -> Tree list -> 'a) -> Tree -> 'a
    exception IllFormed of string
end =
struct
open library
exception IllFormed of string

type Attribute = (string * string) 

(* Tags consist of element names, and a list of attribute name-value pairs *)
type Tag = string * Attribute list

datatype Tree = Node of Tag * Tree list
              | Text of string

val filter_nodes = List.filter (fn Node x => true
				 | _      => false)

val filter_text  = List.filter (fn Text x => true
				 | _      => false)
		   
fun text_of (Text s) = s
  | text_of _        = raise IllFormed "text_of called on Node element"
			    
fun tag_of  (Node (tag,trees)) = tag 
  | tag_of  _                  = raise IllFormed "tag_of called on a Text-Node"

fun attributes_of (Node ((elem,atts),trees)) = atts
  | attributes_of _ = raise IllFormed "attributes_of called on a Text-Node"

fun children_of   (Node ((elem,atts),trees)) = trees
  | children_of   _ = raise IllFormed "children_of called on a Text-Node"

fun node_children_of (Node ((elem,atts),trees)) = filter_nodes trees
  | node_children_of   _ = raise IllFormed "node_children_of called on a Text-Node"

fun text_children_of (Node ((elem,atts),trees)) = filter_text trees
  | text_children_of _ = raise IllFormed "node_children_of called on a Text-Node"

fun tagname_of    (Node ((elem,atts),trees)) = elem
  | tagname_of    (Text _) = ""

fun attvalue_of string atts = Option.map #2 (List.find (fn (x,_) => x = string) atts)

fun skip string tree = if string = tagname_of tree 
		       then node_children_of tree
		       else raise IllFormed ("in XmlTree.skip: did not find element "^string)
				  
fun filter string trees = List.filter (fn x => string = tagname_of x) 
				      trees
fun filter_children string tree = List.filter (fn x => string = tagname_of x) 
				      (node_children_of tree)
				      
fun find_some string trees = (List.find (fn x => string = tagname_of x) trees)

fun find string trees = valOf (List.find (fn x => string = tagname_of x) trees) 
    handle Option => raise IllFormed ("in XmlTree.find: did not find element "^string)

fun find_child string tree = valOf (List.find (fn x => string = tagname_of x) (node_children_of tree)) 
    handle Option => raise IllFormed ("in XmlTree.find_child: did not find element "^string)
			   
fun dfs string tree = if tagname_of tree = string 
		      then SOME tree
		      else Option.join (List.find Option.isSome (List.map (dfs string) (node_children_of tree)))

fun exists string trees = List.exists (fn x => string = tagname_of x) trees 
fun has_child string tree = List.exists (fn x => string = tagname_of x) (node_children_of tree) 
			  
fun follow string trees = if exists string trees
			  then (node_children_of o (find string)) trees
			  else []
		    
fun follow_all string trees = map node_children_of (filter string trees) 
			     			    
fun apply_on name f tree = 
    if tagname_of tree = name
    then f (attributes_of tree) (node_children_of tree)
    else raise IllFormed ("in XmlTree.apply_on: did not find element "^name)


end

structure XmlTreeHooks : Hooks =
struct
open IgnoreHooks XmlTree UniChar HookData
exception IllFormed

type AppData = Dtd.Dtd * Tree list * (Tag * Tree list) list
type AppFinal = Tree
(* val appStart = (nil,nil) *)
		
fun attspec2name dtd nil = nil 
  | attspec2name dtd ((i,AP_PRESENT (s,v,_),_)::atts) =
    let val attName = UniChar.Data2String (Dtd.Index2AttNot dtd i) 
	val attValue = UniChar.Vector2String v
    in 
	(attName,attValue)::(attspec2name dtd atts)
    end
  | attspec2name dtd ((i,AP_DEFAULT (s,v,_),_)::atts) =
    let val attName = UniChar.Data2String (Dtd.Index2AttNot dtd i) 
	val attValue = UniChar.Vector2String v
    in 
	(attName,attValue)::(attspec2name dtd atts)
    end
  | attspec2name dtd (_::atts) = attspec2name dtd atts
    

fun hookStartTag ((dtd,content, stack), (_,elem,atts,_,empty)) =
    let val elemName = UniChar.Data2String (Dtd.Index2Element dtd elem) 
	val attNames = attspec2name dtd atts in
	if empty 
	then (dtd,Node ((elemName,attNames),nil)::content,stack)
	else (dtd,nil,((elemName,attNames),content)::stack)
    end
	
fun hookEndTag ((dtd,_,nil),_) = raise IllFormed
  | hookEndTag ((dtd,content,(tag,content')::stack),_) =
    (dtd,Node (tag,rev content)::content',stack)

fun hookData ((dtd,content,stack),(_,vec,_)) =
    (dtd,Text (UniChar.Vector2String vec)::content,stack)

fun hookCData ((dtd,content,stack),(_,vec)) =
    (dtd,Text (UniChar.Vector2String vec)::content,stack)

fun hookCharRef ((dtd,content,stack),(_,c,_)) = (* FIX *)
    (dtd,content,stack)

fun hookFinish (dtd,[elem],nil) = elem
  | hookFinish _ = raise IllFormed


fun print_message (pos,msg) = 
    TextIO.output (TextIO.stdErr, ErrorString.Position2String pos^": "^msg)

fun hookError (x as (dtd,content,stack),(pos,ErrorData.ERR_NO_DTD)) = 
    (Dtd.setHasDtd dtd; x)
  |  hookError (x,(pos,err)) = 
     (print_message (pos, "Error: "^
			  (String.concatWith " " (Errors.errorMessage err))^
			  "\n"); 
      x)

fun hookWarning (x,(pos,warn)) = 
    (print_message (pos, "Warning: "^
			 (String.concatWith " " (Errors.warningMessage warn))^
			 "\n"); 
     x) 

end

structure ParseXmlTree : sig
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
	fun su4sml_home () = case OS.Process.getEnv "HOLOCL_HOME" of
			     SOME p => p^"lib/su4sml/src"
			   | NONE => getOpt(OS.Process.getEnv "UML2CDL_HOME",".")
	(* how to do the following in a clean/portable way? *)
	fun read_dtd dtd = 
	    let val _ = OS.FileSys.chDir (su4sml_home())
		(* dummy check to see if the file exists...*)	     
		val _ = OS.FileSys.fileSize "UML15OCL.xmi" 
		val _ = Parser.parseDocument 
			    (SOME (Uri.String2Uri ("file:UML15OCL.xmi")))
			    (SOME dtd) (dtd,nil,nil) 
		val _ = OS.FileSys.chDir currentDir 
	    in ()
	    end
		handle SysErr => (print ("Warning: in readFile: "^ 
					 "did not find file UML15OCL.xmi\n");
				  OS.FileSys.chDir currentDir )
	fun read_file dtd filename = 
	    if filename = "-" then 
		Parser.parseDocument
		    (NONE)
		    (SOME dtd) (dtd,nil,nil)
	    else 
	    let (* dummy check to see if the file exists...*)
		val _ = OS.FileSys.fileSize filename 
	    in 
		Parser.parseDocument
		    (SOME (Uri.String2Uri filename))
		    (SOME dtd) (dtd,nil,nil)
	    end
		handle SysErr => (print ("Warning: in readFile: did not find file "
					 ^filename^"\n"); 
				  Node (("",nil),nil)) 
	val dtd = Dtd.initDtdTables()
    in  ( read_dtd dtd; 
	  read_file dtd filename )
    end
end


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
    (TextIO.output (stream,"<"^escape (tagname_of tree));
     List.app (writeAttribute stream) (attributes_of tree);
     TextIO.output (stream,">\n"))

fun writeStartEndTag stream tree =
    (TextIO.output (stream,"<"^escape (tagname_of tree));
     List.app (writeAttribute stream) (attributes_of tree);
     TextIO.output (stream, " />\n"))

fun writeIndent stream 0 = ()
  | writeIndent stream n = (TextIO.output (stream, "  "); writeIndent stream (n-1))
    

fun writeXmlTree indent stream (Text s) =  
    (writeIndent stream indent;
     TextIO.output (stream, s^"\n"))
  | writeXmlTree indent stream tree = 
    let val elemName = escape (tagname_of tree)
    in 
	writeIndent stream indent;
	if children_of tree = nil 
	then writeStartEndTag stream tree
	else (writeStartTag stream tree;
	      List.app (writeXmlTree (indent+1) stream) (children_of tree);
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
