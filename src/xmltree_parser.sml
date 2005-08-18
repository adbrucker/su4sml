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


structure XmlTreeData =
struct
exception IllFormed

(* Tags consist of element names, and a list of attribute name-value pairs *)
type Tag = string * ((string * string) list)

(*datatype Tree = TEXT of UniChar.Vector
		      | ELEM of Tag * Content
withtype Content = Tree list *)
datatype XmlTree = ELEM of Tag * XmlContent
withtype XmlContent = XmlTree list

fun getAtts  (ELEM ((elem,atts),trees)) = atts
fun getTrees (ELEM ((elem,atts),trees)) = trees
fun getElem  (ELEM ((elem,atts),trees)) = elem

end

structure XmlTreeHooks =
struct
open IgnoreHooks XmlTreeData UniChar HookData

type AppData = Dtd.Dtd * XmlContent * (Tag * XmlContent) list
type AppFinal = XmlTree
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
	then (dtd,ELEM ((elemName,attNames),nil)::content,stack)
	else (dtd,nil,((elemName,attNames),content)::stack)
    end
	
fun hookEndTag ((dtd,_,nil),_) = raise IllFormed
  | hookEndTag ((dtd,content,(tag,content')::stack),_) =
    (dtd,ELEM (tag,rev content)::content',stack)

fun hookData ((dtd,content,stack),(_,vec,_)) =
    (dtd,content,stack)

fun hookCData ((dtd,content,stack),(_,vec)) =
    (dtd,content,stack)

fun hookCharRef ((dtd,content,stack),(_,c,_)) =
    (dtd,content,stack)

fun hookFinish (dtd,[elem],nil) = elem
  | hookFinish _ = raise IllFormed

end

structure ParseXmlTree = (* :
  sig
    val parseTree : Uri.Uri option -> Dtd.Dtd option -> TreeData.Tree
  end = *)
struct
open XmlTreeData 
structure Parser = Parse (structure Dtd = Dtd
			  structure Hooks = XmlTreeHooks
			  structure ParserOptions = ParserOptions ()
			  structure Resolve = ResolveNull)
		   
fun parseXmlTree filename = 
    let val currentDir = OS.FileSys.getDir()
	val _ = OS.FileSys.fileSize filename (* dummy check to see if the file exists...*)
	val dtd = Dtd.initDtdTables()
	(* how to do the following in a clean/portable way? *)
	val _ = OS.FileSys.chDir (su4sml_home())
	val _ = OS.FileSys.fileSize "dummy.xmi" (* dummy check to see if the file exists...*)
	val _ = Parser.parseDocument 
		    (SOME (Uri.String2Uri ("file:dummy.xmi")))
		    (SOME dtd) (dtd,nil,nil) 
	val _ = OS.FileSys.chDir currentDir 
    in Parser.parseDocument
	   (SOME (Uri.String2Uri filename))
	   (SOME dtd) (dtd,nil,nil)
    end

end


structure WriteXmlTree =
struct
open XmlTreeData

fun writeAttribute stream (name,value) =
    TextIO.output (stream, " "^name^"=\""^value^"\"")

fun writeEndTag stream name = TextIO.output (stream,"</"^name^">\n")

fun writeStartTag stream tree = 
    (TextIO.output (stream,"<"^(getElem tree));
     map (writeAttribute stream) (getAtts tree);
     TextIO.output (stream,">\n"))

fun writeIndent stream 0 = ()
  | writeIndent stream n = (TextIO.output (stream, "  "); writeIndent stream (n-1))
    

fun writeXmlTree' indent stream tree = 
    let val elemName = getElem tree 
    in 
	writeIndent stream indent;
	writeStartTag stream tree;
	map (writeXmlTree' (indent+1) stream) (getTrees tree);
	writeIndent stream indent;
	writeEndTag stream elemName
    end

fun writeXmlTree filename tree = 
    let val stream = TextIO.openOut filename 
    in 
	writeXmlTree' 0 stream tree;
	TextIO.closeOut stream
    end

end
