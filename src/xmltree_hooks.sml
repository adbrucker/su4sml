(*****************************************************************************
 *          su4sml - a SecureUML repository for SML              
 *                                                                            
 * xmltree_hooks.sml - hooks for the xml-parser 
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


structure XmlTreeHooks : Hooks =
struct
open IgnoreHooks XmlTree UniChar HookData

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
	
fun hookEndTag ((dtd,_,nil),_) = raise IllFormed "in hookEndTag: illformed XML"
  | hookEndTag ((dtd,content,(tag,content')::stack),_) =
    (dtd,Node (tag,rev content)::content',stack)

fun hookData ((dtd,content,stack),(_,vec,_)) =
    (dtd,Text (UniChar.Vector2String vec)::content,stack)

fun hookCData ((dtd,content,stack),(_,vec)) =
    (dtd,Text (UniChar.Vector2String vec)::content,stack)

fun hookCharRef ((dtd,content,stack),(_,c,_)) = (* FIX *)
    (dtd,content,stack)

fun hookFinish (dtd,[elem],nil) = elem
  | hookFinish _ = raise IllFormed "in hookFinish: illformed XML"


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
