(*****************************************************************************
 * su4sml --- a SML repository for managing (Secure)UML/OCL models
 *             http://projects.brucker.ch/su4sml/
 *                                                                            
 * xmltree_hooks.sml --- hooks for the xml-parser 
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

structure XmlTreeHooks : Hooks =
struct
open IgnoreHooks XmlTree UniChar HookData library

type AppData = Dtd.Dtd * Tree list * (Tag * Tree list) list
type AppFinal = Tree
(* val appStart = (nil,nil) *)
		
fun attspec2name dtd nil = nil 
  | attspec2name dtd ((i,AP_PRESENT (s,v,_),_)::atts) =
    let val attName = UniChar.Data2String (Dtd.Index2AttNot dtd i) 
	val attValue = UniChar.Data2String (UniChar.Vector2Data  v)
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
	
fun hookEndTag ((dtd,_,nil),_) = error "in hookEndTag: illformed XML"
  | hookEndTag ((dtd,content,(tag,content')::stack),_) =
    (dtd,Node (tag,rev content)::content',stack)

fun hookData ((dtd,content,stack),(_,vec,_)) =
    (dtd,Text (UniChar.Vector2String vec)::content,stack)

fun hookCData ((dtd,content,stack),(_,vec)) =
    (dtd,Text (UniChar.Vector2String vec)::content,stack)

fun hookCharRef ((dtd,content,stack),(_,c,_)) = (* FIX *)
    (dtd,content,stack)

fun hookFinish (dtd,[elem],nil) = elem
  | hookFinish _ = error "in hookFinish: illformed XML"


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
