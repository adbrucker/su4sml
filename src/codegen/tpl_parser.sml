(*****************************************************************************
 *          su4sml GCG - Generic Code Generator               
 *                                                                            
 * tpl_parser.sml - template parser of a su4sml-gcg template
 * Copyright (C) 2005 Raphael Eidenbenz <eraphael@student.ethz.ch>
 *                                                                            
 * This file is part of su4sml-gcg.                                              
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

(** A parser for template files. *)
signature TPL_PARSER = 
sig

datatype TemplateTree
  = ElseNode of TemplateTree list
  | EvalLeaf of TemplateTree list
  | ForEachNode of string * TemplateTree list
  | IfNode of string * TemplateTree list
  | OpenFileLeaf of string
  | RootNode of TemplateTree list
  | TextLeaf of string

val printTTree  	: TemplateTree -> unit
val parse 	  	: string -> TemplateTree

end


structure Tpl_Parser :  TPL_PARSER = 
struct
open Gcg_Helper

val tplStream = ref (TextIO.openString "@// dummy template\n");

fun opentFile file = (TextIO.closeIn (!tplStream) ; 
		      print ("opening "^file^"...\n");
		      tplStream := (TextIO.openIn file))

fun cleanUp tplFile = (TextIO.closeIn (!tplStream);
			  OS.FileSys.remove tplFile)

fun readNextLine () = TextIO.inputLine (!tplStream)
 
datatype TemplateTree =   RootNode of TemplateTree list
			| OpenFileLeaf of string
			| EvalLeaf of TemplateTree list
			| TextLeaf of string
			| IfNode of string * TemplateTree list
			| ElseNode of TemplateTree list
			| ForEachNode of string * TemplateTree list
			
			


(** 
 *  replaceSafely (s,v,x) replaces every v in s with x or if v is escaped removes "\"  
 *)
fun replaceSafely ("",_,_) = ""
  | replaceSafely (s,v,x) = let val v_size = size v and 
		  	    s_size = size s
		      in
			if String.isPrefix ((str #"\\")^v) s 
				then (v^(replaceSafely(String.extract(s,v_size +1,NONE),v,x)))
		    	else if String.isPrefix v s 
		    		then x^(replaceSafely(String.extract(s,v_size,NONE),v,x))
		    	else str(String.sub(s,0))^(replaceSafely(String.extract(s,1,NONE),v,x))
		      end
		      

(** 
 * splits string into tokens and
 * removes space- and tab-characters  
 *)
fun cleanLine s = let fun removeWspace s = 
			String.implode (List.filter (fn c => not (Char.isSpace c)) (String.explode s)) 
		   fun concatWith [] d = ""
			   | concatWith [s] d = s^" "
  			   | concatWith (h::t) d = h^d^(concatWith t d)
  	           val myToken = (String.tokens (fn c => c = #" "))
			in
			  concatWith ( List.filter (fn s => s <>"")(((List.map removeWspace) o myToken) s )) " "
			end

(* debugging function
 * prints ParseTree to stdOut
 *)
fun printTplTree prefix (RootNode(l))	= (print (prefix^"root"^"\n"); List.app (printTplTree (prefix))l)
  | printTplTree prefix (OpenFileLeaf(s))= print (prefix^"openfile:"^s^"\n") 
  | printTplTree prefix (EvalLeaf(l)) 	= (print (prefix^"eval:\n"); List.app (printTplTree (prefix^"\t"))l)
  | printTplTree prefix (TextLeaf(s))	= print (prefix^"text:"^s^"\n")
  | printTplTree prefix (IfNode(s,l))	= (print (prefix^"if:"^s^"\n");List.app (printTplTree (prefix^"\t")) l)
  | printTplTree prefix (ElseNode(l))	= (print (prefix^"else:"^"\n"); List.app (printTplTree (prefix^"\t")) l)
  | printTplTree prefix (ForEachNode(s,l))=(print (prefix^"foreach:"^s^"\n");List.app (printTplTree (prefix^"\t")) l) 

val printTTree = printTplTree ""

fun isComment s = (String.isPrefix "//" s)

(** returns the left part of l up to the element where f evaluates to true 
 *)
fun itemsUntil f [] = []
  | itemsUntil f (h::t) = if (f h) then []
  			  else h::(itemsUntil f t)
		   
  				   
(** splits line into tokens considering handling escaped @ *)
fun tokenize line = let val l =   joinEscapeSplitted "@" (fieldSplit line #"@");
		    in
		      (hd l)::(itemsUntil isComment (tl l))
		    end

(** 
 * extracts the type of line.
 * line type must be first token in line!
 * if no control tag in line -> "text" returned
 *)
fun getType l = let val sl = tokenize l
		in
		  if (length sl  = 1)
		  then "text"
		  else hd(fieldSplit (String.concat(tl(sl))) #" ")
		end


(** 
 * getContent line 
 * @return the content of a line
 *)		
fun getContent l = let val sl = tokenize l 
		   in 
		     if (length sl = 1)
		     then hd(sl)
		     else String.concat(tl(fieldSplit (String.concat(tl(sl))) #" "))
		   end

(**
 * cleans line, replaces nl and tabs
 * so that no space char is left out
 *)			  
		   
fun preprocess s = let val rl = replaceSafely(replaceSafely(cleanLine s,"@nl ","\n"),"@tab ","\t")
		   in
		     replaceSafely(replaceSafely(rl,"@nl","\n"),"@tab","\t")
		   end


(**
 * builds the TemplateTree.
 * @return a TemplateTree list
 *)
fun buildTree (SOME line) = let fun getNode ("text",c) 	  = (TextLeaf(c))::(buildTree (readNextLine()))
			 	  | getNode ("foreach",c) = ForEachNode(c,(buildTree (readNextLine())))::(buildTree (readNextLine()))
			 	  | getNode ("if",c) 	  = IfNode(c,buildTree (readNextLine()))::(buildTree (readNextLine()))
			 	  | getNode ("else",_) 	  = [ElseNode(buildTree (readNextLine()))]
			 	  | getNode ("elsif",c)	  = [ElseNode([IfNode(c,buildTree (readNextLine()))])]
			 	  | getNode ("openfile",c)= (OpenFileLeaf(c))::(buildTree (readNextLine()))
			 	  | getNode ("eval","")   = 
			 	  	(EvalLeaf(buildTree(readNextLine())))::(buildTree (readNextLine()))
			 	  | getNode ("eval",expr) = (EvalLeaf([TextLeaf(expr)]))::(buildTree (readNextLine()))
			 	  | getNode ("end",_) 	  = []
			 	  | getNode (t,c) 	  = gcg_error ("Couldn't parse the node \""^t^"\" with content\""^c^"\" in tpl_parser.buildTree.")
			 	val prLine = preprocess line  
		     	  in
		     	    getNode ((getType prLine),(getContent prLine))
		     	  end
  | buildTree NONE  = []


fun codegen_home _ = getOpt(OS.Process.getEnv "CODEGEN_HOME",
                            library.su4sml_home()^"src/codegen")
			
(** calls the external cpp ( C PreProcessor).
 * writes merged template to a file with extension .tmp instead of .tpl
 * and returns this file 
 *)
fun call_cpp file = 
	let (*val targetFile = String.substring (file,0,size file -4) ^".tmp";*)
		val targetFile = OS.FileSys.tmpName () 
		val _ = OS.Process.system ("cpp -P -C "^codegen_home()^"/"^file^" "^targetFile)
	in
		targetFile
	end


		    
(**  parse template-file
 *  @return the parsed template tree		     
 *)
fun parse file = let val mergedTpl = call_cpp file;
		     val u = opentFile mergedTpl;
 		     val pt = RootNode(buildTree (readNextLine()));
		     val u2 = cleanUp mergedTpl; 
		in 
		  (print "...template parsed.\n"; pt) 
		end

(*			
val testline = "@foreach \\@public @// commkejbk";
val textline1 = "\t\tpublic $class_name$ ";
val textline2 = "";
val endline = "\t@end";
*)
(*
val ParseTree = parse "examples/C#.tpl";

printTplTree ParseTree;
*)
end
