(*****************************************************************************
 *          su4sml GCG - Generic Code Generator               
 *                                                                            
 * gcg_core.sml - implements functor GCG_Core 
 * 		  transcribes a su4sml model according to a template tree 
 *		  into code specific to a target language cartridge C
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

(** A Code generator *)
signature GCG = 
sig

val writeLine : string -> unit
val generate  : Rep.Model -> string -> unit

end

(** builds a code generator from a cartridge chain. *)
functor GCG_Core (C: CARTRIDGE): GCG  = 
struct

val curFile = ref ""


val out = ref TextIO.stdOut

fun closeFile ()= if (!curFile = "") 
				  then ()
				  else (TextIO.closeOut (!out); 
						print ((!curFile)^" ... done\n");
						curFile := "") 
		  

fun openFile file = (closeFile ();
					 print ("opening "^file^"...\n");
					 Gcg_Helper.assureDir file;
					 out := (TextIO.openOut file);
					 curFile := file
					 )
		     
fun initOut () =  (out := TextIO.stdOut;
				   curFile := "")
		  
fun writeLine s = TextIO.output (!out,s)

fun eval s = (print ("<eval>\n");
			  CompilerExt.eval true s;
			  print "<>\n")

(** applies f to every other element in l starting with the second
 *)
fun map2EveryOther f [] = []
  | map2EveryOther f [a] = [a]
  | map2EveryOther f (a::b::z) = a::(f b)::(map2EveryOther f z)  

fun substituteVars e s = let val tkl = (Gcg_Helper.joinEscapeSplitted "$") (Gcg_Helper.fieldSplit s #"$")
			in
			  String.concat (map2EveryOther (C.lookup e) tkl)
			end
			

(** traverses a templateParseTree and executes the given instructions *)
fun write env (Tpl_Parser.RootNode(l))  = List.app (write env) l
  | write env (Tpl_Parser.OpenFileLeaf(file)) = openFile (substituteVars env file)
  | write env (Tpl_Parser.EvalLeaf(l)) = let fun collectEval 		[] 	    = ""
  				| collectEval ((Tpl_Parser.TextLeaf(expr))::t) = expr^"\n"^(collectEval t)
  				| collectEval 		_	    = 
  				     Gcg_Helper.gcg_error "eval failed: TextLeaf expected in gcg_core.write." 
  			  in
  			    eval (substituteVars env (collectEval l))
  			  end
  | write env (Tpl_Parser.TextLeaf(s)) =  writeLine (substituteVars env s)
  | write env (Tpl_Parser.IfNode(cond,l)) 
  	=   let (*val list_of_environments = C.foreach listType env
		fun write_children e = List.app (fn tree => write e tree) children
		*)
		fun writeThen _ []           = ()
  		 |  writeThen _ [Tpl_Parser.ElseNode(_)]= ()
  		 |  writeThen e (h::t)	    = (write e h ;writeThen e t) 
  	    in
  	    	if (C.test env cond) 
  		then writeThen env l
  		else (case (List.last l) of nd as (Tpl_Parser.ElseNode(_)) => write env nd
  			  		    |          _        => ()	)
  	    end
  | write env (Tpl_Parser.ElseNode(l)) = List.app (write env) l
  | write env (Tpl_Parser.ForEachNode(listType,children)) 
  	=	let val list_of_environments = C.foreach listType env
		    fun write_children e = List.app (fn tree => write e tree) children
		in 
		    List.app (fn e => write_children e) list_of_environments
     		end
     				

(** generate code according to the given template file for the given model *)
fun generate model template 
  = let 
      val env = C.initEnv  model 
	  val tree = Tpl_Parser.parse template
	in
		(initOut();
		 (*printTTree tree;*)
		 write env tree;
		 closeFile () ) 
		handle GCG_Error => (closeFile(); raise GCG_Error)
	end
		
		
end
