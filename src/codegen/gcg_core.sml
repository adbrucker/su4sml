(*****************************************************************************
 * su4sml --- an SML repository for managing (Secure)UML/OCL models
 *             http://projects.brucker.ch/su4sml/
 *                                                                            
 * gcg_core.sml --- implements functor GCG_Core  
 *                  transcribes a su4sml model according to a template tree 
 *		    into code specific to a target language cartridge 
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

fun closeFile () = if (!curFile = "") 
		   then ()
		   else (TextIO.closeOut (!out); 
			 (* info ("closing "^(!curFile));*)
			 curFile := "") 
		        
(* FIXME: set out to a real NullStream *)
fun openNull file = (closeFile ();
		     Logger.info ("skipping "^file);
		     out := (TextIO.openOut "/dev/null");
		     curFile := "/dev/null"
		    )
     

fun openFile file = (closeFile ();
		     Logger.info ("opening  "^file);
		     Gcg_Helper.assureDir file;
		     out := (TextIO.openOut file);
		     curFile := file
		    )
                    
fun openFileIfNotExists file = (closeFile ();
				(if ((OS.FileSys.fileSize file) > 0) 
                                 then openNull file
				 else openFile file
				) handle SysErr => ( openFile file ))
			       
fun initOut () =  (out := TextIO.stdOut;
		   curFile := "")
		  
fun writeLine s = TextIO.output (!out,s)
                  
fun eval s = (Logger.info "<eval>"; CompilerExt.eval true s)

(** applies f to every other element in l starting with the second
 *)
fun map2EveryOther f [] = []
  | map2EveryOther f [a] = [a]
  | map2EveryOther f (a::b::z) = a::(f b)::(map2EveryOther f z)  

fun substituteVars e s = 
    let val tkl = Gcg_Helper.joinEscapeSplitted "$" (Gcg_Helper.fieldSplit #"$" s)
    in
	String.concat (map2EveryOther (C.lookup e) tkl)
        handle ex => (Logger.error ("in GCG_Core.substituteVars: \
                                 \variable lookup failure in string \""^s^"\".");
                      s)
    end

(** traverses a templateParseTree and executes the given instructions *)
fun write env (Tpl_Parser.RootNode(l))                   = List.app (write env) l
  | write env (Tpl_Parser.OpenFileLeaf(file))            = openFile (substituteVars env file)
  | write env (Tpl_Parser.OpenFileIfNotExistsLeaf(file)) = 
    openFileIfNotExists (substituteVars env file)
  | write env (Tpl_Parser.EvalLeaf(l))                   = 
    let fun collectEval [] 	                         = ""
  	  | collectEval ((Tpl_Parser.TextLeaf(expr))::t) = expr^"\n"^(collectEval t)
  	  | collectEval _	                         = 
  	    Logger.error "in GCG_Core.write: No TextLeaf in EvalLeaf" 
    in
  	eval (substituteVars env (collectEval l))
    end
  | write env (Tpl_Parser.TextLeaf(s))                   = writeLine (substituteVars env s)
  | write env (Tpl_Parser.IfNode(cond,l))                =
    let	fun writeThen _ []                               = ()
  	  | writeThen _ [Tpl_Parser.ElseNode(_)]         = ()
  	  | writeThen e (h::t)	                         = (write e h ;writeThen e t) 
    in
  	(if (C.test env cond) 
  	 then writeThen env l
  	 else case (List.last l) of nd as (Tpl_Parser.ElseNode(_)) => write env nd
  			  	  | _                              => ())
        handle ex => Logger.error ("in GCG_Core.write: problem in IfNode "^cond)
    end
  | write env (Tpl_Parser.ElseNode(l))                   = List.app (write env) l
  | write env (Tpl_Parser.ForEachNode(listType,children))=
    let val list_of_environments = C.foreach listType env
	fun write_children e     = List.app (fn tree => write e tree) children
    in 
	List.app (fn e => write_children e) list_of_environments
        handle ex => (Logger.error ("in GCG_Core.write: error in foreach node "^listType^
                                ": "^General.exnMessage ex);
                      ())
    end
    

(** generate code according to the given template file for the given model *)
fun generate model template 
  = let val env = C.initEnv  model 
	val tree = Tpl_Parser.parse template
    in
	(initOut();
	 (*printTTree tree;*)
	 write env tree;
	 closeFile ();
         Logger.info "codegen  finished successfully"
        ) 
	handle ex => (closeFile(); raise ex)
    end
	
end
