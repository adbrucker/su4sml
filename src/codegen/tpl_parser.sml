(*****************************************************************************
 * su4sml --- an SML repository for managing (Secure)UML/OCL models
 *             http://projects.brucker.ch/su4sml/
 *                                                                            
 * tpl_parser.sml --- template parser of a su4sml-gcg template
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

(** A parser for template files. *)
signature TPL_PARSER = 
sig

    datatype TemplateTree
      = ElseNode of TemplateTree list
      | EvalLeaf of TemplateTree list
      | ForEachNode of string * TemplateTree list
      | IfNode of string * TemplateTree list
      | OpenFileLeaf of string
      | OpenFileIfNotExistsLeaf of string
      | RootNode of TemplateTree list
      | TextLeaf of string

    val printTTree          : TemplateTree -> unit
    val parse               : string -> TemplateTree

end


structure Tpl_Parser :  TPL_PARSER = 
struct
open Gcg_Helper

val tplStream = ref (TextIO.openString "@// dummy template\n");

fun opentFile file = (TextIO.closeIn (!tplStream) ; 
                      tplStream := (TextIO.openIn file))
    handle ex => Logger.error ("in Tpl_Parser.opentFile: \
                        \couldn't open preprocessed template file: "^
                        General.exnMessage ex)
                 
fun cleanUp tplFile = (TextIO.closeIn (!tplStream);
                       OS.FileSys.remove tplFile)
                      
fun readNextLine () = TextIO.inputLine (!tplStream)
                      

(* FIXME: this currently uses a simple line-based template-file structure *)
(*        (every line corresponds to exactly one node in this tree)       *)
(*        This should really be relaxed...                                *)
(* FIXME: add separate VariableLeaf                                       *)
(* FIXME: merge If and Else Nodes                                         *)
(* FIXME: add InfoLeaf to print informational messages during codegen     *)
datatype TemplateTree =   RootNode                of TemplateTree list
                        | OpenFileLeaf            of string
                        | OpenFileIfNotExistsLeaf of string
                        | EvalLeaf                of TemplateTree list
                        | TextLeaf                of string
                        | IfNode                  of string * TemplateTree list
                        (* FIXME: why a seperate ElseNode? should be part of IfNode *)
                        | ElseNode                of TemplateTree list
                        | ForEachNode             of string * TemplateTree list
                                                     


(** 
 *  replaceSafely (s,v,x) replaces every v that occurs unescaped in s with x.
 * if v occurs escaped with "\" in s, then the "\" is removed from s.
 * FIXME: move to stringhandling?
 *)
fun replaceSafely _ _ "" = ""
  | replaceSafely v x s  = 
    let val v_size = size v  
        val s_size = size s
    in
        if String.isPrefix (str #"\\"^v) s 
        then v^replaceSafely v x (String.extract (s, v_size + 1, NONE))
        else if String.isPrefix v s 
        then x^replaceSafely v x (String.extract (s, v_size, NONE))
        else str (String.sub (s,0))^replaceSafely v x (String.extract (s, 1, NONE))
    end
                            

(** removes leading, trainling, and multiple consecutive whitespace chars. *)
(* FIXME: movev to StringHandling? *)
fun cleanLine s = String.concatWith " " (String.tokens Char.isSpace s) 

                  
(* debugging function
 * prints ParseTree to stdOut
 *)
fun printTplTree prefix (RootNode(l))   = (print (prefix^"root"^"\n"); List.app (printTplTree (prefix))l)
  | printTplTree prefix (OpenFileLeaf(s))= print (prefix^"openfile:"^s^"\n") 
  | printTplTree prefix (OpenFileIfNotExistsLeaf(s))= print (prefix^"openfileifnotexists:"^s^"\n") 
  | printTplTree prefix (EvalLeaf(l))   = (print (prefix^"eval:\n"); List.app (printTplTree (prefix^"\t"))l)
  | printTplTree prefix (TextLeaf(s))   = print (prefix^"text:"^s^"\n")
  | printTplTree prefix (IfNode(s,l))   = (print (prefix^"if:"^s^"\n");List.app (printTplTree (prefix^"\t")) l)
  | printTplTree prefix (ElseNode(l))   = (print (prefix^"else:"^"\n"); List.app (printTplTree (prefix^"\t")) l)
  | printTplTree prefix (ForEachNode(s,l))=(print (prefix^"foreach:"^s^"\n");List.app (printTplTree (prefix^"\t")) l) 

val printTTree = printTplTree ""

fun isComment s = (String.isPrefix "//" s)

(** returns the prefix of l up to the first element where f evaluates to true *)
fun takeUntil f [] = []
  | takeUntil f (h::t) = if f h then [] else h::(takeUntil f t)
                               
                              
(** splits line into tokens considering handling escaped @ *)
fun tokenize line = let val l = joinEscapeSplitted "@" (fieldSplit #"@" line)
                    in
                        takeUntil isComment l
                    end

(** 
 * extracts the type of line.
 * line type must be first token in line!
 * if no control tag in line -> "text" returned
 *)
fun getType l = let val sl = tokenize l
                in
                    if (length sl = 1) orelse (length sl = 0)
                    then "text" (* rather: comment? *)
                    else hd (tokenSplit #" " (String.concat sl))
                end
                handle ex => Logger.error ("in Tpl_Parser.getType: "^General.exnMessage ex)

(** 
 * getContent line 
 * @return the content of a line
 *)             
fun getContent l = let val sl = tokenize l 
                   in 
                       if (length sl = 0) then ""
                       else if  (length sl = 1) then hd sl
                       else String.concat (tl (fieldSplit #" " (String.concat (tl sl))))
                   end
    handle ex => Logger.error ("in Tpl_Parser.getContent: "^General.exnMessage ex)

(** cleans line, replaces nl and tabs so that no space char is left out. *)
fun preprocess s = replaceSafely "@spc" " " (replaceSafely "@tab" "\t" (replaceSafely "@nl" "\n" (cleanLine s)))


(**
 * builds the TemplateTree.
 * @return a TemplateTree list
 *)
fun buildTree (SOME line) = 
    (let fun getNode ("text", c)     = TextLeaf c :: buildTree (readNextLine())
          | getNode ("foreach", c)  = ForEachNode (c, buildTree (readNextLine()))
                                      :: buildTree (readNextLine())
          | getNode ("if", c)       = IfNode (c, buildTree (readNextLine()))
                                      :: buildTree (readNextLine())
          | getNode ("else", _)     = [ ElseNode (buildTree (readNextLine())) ]
          | getNode ("elsif", c)    = [ ElseNode [ IfNode (c, buildTree (readNextLine())) ]]
          | getNode ("openfile", c) = OpenFileLeaf c :: buildTree (readNextLine())
          | getNode ("openfileifnotexists", c) = OpenFileIfNotExistsLeaf c 
                                                 :: buildTree (readNextLine())
          | getNode ("eval", "")   = EvalLeaf (buildTree (readNextLine()))
                                     :: buildTree (readNextLine())
          | getNode ("eval", expr) = EvalLeaf [ TextLeaf expr ]:: buildTree (readNextLine())
          | getNode ("end",_)      = []
          | getNode (t,c)          = Logger.error ("in Tpl_Parser.buildTree: error while parsing \
                                            \node \""^t^"\" with content \""^c^"\".")
        val prLine = preprocess line  
    in
        getNode ((getType prLine),(getContent prLine))
     end
     handle ex => Logger.error ("in Tpl_Parser.buildTree: error "^General.exnMessage ex))
  | buildTree NONE  = []
    

(** calls the external cpp ( C PreProcessor).
 * writes merged template to a file with extension .tmp instead of .tpl
 * and returns this file 
 *)
fun call_cpp file = 
    let val targetFile = OS.FileSys.tmpName () 
        val _ = OS.Process.system ("cpp -P -C "^(Config.su4sml_share())^"/"^file^" "^targetFile)
    in
        targetFile
    end


    
(**  parse template-file
 *  @return the parsed template tree                 
 *)
fun parse file = let val _         = Logger.info ("parsing  template "^file)
                     val mergedTpl = call_cpp file;
                     val _         = opentFile mergedTpl;
                     val pt        = RootNode(buildTree (readNextLine()));
                     val _         = cleanUp mergedTpl; 
                 in 
                     pt
                 end
end
