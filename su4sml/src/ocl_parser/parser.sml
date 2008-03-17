(*****************************************************************************
 * su4sml --- a SML repository for managing (Secure)UML/OCL models
 *             http://projects.brucker.ch/su4sml/
 *                                                                            
 * parser.sml --- 
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





structure OclParser : sig
    
	           val parse : string -> unit
		   val parse_contextlist: string -> Context.context list
end = 
struct
 open library 
 open Context
 
  structure OclParserLrVals =
    OclParserLrValsFun(structure Token = LrParser.Token)

  structure OclParserLex =
    OclParserLexFun(structure Tokens = OclParserLrVals.Tokens)

  structure OclParserParser =
    Join(structure LrParser = LrParser
	 structure ParserData = OclParserLrVals.ParserData
	 structure Lex = OclParserLex)

(* 
 * We need a function which given a lexer invokes the parser. The
 * function invoke does this.
 *)
  
  fun invoke lexstream =
      let fun print_error (s,i:(int * int * int),_) =
	      TextIO.output(TextIO.stdOut,
			    "Error, line .... " ^ (Int.toString (#1 i)) ^"."^(Int.toString (#2 i ))^ ", " ^ s ^ "\n")
       in OclParserParser.parse(0,lexstream,print_error,())
      end

(* 
 * Finally, we need a driver function that reads one or more expressions
 * from the standard input. The function parse, shown below, does
 * this. It runs the calculator on the standard input and terminates when
 * an end-of-file is encountered.
 *)

  fun parse oclFile = 
      let 
	  val infile = TextIO.openIn oclFile
	  val lexer = OclParserParser.makeLexer  (fn _ => case ((TextIO.inputLine) infile) of
                                                      SOME s => s
                                                    | NONE   => "")
	  val ocl = ""
	  val dummyEOF = OclParserLrVals.Tokens.EOF((0,0,0),(0,0,0))
	  fun loop lexer =
	      let 
		  val _ = (OclParserLex.UserDeclarations.pos := (0,0,0);())
		  val (res,lexer) = invoke lexer
		  val (nextToken,lexer) = OclParserParser.Stream.get lexer
                  val _ = TextIO.output(TextIO.stdOut,(cxt_list2string res) ^ "\n")
	       in if OclParserParser.sameToken(nextToken,dummyEOF) then ()
		  else loop lexer
	      end
       in loop lexer
      end

 fun parse_contextlist oclFile = 
      let 
	  val infile = TextIO.openIn oclFile
	  val lexer = OclParserParser.makeLexer  (fn _ => case ((TextIO.inputLine) infile) of
                                                      SOME s => s
                                                    | NONE   => "")
	  val ocl = ""
	  val dummyEOF = OclParserLrVals.Tokens.EOF((0,0,0),(0,0,0))
	  fun loop lexer =
	      let 
		  val _ = (OclParserLex.UserDeclarations.pos := (0,0,0);())
		  val (res,lexer) = invoke lexer
		  val (nextToken,lexer) = OclParserParser.Stream.get lexer
(*                  
val _ = TextIO.output(TextIO.stdOut,(cxt_list2string res) ^ "\n")
*)
	       in if OclParserParser.sameToken(nextToken,dummyEOF) then ((),res)
		  else loop lexer
	      end
       in (#2(loop lexer))
      end
end 
