



structure OclParser : sig
    
	           val parse : string -> unit
		   val parse_contextlist: string -> Context.context list
end = 
struct
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
  
(* Error logging *)
val high = 5
val medium = 20
val low = 100


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
