(* ulex-buffer.sml
 *
 * COPYRIGHT (c) 2006
 * John Reppy (http://www.cs.uchicago.edu/~jhr)
 * Aaron Turon (http://www.cs.uchicago.edu/~adrassi)
 * All rights reserved.
 *
 * Forward-chained buffers for lexing
 *)

structure ULexBuffer : sig

  type stream
  val mkStream : (AntlrStreamPos.pos * (unit -> string)) -> stream
  val getc : stream -> (Char.char * stream) option
  val getpos : stream -> AntlrStreamPos.pos
  val subtract : stream * stream -> Substring.substring
  val eof : stream -> bool
  val lastWasNL : stream -> bool

end = struct

  datatype stream = S of (buf * int * bool) 
  and buf = B of { 
    data : string,
    basePos : AntlrStreamPos.pos,
    more : more ref,
    input : unit -> string
  }
  and more = UNKNOWN | YES of buf | NO
        
  fun mkStream (pos, input) = 
        (S (B {data = "", basePos = pos, 
	       more = ref UNKNOWN,
	       input = input},
	    0, true))

  fun getc (S (buf as B {data, basePos, more, input}, pos, lastWasNL)) = 
        if pos < String.size data then let
	    val c = String.sub (data, pos)
	  in
	    SOME (c, S (buf, pos+1, c = #"\n"))
	  end
	else (case !more
	       of NO => NONE
		| YES buf' => getc (S (buf', 0, lastWasNL))
		| UNKNOWN => 
		    (case input()
		      of "" => (more := NO; NONE)
		       | data' => let 
			   val buf' = B {
                               data = data',
			       basePos = AntlrStreamPos.forward (basePos, String.size data),
			       more = ref UNKNOWN,
			       input = input
			     }
			   in
			     more := YES buf';
			     getc (S (buf', 0, lastWasNL))
			   end
		     (* end case *))
              (* end case *))

  fun getpos (S (B {basePos, ...}, pos, _)) = AntlrStreamPos.forward (basePos, pos)

  fun subtract (new, old) = let
        val (S (B {data = ndata, basePos = nbasePos, ...}, npos, _)) = new
	val (S (B {data = odata, basePos = obasePos, 
		   more, input}, opos, _)) = old
        in
          if nbasePos = obasePos then
	    Substring.substring (ndata, opos, npos-opos)
	  else case !more
		of NO =>      raise Fail "BUG: ULexBuffer.subtract, but buffers are unrelated"
		 | UNKNOWN => raise Fail "BUG: ULexBuffer.subtract, but buffers are unrelated"
		 | YES buf => 
		     Substring.extract (
		       Substring.concat [
			 Substring.extract (odata, opos, NONE),
			 subtract (new, S (buf, 0, false))],
		       0, NONE)
        end

  fun eof s = not (isSome (getc s))

  fun lastWasNL (S (_, _, lastWasNL)) = lastWasNL

end