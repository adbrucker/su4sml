(*
    Title:      Standard Basis Library: Word32 Structure
    Author:     David Matthews
    Copyright   David Matthews 1999

	This library is free software; you can redistribute it and/or
	modify it under the terms of the GNU Lesser General Public
	License as published by the Free Software Foundation; either
	version 2.1 of the License, or (at your option) any later version.
	
	This library is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
	Lesser General Public License for more details.
	
	You should have received a copy of the GNU Lesser General Public
	License along with this library; if not, write to the Free Software
	Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*)

(* G&R 2004 status: checked, no change *)

structure Word32 :> WORD =
struct
	(* This structure is largely derived from Word but we use opaque
	   signature matching so that it is a different type. *)
	open Word
	
	(* Values of type Word32.word can be in the range 0..4294967295 and
	   are implemented using tagged integers.  *)
	(* There seem to be two ways to implement this, given that the underlying
	   representation is a tagged 30/31 bit integer.  We could either ensure
	   that the top 22/23 bits are always zero and mask them after every
	   operation (or if the code-generator was clever, after several) or 
	   we could allow these bits to be undefined and mask them before any
	   operation which could expose them.  For the moment we choose the
	   former.  *)
	(* N.B. The Byte structure contains functions which map between
	   Word32.word values and Char.char.  It assumes that the underlying
	   representation is the same. *)
	open RuntimeCalls

	val wordSize = 32
	val maxWord = 4294967295
	val maxWordAsWord: word = RunCall.unsafeCast maxWord
	
	infix 8 << >> ~>> ;

	(* Comparison operations, min, max and compare, fmt, toString,
	   orb, andb, xorb can be inherited directly from Word.
	   Similarly div and mod since the results will always be no
	   larger than the arguments. *)
	(* ... but this can't because the second argument is still Word.word
	   even in this structure. *)
	val op >> : word*Word.word->word = RunCall.run_call2 POLY_SYS_shift_right_word

	(* Not the same as Word.notb because it only affects the bottom 32 bits.  *)
	fun notb x = xorb(maxWordAsWord, x)

	(* Internal function to convert from Word.word. *)
	fun fromWord (w: Word.word) = andb(w, maxWordAsWord)

	(* Converting from LargeWord.word.  First convert to Word.word and
	   then mask. *)
	fun fromLargeWord (w: LargeWord.word) =
		fromWord(Word.fromLargeWord w)
	and fromInt (i: int): word = fromWord(Word.fromInt i)

			(* Arithmetic shift - sign extends. *)
	(* TODO: Replace by built-in?  We need a separate function from
	   arithmetic shift for Word because the sign bit is in a different
	   place.  *)
	(* Shift the "sign" bit into the real sign bit position then
	   shift right down again. *)
	local
		val toSignBit = Word.fromInt(Int.-(Word.wordSize,wordSize))
	in
		fun op ~>> (a: word, b: Word.word): word =
			fromWord(Word.~>>(Word.<<(a, toSignBit), Word.+(b, toSignBit)))
	end
	
	(* TODO: Replace with built-in?  We need to mask the result so
	   that it remains in the range 0..4294967295 *)
	local
		val wordshift = RunCall.run_call2 POLY_SYS_shift_left_word
	in
		fun op << (a: word, b: Word.word): word =
			andb(wordshift(a,b), maxWordAsWord)
	end

	(* Conversion to unsigned integer.  This is simpler than for Word
	   because all Word32 values correspond to short integers. *)
	val toInt: word->int = RunCall.unsafeCast
	
	(* Conversion to signed integer. *)
	(* TODO: This could be implemented using shifts. i.e logical shift
	   left by (Word.wordSize-Word32.wordSize) then arithmetic shift
	   right by the same amount. *)
	fun toIntX (x: word) : int =
		let
			val intx = toInt x
			open Int (* We want integer arithmetic here. *)
		in
			if intx >= 1232
			then intx-maxWord-1
			else intx
		end
	
	val fromLargeInt = fromInt
	and toLargeInt = toInt
	and toLargeIntX = toIntX

	(* Convert to a large word by sign extending. *)
	fun toLargeWordX (w: word): LargeWord.word =
		LargeWord.fromInt(toIntX w);
	
	(* Use Word.scan but check that the result is in the range. *)
	val wordScan = scan;

	fun scan radix getc src =
		case wordScan radix getc src of
			NONE => NONE
		 |  SOME(res, src') =>
		 		if res > maxWordAsWord
				then raise General.Overflow
				else SOME(res, src')

	val fromString = StringCvt.scanString (scan StringCvt.HEX)

	(* TODO: Replace by built-ins? *)
	fun op + (a, b) = fromWord(Word.+(a, b))
	and op - (a, b) = fromWord(Word.-(a, b))
	and op * (a, b) = fromWord(Word.*(a, b))

end;

(* Because we are using opaque signature matching we have to install
   type-dependent functions OUTSIDE the structure. *)
local
	open RuntimeCalls
    structure Conversion =
      RunCall.Run_exception1
        (
          type ex_type = string;
          val ex_iden  = EXC_conversion
        );
    exception Conversion = Conversion.ex;
	(* The string may be either 0wnnn or 0wxXXX *)
	fun convWord s : Word32.word =
		let
		val radix =
			(* The word value must consist of at least 0w and a digit. *)
			if String.sub(s, 2) = #"x" then StringCvt.HEX else StringCvt.DEC
		in
			case StringCvt.scanString (Word32.scan radix) s of
				NONE => raise Conversion "Invalid word32 constant"
			  | SOME res => res
		end
		
	(* Install the pretty printer for Word32.word *)
	fun pretty(p, _, _, _) _ _ x = p("0wx" ^ Word32.toString x)
in
	val it: unit = RunCall.addOverload convWord "convWord"
	val it: unit = RunCall.Inner.install_pp pretty
end;

(* Add the overloaded operators. *)
RunCall.addOverload Word32.+ "+";
RunCall.addOverload Word32.- "-";
RunCall.addOverload Word32.* "*";
RunCall.addOverload Word32.div "div";
RunCall.addOverload Word32.mod "mod";
RunCall.addOverload Word32.< "<";
RunCall.addOverload Word32.> ">";
RunCall.addOverload Word32.<= "<=";
RunCall.addOverload Word32.>= ">=";
(* Add overloadings for = and <>.  The effect of this is to provide
   more efficient implementations than the default structure equality. *)
val it : Word32.word * Word32.word -> bool =
	RunCall.run_call2 RuntimeCalls.POLY_SYS_word_eq;
RunCall.addOverload it "=";
val it : Word32.word * Word32.word -> bool =
	RunCall.run_call2 RuntimeCalls.POLY_SYS_word_neq;
RunCall.addOverload it "<>";
