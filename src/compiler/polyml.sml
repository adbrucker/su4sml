(*****************************************************************************
 *          su4sml - a SecureUML repository for SML              
 *                                                                            
 * polyml.sml - interactive eval 
 * Copyright (C) 2005 Achim D. Brucker <brucker@inf.ethz.ch>
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

structure CompilerExt : COMPILER_EXT = 
struct

exception EvalNotSupported
fun drop_last [] = []
  | drop_last [x] = []
  | drop_last (x :: xs) = x :: drop_last xs;

fun eval verbose txt =
    let 
	fun eval_fh (print, err) verbose txt =
	    let
		val in_buffer = ref (SML90.explode txt);
		val out_buffer = ref ([]: string list);
		fun output () = SML90.implode (drop_last (rev (! out_buffer)));
		    
		fun get () =
		    (case ! in_buffer of
			 [] => ""
		       | c :: cs => (in_buffer := cs; c));
		fun put s = out_buffer := s :: ! out_buffer;
		    
		fun exec () =
		    (case ! in_buffer of
			 [] => ()
		       | _ => (PolyML.compiler (get, put) (); exec ()));
	    in
		exec () handle exn => (err (output ()); raise exn);
		if verbose then print (output ()) else ()
	    end
    in	
	eval_fh (fn s => print (s^"\n"), fn s => library.error (s^"\n")) verbose txt
    end

fun exnHistory _ = []

end
