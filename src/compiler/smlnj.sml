(*****************************************************************************
 *          su4sml - a SecureUML repository for SML              
 *                                                                            
 * smlnj.sml - interactive eval stub (not supported by MLton)
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

fun eval verbose txt =
    let 
	fun eval_fh (print, err) verbose txt =
	    let
		val ref out_orig = Control.Print.out;
		    
		val out_buffer = ref ([]: string list);
		val out = {say = (fn s => out_buffer := s :: ! out_buffer), 
			   flush = (fn () => ())};
		fun output () =
		    let val str = SML90.implode (rev (! out_buffer))
	    in String.substring (str, 0, Int.max (0, size str - 1)) end;
	    in
		Control.Print.out := out;
		Backend.Interact.useStream (TextIO.openString txt) 
		handle exn =>
		       (Control.Print.out := out_orig; err (output ()); raise exn);
		Control.Print.out := out_orig;
		if verbose then print (output ()) else ()
	    end
    in	
	eval_fh (fn s => print (s^"\n"), fn s => library.error s) verbose txt
    end

fun exnHistory e = SMLofNJ.exnHistory e
 
end
