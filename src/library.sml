(*****************************************************************************
 *          su4sml - a SecureUML repository for SML              
 *                                                                            
 * library.sml - main "ROOT.ML" file for su4sml
 * Copyright (C) 2005 Achim D. Brucker <brucker@inf.ethz.ch>   
 *                    Jürgen Doser <doserj@inf.ethz.ch>
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

structure library =
struct
infix |>
fun (x |> f) = f x;


(* HOLOCL_HOME resp. SU4SML_HOME should point to the top-level directory *)
(* of the corresponding library.  The semantics of UML2CDL_HOME should   *)
(* probably be fixed                                                     *)
fun su4sml_home () = case OS.Process.getEnv "HOLOCL_HOME" of
			 SOME p => p^"/lib/su4sml/src"
		       | NONE   => (case OS.Process.getEnv "SU4SML_HOME" of
				        SOME p => p^"/src"
				      | NONE => getOpt(OS.Process.getEnv "UML2CDL_HOME",".")
                                   )


fun filter (pred: 'a->bool) : 'a list -> 'a list =
    let fun filt [] = []
          | filt (x :: xs) = if pred x then x :: filt xs else filt xs
	in filt end;
    
    
fun exists (pred: 'a -> bool) : 'a list -> bool =
	let fun boolf [] = false
	      | boolf (x :: xs) = pred x orelse boolf xs
	in boolf end;
    
    
fun append xs ys = xs @ ys;
    
fun find _ []        = Option.NONE
  | find p (x :: xs) = if p x then Option.SOME x else find p xs;
    


(* fun getenv var =
  (case OS.Process.getEnv var of
      NONE => ""
        | SOME txt => txt);
*)
(*	                                
fun print_depth n = 
   (Control.Print.printDepth := n div 2;
    Control.Print.printLength := n); 

val cd = OS.FileSys.chDir;
val pwd = OS.FileSys.getDir;
*)

fun foldr1 f l =
    let fun itr [x] = x
          | itr (x::l) = f(x, itr l)
    in  itr l  end;

(* use Option.map instead 
fun ap_some f (SOME x) = SOME(f x)
  |ap_some f NONE     = NONE
*)

fun separate s (x :: (xs as _ :: _)) = x :: s :: separate s xs
  | separate _ xs = xs;
(* fun suffix sfx s = s ^ sfx;*)

fun take (n, []) = []
  | take (n, x :: xs) =
    if n > 0 then x :: take (n - 1, xs) else [];
    
fun space_implode a bs = implode (separate a bs);

fun print_stderr s = (TextIO.output (TextIO.stdErr, s); TextIO.flushOut TextIO.stdErr);

exception ERROR;
   
(* val writeln = std_output o suffix "\n";*)
(* fun error_msg s = writeln(s) *)
fun info s  = print (s^"\n")
fun warn s  = print (s^"\n")
fun error_ (s,ex) = (print (s^"\n"); raise ex)
fun error' s = error_ (s,Fail s)
fun error s = print (s^"\n")


fun fst (x, y) = x
                 
fun snd (x, y) = y

end
