(*****************************************************************************
 *          su4sml GCG - Generic Code Generator                     
 *                                                                            
 * gcg_library.sml - provides simple library needed by su4sml-gcg
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

structure GCG_Library = struct
(* is this used anywhere? *)
exception GCG_Error

fun gcg_error s = (print ("Error:"^s^"\n"); raise GCG_Error);

fun gcg_warning s = (print ("Warning: "^s^"\n"));

fun fieldSplit s d = String.fields (fn c => (c = d)) s

local
	fun endsWithEscape "" = false
	  | endsWithEscape s = (substring(s,size(s)-1,1) = "\\")
in
 fun joinEscapeSplitted d [] 	   = []
   | joinEscapeSplitted d [l]	   = [l]
   | joinEscapeSplitted d (h::s::t) = if endsWithEscape(h)
  				     then (substring(h,0,size(h)-1)^d^s)::t
  				     else h::(joinEscapeSplitted d (s::t))
end 

val curry = fn f => fn x => fn y => f (x, y)
val uncurry = fn f => fn (x, y) => f x y
			     
end
