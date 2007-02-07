(*****************************************************************************
 *          su4sml GCG - Generic Code Generator   
 *                                                                            
 * gcg_helper.sml - helper library for su4sml-gcg
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


structure Gcg_Helper =
struct

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
 

(* FIXME: move to ListEq (and rename to isPrefix...) (is this even used somewhere?) *)
fun isSuffix  [] _ = true
 |  isSuffix  _ [] = false
 |  isSuffix (h1::t1) (h2::t2) = (h1=h2) andalso (isSuffix t1 t2)

(* FIXME: cleanup *)
fun assureDir file = 
    let val dirList = rev (tl (rev (String.tokens (fn c => c = #"/") file)))

	fun assert1 "" d = ((if (OS.FileSys.isDir d) then () else ())
			    handle SysErr => (OS.FileSys.mkDir d) )
	  | assert1 prefix d = (if (OS.FileSys.isDir (prefix^"/"^d)) then () else ())
	    handle SysErr => (OS.FileSys.mkDir (prefix^"/"^d))
			     

	fun assertDList _ [] = ()
	  | assertDList prefix [d] = assert1 prefix d
	  | assertDList "" (h::t) = (assert1 "" h ; assertDList h t)
	  | assertDList prefix (h::t) = (assert1 prefix h; 
			   		 assertDList (prefix^"/"^h) t ) 
    in
	assertDList "" dirList
    end


end
