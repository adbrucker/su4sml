(*****************************************************************************
 *          uml2cdl - a CDL parser using a UML profile              
 *                                                                            
 * ROOT.ML - main "ROOT.ML" file for uml2cdl
 * Copyright (C) 2005 Achim D. Brucker <brucker@inf.ethz.ch>   
 *                    Juergen Doser    <doserj@inf.ethz.ch>
 *                    Burkhart Wolff   <bwolff@inf.ethz.ch>
 *                                                                            
 * This file is part of uml2cdl.                                              
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


(* ****************************************************** *)
(* include su4sml library:                                *)
val curdir = OS.FileSys.getDir ();
OS.FileSys.chDir "../lib/su4sml/src";
use "ROOT.ML";
OS.FileSys.chDir  curdir;

(* ****************************************************** *)
(* Representation of a Choreography in WS-CDL             *)
use "cdl.sml";

(* ****************************************************** *)
(* Conversion Processes                                   *)
use "xmi2cdl.sml"; (* conversion XMI to CDL *) 
(* use "rep2cdl.sml"; (* alternatively, use Rep-datastructures? *) *)
use "cdl2xml.sml"; (* conversion CDL to XML *)

(* use "uml2cdl.sml"; (* main function *) *)
