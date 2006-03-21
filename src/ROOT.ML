(*****************************************************************************
 *          su4sml - a SecureUML repository for SML              
 *                                                                            
 * ROOT.ML - main "ROOT.ML" file for su4sml
 * Copyright (C) 2005 Achim D. Brucker <brucker@inf.ethz.ch>   
 *                    Juergen Doser    <doserj@inf.ethz.ch>
 *                    Burkhart Wolff   <bwolff@inf.ethz.ch>
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

use "library.sml";


(* library should be opend locally for MLton compatibility... *)
(* open library; *)


(* ****************************************************** *)
(* Load the (foreign) fxp-module providing
   an elemantary library in processing xml documents. *)
OS.FileSys.chDir "../lib/fxp/src"; 
use "ROOT.ML"; 
OS.FileSys.chDir "../../../src";


(* ****************************************************** *)
(* Abstract Representation of an XMI File of a UML Model.
   - References kept
   - only "interesting" parts were represented
   - structural simplifications whereever needed for
     our applications.
   - Layout Information skippedi
   - the structure is oriented towards UML 1.5
     (although the supported Poseidon is apparantly based
     on Version 1.4 or something ...).  *)
use "xmi_ocl.sml"; 
use "xmi_datatypes.sml";
use "xmi_extension_mechanisms.sml";
use "xmi_state_machines.sml"; 
use "xmi_activity_graphs.sml"; 
use "xmi_core.sml"; 
use "xmi.sml"; 



(* ****************************************************** *)
(* Abstract Representation ("The Repository") of a UML model.
   - References resolved
   - only supported parts were represented
   - structural simplifications whereever needed for
     our applications.  *)
use "rep_ocl.sig";
use "rep_ocl.sml";
use "rep_state_machines.sig";
use "rep_state_machines.sml";
use "rep_activity_graphs.sig";
use "rep_activity_graphs.sml";
use "rep_core.sig";
use "rep_core.sml";
(* use "rep_secureuml.sig"; *)
(* use "rep_secureuml.sml"; *)
use "rep.sig";
use "rep.sml";





(* support functions *)
use "ocl2string.sml";

(* ****************************************************** *)
(* Main Conversion Processes  *)
use "xmltree_parser.sml"; (* provides explicit xml-tree data structure,
                             abstracts away fxp package. *)
use "xml2xmi.sml";        (* conversion XML to XMI *);

use "xmi_idtable.sml";    (* auxiliary table to store and dereference xmi.id's *)
use "xmi2rep.sml";        (* conversion XMI to Rep *)


use "mds.sig";
use "component_uml.sml";
use "secure_uml.sml";
use "rep_secure.sig";
use "rep_secure.sml";

(* ******************************************************** *)
(* codegen-module, developed by Raphael Eidenbenz           *)
OS.FileSys.chDir "codegen"; 
use "ROOT.ML"; 
OS.FileSys.chDir "..";
