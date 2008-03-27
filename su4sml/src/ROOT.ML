(*****************************************************************************
 * su4sml --- a SML repository for managing (Secure)UML/OCL models
 *             http://projects.brucker.ch/su4sml/
 *                                                                            
 * ROOT.ML ---
 * This file is part of su4sml.
 *
 * Copyright (c) 2005-2007, ETH Zurich, Switzerland
 *
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *
 *     * Redistributions in binary form must reproduce the above
 *       copyright notice, this list of conditions and the following
 *       disclaimer in the documentation and/or other materials provided
 *       with the distribution.
 *
 *     * Neither the name of the copyright holders nor the names of its
 *       contributors may be used to endorse or promote products derived
 *       from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 ******************************************************************************)
(* $Id$ *)

use "rep_logger.sml";
use "rep_help_functions.sml"
use "stringHandling.sml";

(* ****************************************************** *)
(* Load the (foreign) fxp-module providing
   an elemantary library in processing xml documents. *)
OS.FileSys.chDir "../lib/fxp/src"; 
use "ROOT.ML"; 
OS.FileSys.chDir "../../../src";

OS.FileSys.chDir "compiler";

use "compiler_ext.sig";

val ml_system  = getOpt (OS.Process.getEnv "ML_SYSTEM", "poly")

val use_wrapper = if (String.isSubstring "poly" ml_system)
		  then (use "../contrib/HashTable.sml"; "polyml.sml")
                  else "smlnj.sml";


use use_wrapper; (* or "smlnj.sml", "polyml.sml" or "mlton.sml" *) 

OS.FileSys.chDir ".."; 


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
use "rep_ocl.sml";
use "rep_state_machines.sml";
use "rep_activity_graphs.sml";
use "rep_core.sml";
use "ocl_library.sml";
(* use "rep_secureuml.sig"; *)
(* use "rep_secureuml.sml"; *)
use "rep.sml";




(* support functions *)
use "ocl2string.sml";
use "ocl2dresdenjava.sml";

(* ****************************************************** *)
(* Main Conversion Processes  *)
use "xmltree.sml";
use "xmltree_helper.sml";
use "xmltree_hooks.sml";
use "xmltree_writer.sml";
use "xmltree_parser.sml"; (* provides explicit xml-tree data structure,
                             abstracts away fxp package. *)
use "xmi_parser.sml";        (* conversion XML to XMI *);

use "xmi_idtable.sml";    (* auxiliary table to store and dereference xmi.id's *)
use "rep_parser.sml";        (* conversion XMI to Rep *)

use "transform_library.sml";
use "rep_transform.sml";
use "listeq.sml";
use "mds.sig";
use "component_uml.sml";
use "secure_uml.sml";
use "rep_secure.sml";
use "rep_su2holocl.sml";

(* ******************************************************** *)
(* OCL parser and type-checker                              *)
OS.FileSys.chDir "ocl_parser"; 
use "ROOT.ML"; 
OS.FileSys.chDir "..";

(* ******************************************************** *)
(* codegen-module                                           *)
OS.FileSys.chDir "codegen"; 
use "ROOT.ML"; 
OS.FileSys.chDir "..";
(* ******************************************************** *)
(* wfcpog-module                                           *)
OS.FileSys.chDir "wfcpo-gen"; 
use "ROOT.ML"; 
OS.FileSys.chDir "..";

val _ = print "successfully compiled su4sml\n"

