(*****************************************************************************
 *          su4sml - a SecureUML repository for SML              
 *                                                                            
 * ROOT.ML - main "ROOT.ML" file for su4sml
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

use "library.sml";
open library;
fun su4sml_home () = if (getenv "HOLOCL_HOME") <> ""
                  then getenv "HOLOCL_HOME"
		                    else ".";


cd "../lib/fxp/src"; 
use "ROOT.ML"; 
cd "../../../src";

use "ocl.sig";
use "ocl.sml";
use "mdr_core.sig";
use "mdr_core.sml";
use "state_machine.sig";
use "mdr.sig";
use "xmi_uml.sml";
use "xmltree_parser.sml";
use "parse_xmi.sml";
use "xmi2mdr.sml";

