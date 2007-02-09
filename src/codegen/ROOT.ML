(*****************************************************************************
 *          su4sml GCG - Generic Code Generator                          
 *                                                                            
 * ROOT.ML - main "ROOT.ML" file for su4sml-GCG
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


use "gcg_helper.sml";
use "stringHandling.sml";
(*use "examples/simple.sml"; *)
(*use "examples/ebank.sml";*)

use "tpl_parser.sml";

(* Base *)
use "cartridge.sig";
use "base_cartridge.sml";

(* C# *)
use "c#_cartridge.sml";
use "c#_net1_cartridge.sml";

(* SecureUML *)
use "design_cartridge.sig";
use "secureuml_cartridge.sml";
use "componentuml_cartridge.sml";


use "java_cartridge.sml";
use "junit_cartridge.sml"; 


(* Statemachines *)
use "SM_helper.sml";
use "stateMachineTypes.sml";
use "stateMachine.sml";
use "c#sm_cartridge.sig";
use "c#sm_cartridge.sml";

use "gcg_core.sig";
use "gcg_core.sml";

use "codegen.sml";
 			




