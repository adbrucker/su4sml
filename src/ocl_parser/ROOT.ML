(*****************************************************************************
 *          HOL-OCL - a shallow embedding of OCL into Isabelle/HOL              
 *                                                                            
 * ROOT.ML - main "ROOT.ML" file for HOL-OCL
 * Copyright (C) 2006,2007 Achim D. Brucker <achim@brucker.ch>
 *                         Burkhart Wolff   <bwolff@inf.ethz.ch>    
 *                                                                            
 * This file is part of HOL-OCL.                                              
 *                                                                            
 * HOL-OCL is free software; you can redistribute it and/or modify it under   
 * the terms of the GNU General Public License as published by the Free       
 * Software Foundation; either version 2 of the License, or (at your option)  
 * any later version.                                                         
 *                                                                            
 * HOL-OCL is distributed in the hope that it will be useful, but WITHOUT ANY 
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS 
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more 
 * details.                                                              
 *                                                                            
 * You should have received a copy of the GNU General Public License along    
 * with this program; if not, write to the Free Software Foundation, Inc.,    
 * 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.                  
 ******************************************************************************)


OS.FileSys.chDir "ml-yacc-lib"; 
use "root.sml"; 
OS.FileSys.chDir "..";

use "library.sml";
use "context_declarations.sml";
use "preprocessor.sml";
use "type_checker.sml";
use "make_classifier_list.sml";
use "ocl.grm.sig";
use "ocl.lex.sml";
nonfix THEN;
use "ocl.grm.sml";
use "parser.sml";
use "fix_types.sml";
use "model_import.sml";
