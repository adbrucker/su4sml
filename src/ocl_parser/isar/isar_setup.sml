(*****************************************************************************
 *          HOL-OCL - a shallow embedding of OCL into Isabelle/HOL              
 *                                                                            
 * model_import. - main "ROOT.ML" file for HOL-OCL
 * Copyright (C) 2006,2007 Achim D. Brucker <achim@brucker.ch>
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
 *****************************************************************************)


OS.FileSys.chDir ".."; 
use "ROOT.ML";
OS.FileSys.chDir "isar"; 

structure isar_import_setup =
struct 
structure P = OuterParse and K = OuterKeyword;




fun load_xmiOclT ((xmiFile,oclFile),excludedPackages)  thy =
    let

        fun load_mds xmiFile oclFile thy  =
	    let
                val model = map Rep.normalize (ModelImport.import xmiFile oclFile excludedPackages)
		val thy = rep_encoder.add_classifiers model thy;	    
            in
                thy
            end

	val thy = load_mds xmiFile oclFile  thy
    in
        thy
    end

val load_xmiOclP =
    OuterSyntax.command "import_model" "import UML/OCL specification"
                        OuterKeyword.thy_script
                        ((P.name -- (Scan.optional P.name "") 
				 -- (Scan.optional (P.$$$ "[" |-- P.list1 P.name --| P.$$$ "]") [] ))  
                        >> (fn((xmi,ocl),exPackages) => 
               (Toplevel.theory ( fn thy => load_xmiOclT ((xmi,ocl),exPackages) thy))))

val _ =  OuterSyntax.add_parsers [load_xmiOclP];
     
        
end

