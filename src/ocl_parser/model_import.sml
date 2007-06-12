
(*****************************************************************************
 *          HOL-OCL - a shallow embedding of OCL into Isabelle/HOL              
 *                                                                            
 * model_import.sml - main "ROOT.ML" file for HOL-OCL
 * Copyright (C) 2006 Achim D. Brucker <achim@brucker.ch>
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



signature MODEL_IMPORT = 
sig

    val parseUML         : string -> Rep_Core.Classifier list
    val parseOCL         : string -> Context.context list
    val import           : string -> string -> string list -> Rep_Core.Classifier list
    val removePackages   : (Rep_Core.Classifier list * Context.context list) 
			   -> string list
			   -> (Rep_Core.Classifier list * Context.context list)
    val removeOclLibrary : Rep_Core.Classifier list -> Rep_Core.Classifier list
end

structure ModelImport : MODEL_IMPORT =
struct
(* basic library *)
open List
open Posix.Error
     
(* OclParser *)
open Ext_Library
open Context
open TypeChecker
open Update_Model


(* Error logging *)
val high = 5
val medium = 20
val low = 100

fun readFileUnNormalized f = 
                  (RepParser.transformXMI o XmiParser.readFile) f

fun importArgoUMLUnNormalized file =
    let
        fun basename  f = ((hd o rev) o (String.fields (fn x => x = #"/"))) f

        val tmpFile = OS.FileSys.tmpName ()
        val base =  if String.isSuffix ".zargo" file
                    then String.substring(file,0, (String.size file) -6)
                    else file
        val _ = print ("*** Syscall: unzip -ca "^base^".zargo "^(basename base)^".xmi > "^tmpFile^"\n")
        val _ = OS.Process.system ("unzip -ca "^base^".zargo "^(basename base)^".xmi > "^tmpFile)
        val model = readFileUnNormalized tmpFile
        val _ = OS.FileSys.remove tmpFile

    in
        model
    end





fun parseUML umlFile  = 
    let
	val _        =  trace high "### Parsing UML Model ###\n"
	val umlModel = if  String.isSuffix ".zargo" umlFile
		       then importArgoUMLUnNormalized umlFile
		       else readFileUnNormalized umlFile
	val _        = trace high ("### Finished Parsing UML Model ("
			      ^(Int.toString(length umlModel))
			      ^" Classifiers found)###\n\n")
    in
	umlModel
    end

fun parseOCL oclFile =
    let
	val _ =  trace high "### Parsing OCL File ###\n"
	val ocl = case oclFile of 
		      "" =>      []
		    | filename => OclParser.parse_contextlist oclFile;
	val _ =  trace high ("### Finished Parsing OCL File ("
			^(Int.toString(length ocl))
			^" Constraints Found) ###\n\n")
    in
	ocl
    end

fun removePackages (uml,ocl) packageList = 
    let
        fun filter_package model p = filter (fn cl => not (Rep_Core.package_of cl = p)) model
        fun filter_cl_package cl p = filter (fn cl => not (package_of_context cl = p)) cl
	val _ =  trace high "### Excluding Packages ###\n"
	val uml = 
	    let 
		fun stringToPath s = (String.tokens (fn s => (s = (#":"))) s)
	    in 
		foldr (fn (p,m) => filter_package m (stringToPath  p)) uml packageList  
	    end
	val ocl = 
	    let 
		fun stringToPath s = (String.tokens (fn s => (s = (#":"))) s)
	    in 
		foldr (fn (p,m) => filter_cl_package m (stringToPath  p)) ocl packageList  
	    end
	val _ =  trace high ("### Finished excluding Packages ("
		 ^(Int.toString(length uml))
		 ^ " Classifiers found and "
		 ^(Int.toString(length ocl))
		 ^" Constraints Found) ###\n\n")
    in
	(uml,ocl)
    end

fun removeOclLibrary model = 
    let
        fun filter_template model = 
	    let
		fun is_template (Rep_Core.Template _) = true
		  | is_template _ = false
	    in
		filter (not o is_template) model
	    end
        fun filter_oclLib model = filter (not o OclLibrary.is_oclLib) model
    in 
	(filter_oclLib o filter_template) model
    end

fun import xmifile oclfile excludePackages = 
    let
        val xmi = parseUML xmifile
	val ocl = parseOCL oclfile
	val (xmi,ocl) = removePackages (xmi,ocl) excludePackages


	val model = case ocl of 
			[] => xmi
		      | ocl => let
			    val _         = trace high "### Preprocess Context List ###\n"
			    val fixed_ocl = Preprocessor.preprocess_context_list ocl ((OclLibrary.oclLib)@xmi)
			    val _         = trace high "### Finished Preprocess Context List ###\n\n"	

			    val _         = trace high "### Type Checking ###\n"
			    val typed_cl  = TypeChecker.check_context_list fixed_ocl ((OclLibrary.oclLib)@xmi);
			    val _         = trace high "### Finished Type Checking ###\n\n"

			    val _         = print"### Updating Classifier List ###\n"
			    val model     = Update_Model.gen_updated_classifier_list typed_cl ((OclLibrary.oclLib)@xmi);
			    val _         = trace high ("### Finished Updating Classifier List "
							^(Int.toString(length model))
							^ " Classifiers found (11 from 'oclLib') ###\n")

			    val _         = trace high "### Fixing Types ###\n"
	                    val model = removeOclLibrary  model
			    val model     = FixTyping.transform_ocl_spec FixTyping.transformForHolOcl model 
			    val _         = trace high "### Finished Fixing Types ###\n\n"


			in 
			    model 
			end
			      
    in
       model 
    end
    
end
