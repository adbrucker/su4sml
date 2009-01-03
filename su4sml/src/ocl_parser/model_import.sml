(*****************************************************************************
 * su4sml --- a SML repository for managing (Secure)UML/OCL models
 *             http://projects.brucker.ch/su4sml/
 *                                                                            
 * model_import.sml --- 
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


signature MODEL_IMPORT = 
sig
    val parseUML         : string -> Rep_Core.transform_model
    val parseOCL         : string -> Context.context list
    val parseModel       : string -> Rep_Core.Classifier list
    val import           : string -> string -> string list -> Rep_Core.transform_model
    val removePackages   : string list -> Rep_Core.transform_model 
			   -> Rep_Core.transform_model
    val removeOclLibrary : Rep_Core.Classifier list  -> Rep_Core.Classifier list
end

structure ModelImport : MODEL_IMPORT =
struct
(* basic library *)
open List
open Posix.Error

(* su4sml *)
open Rep_Core
     
(* OclParser *)
open Context
open TypeChecker
open Update_Model

(* Rep_Transform *)
(* FIXME: library consolidation? *)
open Rep_Transform


(* Error logging *)
val high = 5
val medium = 20
val low = 100

fun readFileUnNormalized f = 
                  (RepParser.transformXMI_ext o XmiParser.readFile) f

fun importArgoUMLUnNormalized file =
    let
        fun basename  f = ((hd o rev) o (String.fields (fn x => x = #"/"))) f

        val tmpFile = OS.FileSys.tmpName ()
        val base =  if String.isSuffix ".zargo" file
                    then String.substring(file,0, (String.size file) -6)
                    else file
        val _ = Logger.info ("*** Syscall: unzip -p -ca "^base^".zargo "^(basename base)^".xmi > "^tmpFile)
        val _ = OS.Process.system ("unzip -p -ca "^base^".zargo "^(basename base)^".xmi > "^tmpFile)
        val model = readFileUnNormalized tmpFile
        val _ = OS.FileSys.remove tmpFile


    in
        model
    end





fun parseUML umlFile  = 
    let
	val _        =  Logger.info "### Parsing UML Model ###\n"
	val umlModel = if  String.isSuffix ".zargo" umlFile
		       then importArgoUMLUnNormalized umlFile
		       else readFileUnNormalized umlFile
	val _        = Logger.info ("### Finished Parsing UML Model ("
			      ^(Int.toString(length (#1 umlModel)))
			      ^" Classifiers found)###\n\n")
    in
	umlModel
    end

fun parseOCL oclFile =
    let
	val _ =  Logger.info "### Parsing OCL File ###\n"
	val context_classes = case oclFile of 
		      "" =>      ([],[])
		    | filename => OclParser.parse_contextlist oclFile;
	val _ =  Logger.info ("### Finished Parsing OCL File ("
			^(Int.toString(length (#1 context_classes)))
			^" Constraints Found) ###\n\n")
    in
	(#1 context_classes)
    end

fun parseModel oclFile =  
    let
	val _ =  Logger.info "### Parsing OCL File ###\n"
	val context_classes = case oclFile of 
		      "" =>      ([],[])
		    | filename => OclParser.parse_contextlist oclFile;
	val _ =  Logger.info ("### Finished Parsing OCL File ("
			^(Int.toString(length (#2 context_classes)))
			^" Constraints Found) ###\n\n")
    in
	(#2 context_classes)
    end

fun removePackages packageList (cl,al) =
    let
        fun filter_package_assoc model p = filter 
					 (fn a => not ((rev o tl o rev) (Rep_Core.name_of_association a) = p)) model
        fun filter_package model p = filter (fn cl => not (Rep_Core.package_of cl = p)) model
        val _ =  Logger.info "### Excluding Packages ###\n"
        fun stringToPath s = (String.tokens (fn s => (s = (#":"))) s)
        val cl =foldr (fn (p,m) => filter_package m (stringToPath  p)) cl packageList
        val al =foldr (fn (p,m) => filter_package_assoc m (stringToPath  p)) al packageList
        val _ =  Logger.info ("### Finished excluding Packages ("
                 ^(Int.toString(length cl))
                 ^ " Classifiers found ###\n\n")
        (* TODO: Implement check for dangeling references/Types and Ocl Expressions *)
    in
        (cl,al)
    end

    
fun removeOclLibrary (model) = 
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
	((filter_oclLib o filter_template) model)
    end

fun import xmifile oclfile excludePackages = 
    let
        val xmi = parseUML xmifile
  (*	val _ = init_offset() *)
	val ocl = parseOCL oclfile
        val (xmi_cls, xmi_assocs) = xmi
  (*	val _ = init_offset() *)


	val model = case ocl of 
			[] => (xmi_cls,xmi_assocs)
		      | ocl => let
(* 		    val _         = init_offset() *)

			    val _         = Logger.info "### Preprocess Context List ###\n"
			    val fixed_ocl = Preprocessor.preprocess_context_list ocl ((OclLibrary.oclLib)@xmi_cls)
			    val _         = Logger.info "### Finished Preprocess Context List ###\n\n"	
(* 			    val _         = init_offset() *)

			    val _         = Logger.info "### Type Checking ###\n"
			    val typed_cl  = TypeChecker.check_context_list fixed_ocl (((OclLibrary.oclLib)@xmi_cls),xmi_assocs);
			    val _         = Logger.info "### Finished Type Checking ###\n\n"
(* 			    val _         = init_offset() *)

			    val _         = Logger.info "### Updating Classifier List ###\n"
			    val model     = Update_Model.gen_updated_classifier_list typed_cl ((OclLibrary.oclLib)@xmi_cls);
			    val _         = Logger.info ("### Finished Updating Classifier List "
							^(Int.toString(length model))
							^ " Classifiers found (11 from 'oclLib') ###\n")
(* 			    val _         = init_offset() *)

			    val _         = Logger.info "### Fixing Types ###\n"
	                    val model = removeOclLibrary  model
                            val model = removePackages excludePackages (model,xmi_assocs)
			    (*
			    val model     = FixTyping.transform_ocl_spec FixTyping.transformForHolOcl model 
			    *)
			    val _         = Logger.info "### Finished Fixing Types ###\n\n"
			in 
			    model 
			end
			      
    in
	(* FIXME: propagate associations into the ocl_parser *)
       model
    end
    
end
