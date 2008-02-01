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
    val import           : string -> string -> string list -> Rep_Core.transform_model
    val removePackages   : (Rep_Core.transform_model * Context.context list) 
			   -> string list
			   -> (Rep_Core.transform_model * Context.context list)
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
        val _ = print ("*** Syscall: unzip -p -ca "^base^".zargo "^(basename base)^".xmi > "^tmpFile^"\n")
        val _ = OS.Process.system ("unzip -p -ca "^base^".zargo "^(basename base)^".xmi > "^tmpFile)
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
			      ^(Int.toString(length (#1 umlModel)))
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
	    (* filter package and update associations 
	     * fun filter_package model p = filter (fn cl => not 
       * (Rep_Core.package_of cl = p)) model *)
      fun filter_package (all_classifiers,all_associations) p = 
	        let
		        (* FIXME: correct handling for reflexive assocs + !isNavigable *)
		        fun valid_assoc {name,aends,qualifiers,aclass} = 
                List.length aends > 1

		        fun update_association cls_name {name,aends,qualifiers,aclass}
                :Rep_Core.association = 
		            let
			            val cls_path = Rep_OclType.path_of_OclType cls_name
			            val modified_aclass = if (cls_path = (valOf aclass)) 
                                        then NONE
					                              else aclass
			            val modified_aends = filter (fn {aend_type,...} => 
                                                  not (aend_type = cls_name)) 
                                              aends
		            in
			            {name=name,
			             aends=modified_aends,
                   qualifiers=qualifiers (*FIXME?*),
			             aclass=modified_aclass}
		            end

		        fun update_associationends ((Rep_Core.Class {name,associations,
                                                         ...}),assocs):
                Rep_Core.association list =
		            let
			            val assocs = map (get_association all_associations) 
                                   associations
			            val modified_assocs = map (update_association name) assocs
		            in
			            filter valid_assoc modified_assocs
		            end
		          |  update_associationends ((Rep_Core.AssociationClass
                                              {name, associations,association,
                                               ...}),assocs) =
		             let
			             (* update_association also handles the aclass update *)
			             val assocs = map (get_association all_associations) 
                                    (association::associations)
			             val modified_assocs = map (update_association name) assocs
		             in
			             filter valid_assoc modified_assocs
		             end
		          |  update_associationends ((Rep_Core.Primitive
                                              {name,associations,...}),assocs)=
		             let
			             val assocs = map (get_association all_associations) 
                                    associations
			             val modified_assocs = map (update_association name) assocs
		             in
			             filter valid_assoc modified_assocs
		             end
		          |  update_associationends ((Rep_Core.Template
                                              {parameter,classifier}),assocs) =
		             (* FIXME: sound? *)
		             update_associationends (classifier,assocs)
		          |  update_associationends (_,assocs) =
		             assocs
                 
		        val (kept_classifiers,removed_cls) = 
                List.partition (fn cl => not (Rep_Core.package_of cl = p)) 
                               all_classifiers
		        val kept_associations = 
                (case removed_cls of 
					         []    => all_associations
					       | xs   => foldl update_associationends all_associations xs)
	        in
		        (kept_classifiers,kept_associations)
	        end
          
      fun filter_cl_package cl p = 
          List.filter (fn cl => not (package_of_context cl = p)) cl
	    val _ =  trace high "### Excluding Packages ###\n"
	    val uml = 
	        let 
		        fun stringToPath s = (String.tokens (fn s => (s = (#":"))) s)
	        in 
		        foldr (fn (p,m) => filter_package m (stringToPath  p)) 
                  uml packageList  
	        end
	    val ocl = 
	        let 
		        fun stringToPath s = (String.tokens (fn s => (s = (#":"))) s)
	        in 
		        foldr (fn (p,m) => filter_cl_package m (stringToPath  p)) 
                  ocl packageList  
	        end
	    val _ =  trace high ("### Finished excluding Packages ("
		                       ^(Int.toString(length (#1 uml)))
		                       ^ " Classifiers found and "
		                       ^(Int.toString(length (#2 uml)))
		                       ^ " Associations found and "
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
	val ((xmi_cls,xmi_assocs),ocl) = removePackages (xmi,ocl) excludePackages


	val model = case ocl of 
			[] => xmi_cls
		      | ocl => let
			    val _         = trace high "### Preprocess Context List ###\n"
			    val fixed_ocl = Preprocessor.preprocess_context_list ocl ((OclLibrary.oclLib)@xmi_cls)
			    val _         = trace high "### Finished Preprocess Context List ###\n\n"	

			    val _         = trace high "### Type Checking ###\n"
			    val typed_cl  = TypeChecker.check_context_list fixed_ocl (((OclLibrary.oclLib)@xmi_cls),xmi_assocs);
			    val _         = trace high "### Finished Type Checking ###\n\n"

			    val _         = print"### Updating Classifier List ###\n"
			    val model     = Update_Model.gen_updated_classifier_list typed_cl ((OclLibrary.oclLib)@xmi_cls);
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
	(* FIXME: propagate associations into the ocl_parser *)
       (model,xmi_assocs)
    end
    
end
