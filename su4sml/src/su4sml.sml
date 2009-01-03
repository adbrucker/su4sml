(*****************************************************************************
 * su4sml --- a SML repository for managing (Secure)UML/OCL models
 *             http://projects.brucker.ch/su4sml/
 *                                                                            
 * su4sml.sml --- 
 * This file is part of su4sml.
 *
 * Copyright (c) 2008 Achim D. Brucker, Germany
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


structure su4sml = struct  

fun basename name = (hd o rev) (String.fields (fn s => s = #"/" orelse s = #"\\") name)

fun print_usage name = let
  val _ = print ("Type '"^name^" help' for usage.\n")
in
0
end

fun print_help name = let 
  val _ = print("\n")
  val _ = print("usage: "^name^" <subcommand> [options] [args]\n")
  val _ = print("Su4sml command-line client")
  val _ = print("Type '"^name^" help <subcommand>' for help on a specific subcommand.\n")
  val _ = print("\n")
  val _ = print("Available subcommands:\n")
  val _ = print("   check-model\n")
  val _ = print("   codegen\n")
  val _ = print("   transform-model\n")
  val _ = print("   typecheck\n")
  val _ = print("\n")
  val _ = print("Su4sml is a tool for working with UML/OCL and SecureUML/OCL models.\n")
  val _ = print("For additional information, see http://projects.brucker.ch/projects/su4msl/\n")
in
  0
end 


structure typecheck = struct

fun print_usage() = let
  val _ = print("\n")
  val _ = print("typecheck: typecheck a UML/OCL specification\n")
  val _ = print("usage: typeckeck UML [OCL]\n")
  val _ = print("\n")
  val _ = print("UML can be either a ArgoUML file (i.e, *.zargo), a compatible XMI file \n")
  val _ = print("or the output of the Dresden OCL Toolkit, version 2.0. In the latter \n")
  val _ = print("case, the OCL specification contained in the XMI from Dresden OCL \n")
  val _ = print("is merged with the specification given in the OCL file.\n")
(*  val _ = print("\n")
  val _ = print("Valid options:\n") *)
in
  0
end


fun check uml ocl = let
  val xmi = ModelImport.parseUML uml
      handle _ => ([],[])
  val ocl = ModelImport.parseOCL ocl
      handle _ => []
  val OclParse = if ocl = [] then false else true
  val _         = Logger.info "### Preprocess Context List ###\n"
  val fixed_ocl = Preprocessor.preprocess_context_list
                      ocl ((OclLibrary.oclLib)@(#1 xmi))
      handle _ => []
  val OclPreprocess = if fixed_ocl = [] then false else true
  val OclPreprocess = OclPreprocess andalso OclParse
  val _         = Logger.info "### Finished Preprocess Context List ###\n\n"
		  
  val _         = Logger.info "### Type Checking ###\n"
  val typed_cl  = TypeChecker.check_context_list
                      fixed_ocl (((OclLibrary.oclLib)@(#1 xmi)),#2 xmi)
      handle _ => []
  val OclTC     = if typed_cl = [] then false else true
  val OclTC     = OclTC andalso OclPreprocess
  val _         = Logger.info "### Finished Type Checking ###\n\n"
		  
  val _         = Logger.info "### Updating Classifier List ###\n"
  val model     = Update_Model.gen_updated_classifier_list
                      typed_cl ((OclLibrary.oclLib)@(#1 xmi))
      handle _ => []
  val modelUpdate = if model = [] then false else true
  val modelUpdate = modelUpdate andalso OclTC
  val _         = Logger.info "### Finished Updating Classifier List ###\n"
  
		  
  fun printBool b = if b then "passed" else "FAILED"
  val _   = print ("\n *** type checking result ***\n")
  val _   = print ("   parsing:     "^(printBool OclParse)^"\n")
  val _   = print ("   preprocess:  "^(printBool OclPreprocess)^"\n")
  val _   = print ("   typecheck:   "^(printBool OclTC)^"\n")
  val _   = print ("   update:      "^(printBool modelUpdate)^"\n")
  val _   = print ("   ==> summary: "^(printBool modelUpdate)^"\n")
in 
  if modelUpdate then 0 else 1
end


end


fun main (name:string,args:(string list)) = 
    let 
      val prgName = (hd o rev) (String.fields (fn s => s = #"/" orelse s = #"\\") name); 
      val _ = Logger.set_log_level Logger.WARN
    in
      case (prgName,args) of 
	(n, [])                       => print_usage n
      (* su4sml          *)
      | ("su4sml", ["help"])          => print_help "su4sml"
      | ("su4sml", ["help", subcmd])  => main(subcmd,["help"]) 
      (* check-model     *)

      | (_, ["check-model", "help"])  => let val _ =  print "not yet supported \n" in 0 end
      | (_, "check-model"::_)         => let val _ =  print "not yet supported \n" in 0 end
      (* codegen         *)
      | (_, ["codegen", "help"])      => let val _ =  Codegen.print_usage() in 0 end
      | ("su4sml",  "codegen"::args)  => Codegen.main("su4sml",args)
      | (_, "codegen"::_)             => let val _ =  Codegen.print_usage() in 0 end
      (* transform-model *)
      | (_, ["transform-model", "help"]) => let val _ =  print "not yet supported \n" in 0 end
      | (_, "transform-model"::_)     => let val _ =  print "not yet supported \n" in 0 end
      (* typecheck       *)
      | (_, ["typecheck", "help"])    => typecheck.print_usage()
      | (_, ["typecheck", uml])       => typecheck.check uml "" 
      | (_, ["typecheck", uml,ocl])   => typecheck.check uml ocl
      | (_, "typecheck"::_)           => typecheck.print_usage()
      (* default match:  *)
      | (n,_)                         => print_usage n
    end
end

val _ = su4sml.main(CommandLine.name(), CommandLine.arguments())
