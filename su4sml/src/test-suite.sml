(*****************************************************************************
 * su4sml --- a SML repository for managing (Secure)UML/OCL models
 *             http://projects.brucker.ch/su4sml/
 *                                                                            
 * test-suite.sml --- a regression test-suite for su4sml
 * This file is part of su4sml.
 *
 * Copyright (c) 2006, 2007 ETH Zurich, Switzerland
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

signature TESTSUITE = sig
  val runTest : unit -> unit list
end

structure testsuite:TESTSUITE = struct
type result = {
     parse      : bool,
     preprocess : bool,
     typecheck  : bool,
     update     : bool,
     codegen    : bool,
     msg        : string
}

type testcase = {
     name : string,
     uml  : string,
     ocl  : string,
     result : result
}


val initResult = {
     parse      = false,
     preprocess = false,
     typecheck  = false,
     update     = false,
     codegen     = false,
     msg        = ""
    }:result


val prefix = "../../examples/"

val testcases = [
   {
    name = "Company",
    uml  = prefix^"company/company.zargo",
    ocl  = prefix^"company/company.ocl",
    result = initResult
   }:testcase,
   {
    name = "ebank",
    uml  = prefix^"ebank/ebank.zargo",
    ocl  = prefix^"ebank/ebank.ocl",
    result = initResult
   }:testcase,
   {
    name = "encoding_example",
    uml  = prefix^"encoding_example/encoding_example.zargo",
    ocl  = prefix^"encoding_example/encoding_example.ocl",
    result = initResult
   }:testcase,
   {
    name = "isp",
    uml  = prefix^"isp/isp.zargo",
    ocl  = prefix^"isp/isp.ocl",
    result = initResult
   }:testcase,
   {
    name = "Royals and Loyals",
    uml  = prefix^"royals_and_loyals/royals_and_loyals.zargo",
    ocl  = prefix^"royals_and_loyals/royals_and_loyals.ocl",
    result = initResult
   }:testcase,  
   {
    name = "simple",
    uml  = prefix^"simple/simple.zargo",
    ocl  = prefix^"simple/simple.ocl",
    result = initResult
   }:testcase,
   {
    name = "stack",
    uml  = prefix^"stack/stack.zargo",
    ocl  = prefix^"stack/stack.ocl",
    result = initResult
   }:testcase,
   {
    name = "digraph",
    uml  = prefix^"digraph/digraph.zargo",
    ocl  = prefix^"digraph/digraph.ocl",
    result = initResult
   }:testcase,
   {
    name = "vehicles",
    uml  = prefix^"vehicles/vehicles.zargo",
    ocl  = prefix^"vehicles/vehicles.ocl",
    result = initResult
   }:testcase,
   {
    name = "SimpleChair",
    uml  = prefix^"SimpleChair/SimpleChair.zargo",
    ocl  = "",
    result = initResult
   }:testcase
]



fun test (tc:testcase) = 
    let 
	val xmi = ModelImport.parseUML (#uml tc)
	    handle _ => ([],[])
	val ocl = ModelImport.parseOCL (#ocl tc)
	    handle _ => []
	val OclParse = if ocl = [] then false else true
	val (xmi,ocl) = ModelImport.removePackages (xmi,ocl) []
	    handle _ => (([],[]),[]) 

	val _         = print "### Preprocess Context List ###\n"
	val fixed_ocl = Preprocessor.preprocess_context_list 
			    ocl ((OclLibrary.oclLib)@(#1 xmi))
	    handle _ => []
	val OclPreprocess = if fixed_ocl = [] then false else true
	val _         = print "### Finished Preprocess Context List ###\n\n"	
			
	val _         = print "### Type Checking ###\n"
	val typed_cl  = TypeChecker.check_context_list 
			    fixed_ocl (((OclLibrary.oclLib)@(#1 xmi)),#2 xmi)
	    handle _ => []
	val OclTC = if typed_cl = [] then false else true
	val _         = print "### Finished Type Checking ###\n\n"
			
	val _         = print"### Updating Classifier List ###\n"
	val model     = Update_Model.gen_updated_classifier_list 
			    typed_cl ((OclLibrary.oclLib)@(#1 xmi))
	    handle _ => []
	val modelUpdate = if model = [] then false else true
	val _         = print "### Finished Updating Classifier List ###\n"

  	val model     = ModelImport.removeOclLibrary model

	val CodeGen = 
	    let 
		val _  = Codegen.generateFromModel xmi "java"
	    in
		true
	    end       
            handle  _ => false
 
		      

    in 
	    {
	     name = #name tc,
	     uml  = #uml tc,
	     ocl  = #ocl tc,
	     result = 
	     {
	      parse      = OclParse,
	      preprocess = OclParse andalso OclPreprocess,
	      typecheck  = OclParse andalso OclPreprocess andalso OclTC,
	      update     = OclParse andalso OclPreprocess andalso OclTC andalso modelUpdate,
	      codegen    = OclParse andalso OclPreprocess andalso OclTC andalso modelUpdate andalso CodeGen, 
	      msg        = ""
	     }:result
	    }:testcase
    end

fun printResult (tc:testcase) = 
    let
	fun printBool b = if b then "passed" else "FAILED"
	val _   = print ("\n *** "^(#name tc)^" ***\n")
	val res = (#result tc) 
	val _   = print ("   parsing:     "^(printBool (#parse res))^"\n")
	val _   = print ("   preprocess:  "^(printBool (#preprocess res))^"\n")
	val _   = print ("   typecheck:   "^(printBool (#typecheck res))^"\n")
	val _   = print ("   update:      "^(printBool (#update res))^"\n")
	val _   = print ("   codegen:     "^(printBool (#codegen res))^"\n\n")
	val _   = print ("   ==> overall: "^(printBool ((#parse res) 
							andalso (#preprocess res) 
							andalso (#typecheck res) 
							andalso (#update res) 
							andalso (#codegen res)))^"\n")

    in
	()
    end

(* val _ = (Ext_Library.log_level := 1);(); *)

fun runTest () = map printResult  (map test testcases)

end
  
val _ = testsuite.runTest()

