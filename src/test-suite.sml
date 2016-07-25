(*****************************************************************************
 * su4sml --- a SML repository for managing (Secure)UML/OCL models
 *             http://projects.brucker.ch/su4sml/
 *                                                                            
 * test-suite.sml --- a regression test-suite for su4sml
 * This file is part of su4sml.
 *
 * Copyright (c) 2006, 2007 ETH Zurich, Switzerland
 *               2008-2009  Achim D. Brucker, Germany
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
  val runTest : unit -> unit
end

structure testsuite:TESTSUITE = struct
type result = {
     parse      : bool,
     preprocess : bool,
     typecheck  : bool,
     update     : bool,
     transform  : bool,
     rmpkg      : bool,
     codegen    : bool,
     msg        : string
}

type testcase = {
     name : string,
     uml  : string,
     ocl  : string,
     exclude : string list,
     result : result
}

exception TestSuiteException of string

val initResult = {
     parse      = false,
     preprocess = false,
     typecheck  = false,
     update     = false,
     transform  = false,
     codegen     = false,
     rmpkg       = false,
     msg        = ""
    }:result


val prefix = "../../examples/"

val testcases = [
   {
    name = "overriding",
    uml  = prefix^"overriding/overriding.zargo",
    ocl  = prefix^"overriding/overriding.ocl",
    exclude = [],
    result = initResult
   }:testcase,
   {
    name = "Company",
    uml  = prefix^"company/company.zargo",
    ocl  = prefix^"company/company.ocl",
    exclude = [],
    result = initResult
   }:testcase,
   {
    name = "ebank",
    uml  = prefix^"ebank/ebank.zargo",
    ocl  = prefix^"ebank/ebank.ocl",
    exclude = [],
    result = initResult
   }:testcase,
   {
    name = "encoding_example",
    uml  = prefix^"encoding_example/encoding_example.zargo",
    ocl  = prefix^"encoding_example/encoding_example.ocl",
    exclude = [],
    result = initResult
   }:testcase,
   {
    name = "isp",
    uml  = prefix^"isp/isp.zargo",
    ocl  = prefix^"isp/isp.ocl",
    exclude = [],
    result = initResult
   }:testcase,
   {
    name = "Royals and Loyals",
    uml  = prefix^"royals_and_loyals/royals_and_loyals.zargo",
    ocl  = prefix^"royals_and_loyals/royals_and_loyals.ocl",
    exclude = [],
    result = initResult
   }:testcase,  
   {
    name = "simple",
    uml  = prefix^"simple/simple.zargo",
    ocl  = prefix^"simple/simple.ocl",
    exclude = [],
    result = initResult
   }:testcase,
   {
    name = "stack",
    uml  = prefix^"stack/stack.zargo",
    ocl  = prefix^"stack/stack.ocl",
    exclude = [],
    result = initResult
   }:testcase,
   {
    name = "digraph",
    uml  = prefix^"digraph/digraph.zargo",
    ocl  = prefix^"digraph/digraph.ocl",
    exclude = [],
    result = initResult
   }:testcase,
   {
    name = "vehicles",
    uml  = prefix^"vehicles/vehicles.zargo",
    ocl  = prefix^"vehicles/vehicles.ocl",
    exclude = [],
    result = initResult
   }:testcase ,
   {
    name = "Red-black Trees",
    uml  = prefix^"rbt/rbt.zargo",
    ocl  = prefix^"rbt/rbt.ocl",
    exclude = [],
    result = initResult
   }:testcase,
   {
    name = "Invoicing Orders",
    uml  = prefix^"InvoicingOrders/InvoicingOrders.zargo",
    ocl  = prefix^"InvoicingOrders/InvoicingOrders.ocl",
    exclude = [],
    result = initResult
   }:testcase,
   {
    name = "SimpleChair (AbstractsimpleChair01)",
    uml  = prefix^"SimpleChair/specification/SimpleChair.zargo",
    ocl  = prefix^"SimpleChair/specification/AbstractSimpleChair01.ocl",
    exclude = [],
    result = initResult
   }:testcase,
   {
    name = "SimpleChair (AbstractSimpleChair04)",
    uml  = prefix^"SimpleChair/specification/SimpleChair.zargo",
    ocl  = prefix^"SimpleChair/specification/AbstractSimpleChair04.ocl",
    exclude = ["AbstractSimpleChair01", "AbstractSimpleChair02", 
	       "AbstractSimpleChair03",  "ConcreteSimpleChair01", 
	       "ConcreteSimpleChair02"],
    result = initResult
   }:testcase 
]



fun test (tc:testcase) = 
    let 
        val _   = print ("Running test case '"^(#name tc)^" ...'\n")
	val xmi = ModelImport.parseUML (#uml tc)
	    handle _ => ([],[]) 
	val ocl = ModelImport.parseOCL (#ocl tc)
	    handle _ => [] 
	val OclParse = if ocl = [] then false else true
	val fixed_ocl = Preprocessor.preprocess_context_list 
			    ocl ((OclLibrary.oclLib)@(#1 xmi))
	    handle _ => []    
	val OclPreprocess = if fixed_ocl = [] then false else true
	val typed_cl  = TypeChecker.check_context_list 
			    fixed_ocl (((OclLibrary.oclLib)@(#1 xmi)),#2 xmi)
	    handle _ => []   
	val OclTC = if typed_cl = [] then false else true
	val model     = Update_Model.gen_updated_classifier_list 
			    typed_cl ((OclLibrary.oclLib)@(#1 xmi))
	    handle _ => []   
	val modelUpdate = if model = [] then false else true
  	val cl     = ModelImport.removeOclLibrary (model)
	val cl        = FixTyping.transform_ocl_spec FixTyping.transformForHolOcl 
			   (#1 (Rep_Core.normalize_ext (cl, #2 xmi)))
	                handle _ => []   
	val transform = if cl = [] then false else true
	val clrm      = ModelImport.removePackages (#exclude tc) (cl, #2xmi)
	                handle _ => (([],[]):Rep_Core.transform_model)   
	val rmPkg = if (#exclude tc) = [] then true 
		    else if (List.length cl) > (List.length (#1 clrm))
		    then true 
		    else false
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
	     exclude  = #exclude tc,
	     result = 
	     {
	      parse      = OclParse,
	      preprocess = OclParse andalso OclPreprocess,
	      typecheck  = OclParse andalso OclPreprocess andalso OclTC,
	      update     = OclParse andalso OclPreprocess andalso OclTC andalso modelUpdate,
	      transform  = OclParse andalso OclPreprocess andalso OclTC andalso modelUpdate andalso transform, 
	      rmpkg      = OclParse andalso OclPreprocess andalso OclTC andalso modelUpdate andalso transform andalso rmPkg, 
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
	val _   = print ("   transform:   "^(printBool (#transform res))^"\n")
	val _   = print ("   rm pkg:      "^(printBool (#rmpkg res))^"\n")
	val _   = print ("   codegen:     "^(printBool (#codegen res))^"\n\n")
	val _   = print ("   ==> summary: "^(printBool ((#parse res) 
							andalso (#preprocess res) 
							andalso (#typecheck res) 
							andalso (#update res) 
							andalso (#codegen res)
							andalso (#transform res)
							andalso (#rmpkg res)
					    ))^"\n")
    in
      ()	
    end



fun printSummary res = 
    let
      fun printBool b = if b then "passed" else "FAILED"
      fun tcsummary (tc:testcase) = 
	  let
	    val res = (#result tc) 
	  in
	    (#parse res) 
	    andalso (#preprocess res) 
	    andalso (#typecheck res) 
	    andalso (#update res) 
	    andalso (#codegen res)
	    andalso (#transform res)
	    andalso (#rmpkg res)
	  end
      val summary = foldl (fn (a,b) => a andalso b) true (map tcsummary res)
      val _ = print ("\n")
      val _ = print (" ******************************\n")
      val _ = print (" * Overall result:     "^(printBool summary)^" *\n")      
      val _ = print (" ******************************\n")
    in
      ()
    end

  fun runTest () = let 
    val results = (map test testcases)
    val _       = map printResult results
    val _       = printSummary results
  in 
    ()
  end
end
 
val _ = testsuite.runTest()

