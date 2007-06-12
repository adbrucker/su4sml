

type result = {
     parse      : bool,
     preprocess : bool,
     typecheck  : bool,
     update     : bool,
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
     msg        = ""
    }:result


val prefix = "../examples/"

val testcases = [
   {
    name = "Company",
    uml  = prefix^"company/company.zargo",
    ocl  = prefix^"company/company.ocl",
    result = initResult
   }:testcase,
   {
    name = "ebank (Manuel)",
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
   }:testcase
]



fun test (tc:testcase) = 
    let 
	val xmi = ModelImport.parseUML (#uml tc)
	    handle _ => []
	val ocl = ModelImport.parseOCL (#ocl tc)
	    handle _ => []
	val OclParse = if ocl = [] then false else true
	val (xmi,ocl) = ModelImport.removePackages (xmi,ocl) []
	    handle _ => ([],[])

	val _         = print "### Preprocess Context List ###\n"
	val fixed_ocl = Preprocessor.preprocess_context_list ocl ((OclLibrary.oclLib)@xmi)
	    handle _ => []
	val OclPreprocess = if fixed_ocl = [] then false else true
	val _         = print "### Finished Preprocess Context List ###\n\n"	
			
	val _         = print "### Type Checking ###\n"
	val typed_cl  = TypeChecker.check_context_list fixed_ocl ((OclLibrary.oclLib)@xmi)
	    handle _ => []
	val OclTC = if typed_cl = [] then false else true
	val _         = print "### Finished Type Checking ###\n\n"
			
	val _         = print"### Updating Classifier List ###\n"
	val model     = Update_Model.gen_updated_classifier_list typed_cl ((OclLibrary.oclLib)@xmi)
	    handle _ => []
	val modelUpdate = if model = [] then false else true
	val _         = print "### Finished Updating Classifier List ###\n"

	val model     = ModelImport.removeOclLibrary model
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
	val _   = print ("   update:      "^(printBool (#update res))^"\n\n")
	val _   = print ("   ==> overall: "^(printBool ((#parse res) 
							andalso (#preprocess res) 
							andalso (#typecheck res) 
							andalso (#update res)))^"\n")

    in
	()
    end

val _ = (Ext_Library.log_level := 10);();
 
val testcases = map test testcases

val _         = map printResult testcases 

