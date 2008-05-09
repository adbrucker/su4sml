signature WFCPOG_TESTSUITE =
sig
    (** buffer for storing output during testing *)
    val buffer            : string ref
    (** empty buffer *)
    val reset_buffer      : unit -> unit
    (** Executes a test on all (default) models and returns a text output.*)
    val runTests          : WFCPOG.wfpo list -> WFCPOG.wfpo list -> unit
    (** Executes a specified (string fst arg) test and returns a text output.*)
    val runTest           : string -> WFCPOG.wfpo list -> WFCPOG.wfpo list -> unit
    (** Exectues a test on all (default) models and returns, if any, pos.*)
    val runTests_ret_pos  : WFCPOG.wfpo list -> WFCPOG.wfpo list -> (Rep_OclType.Path * Rep_OclTerm.OclTerm) list
    (** Exectures a specified (string fst arg) test and returns the, if any, pos.*)
    val runTest_ret_pos   : string -> WFCPOG.wfpo list -> WFCPOG.wfpo list -> (Rep_OclType.Path * Rep_OclTerm.OclTerm) list
    (** Set Control.Print.printDepth. *)
    val spd               : int -> unit
    (** Set Control.Print.printLength. *)
    val spl               : int -> unit
    exception WFCPOG_TestSuiteError of string
end

structure WFCPOG_TestSuite : WFCPOG_TESTSUITE =
struct

open Rep_Logger
open Rep_OclTerm
open Rep_OclType
open WFCPOG
open WFCPOG_Refine_Constraint
open WFCPOG_Registry
open OclLibrary

exception WFCPOG_TestSuiteError of string

type testcase = 
     { 
      name        : string,
      uml         : string,
      ocl         : string
     }

exception TestSuitError of string

val buffer = ref ""

fun reset_buffer () = 
    let 
	val _ = buffer:=("")
    in
	()
    end

val prefix = "../../../hol-ocl/examples/"

val testcase = 
    { 
     name = "Ebank",
     uml = prefix^"ebank/ebank.zargo",
     ocl = prefix^"ebank/ebank.ocl"
    }


val testcases = [
   { 
    name = "stack_manu",
    uml  = prefix^"stack_manu/stack.zargo",
    ocl  = prefix^"stack_manu/stack.ocl"
   },{ 
    name = "stack",
    uml  = prefix^"stack/stack.zargo",
    ocl  = prefix^"stack/stack.ocl"
   },
   { 
    name = "simple_rfm",
    uml  = prefix^"simple_rfm/simple_rfm.zargo",
    ocl  = prefix^"simple_rfm/simple_rfm.ocl"
   },
   {
    name = "company",
    uml  = prefix^"company/company.zargo",
    ocl  = prefix^"company/company.ocl"
   }:testcase,
   {
    name = "ebank",
    uml  = prefix^"ebank/ebank.zargo",
    ocl  = prefix^"ebank/ebank.ocl"
   }:testcase,
   {
    name = "encoding_example",
    uml  = prefix^"encoding_example/encoding_example.zargo",
    ocl  = prefix^"encoding_example/encoding_example.ocl"
   }:testcase,(*
   {
    name = "isp",
    uml  = prefix^"isp/isp.zargo",
    ocl  = prefix^"isp/isp.ocl"
   }:testcase,*)
   {
    name = "royals_and_loyals",
    uml  = prefix^"royals_and_loyals/royals_and_loyals.zargo",
    ocl  = prefix^"royals_and_loyals/royals_and_loyals.ocl"
   }:testcase,  
   {
    name = "simple",
    uml  = prefix^"simple/simple.zargo",
    ocl  = prefix^"simple/simple.ocl"
   }:testcase,
   {
    name = "digraph",
    uml  = prefix^"digraph/digraph.zargo",
    ocl  = prefix^"digraph/digraph.ocl"
   }:testcase,
   {
    name = "vehicles",
    uml  = prefix^"vehicles/vehicles.zargo",
    ocl  = prefix^"vehicles/vehicles.ocl"
   }:testcase,
   {
    name = "SimpleChair",
    uml  = prefix^"SimpleChair/SimpleChair.zargo",
    ocl  = prefix^"SimpleChair/ConcreteSimpleChair01.ocl"
   }:testcase,
   {
    name = "overriding",
    uml  = prefix^"overriding/overriding.zargo",
    ocl  = prefix^"overriding/overriding.ocl"
   }:testcase
]

fun spd x = 
    let 
	val _ = Control.Print.printDepth:=x
    in
	print ("printDepth set to " ^ (Int.toString (x)) ^ ".\n")
    end

fun spl x = 
    let 
	val _ = Control.Print.printLength:=x
    in
	print ("printLength set to " ^ (Int.toString (x)) ^ ".\n")
    end

fun add_dot 1 = ["."]
  | add_dot x = (".")::(add_dot (x-1))

fun insert_dots string = if (String.size(string) >= 100)
			 then raise WFCPOG_TestSuiteError ("Name of wfpo to long...\n")
			 else String.concat (add_dot (100 - String.size(string)))
    
(* RETURN: (path,term) list *)
fun start_tests model [] = []
  | start_tests model (h::wfpos) = 
    case (apply_of h) of
	WFC (a) =>
	let 
	    val _ = trace wgen ("Testing a wellformed constraint: \n")
	    val res_wfcs = check_wfcs model [h]
		handle WFCPOG.WFC_FailedMessage s =>
		       let
			   val _  = trace wgen ("WFC_Failed_Exception handler ...\n")
			   val _  = buffer:=((!buffer)^s)
		       in
			   raise WFCPOG.WFC_FailedException(h,s) 
		       end
	    val check = List.all (fn (a,b) => b = true) res_wfcs
	in
	    case check of
		false => 
		let
		    val _ = trace wgen ("test is false ...\n")
		    val mes = ("\n" ^ (name_of h) ^ (insert_dots (name_of h)) ^ "[FAILED]\n")
		    val _ = trace wgen mes
		    val _ = buffer:=(!buffer)^mes
		    val _ = trace wgen ("results logged in buffer ...\n")
		in
		    ([])@(start_tests model wfpos)
		end
	      | true =>  
		let
		    val _ = trace wgen ("test is true ...\n")
		    val name = WFCPOG.name_of h
		    val mes = ("\n" ^ name ^ (insert_dots (name)) ^ "[OK]\n")
		    val _ = trace wgen mes
		    val _ = buffer:=(!buffer)^mes
		    val _ = trace wgen ("results logged in buffer ...\n")
		in
		    ([])@(start_tests model wfpos)
		end
	end 
      | POG (a) =>
	let
	    val _ = trace wgen ("Testing a proof obligation constraint: \n")
	    val pos = generate_pos model [h] 
		handle WFCPOG.WFC_FailedMessage s =>
		       let 
			   val _ = buffer:=((!buffer)^s)
		       in
			   raise WFCPOG.WFC_FailedException (h,s)
		       end
	in
	    case pos of 
		[(h,[(["Exception"],(Variable("x",OclVoid)))])] => 
		let
		    val _ = buffer:=(!buffer)^((name_of h ^ (insert_dots (name_of h)) ^ "[DEPENDING WFC NOT HOLD]\n"))
		in
		    ([])@(start_tests model wfpos)
		end	 
	      | wfpo_term_list => 
		let
		    val ret = List.map (fn (a,b) => 
					   let
					       val _ = buffer:=((!buffer)^(name_of a)^(insert_dots (name_of a))^"[ "^(Int.toString(List.length(b)))^" Terms ]\n")
					   in
					       b
					   end) wfpo_term_list
		in 
		    (ret)@(start_tests model wfpos)
		end
	end
	
fun test (tc:testcase) wfs pos = 
    let 
	val i_model = ModelImport.import (#uml tc) (#ocl tc) []
	val (clist,alist) = Rep_Core.normalize_ext i_model
	val model = ((clist@oclLib),(alist))
	val _ = trace wgen ("Model of testcase loaded ...\n")
	val x = start_tests model (wfs@pos)
	val _ = trace wgen ("Test finished ...\n")

    in
	x
    end

fun printResult s = print s

fun print_tc (tc:testcase)= 
	    let
		val s1 = "\n\n\n***************************************************\n"
		val s2 = "***************************************************\n"
		val name = (#name tc)
		val s3 = "\n***************************************************\n"
	    in
		s1^s2^name^s3
	    end

fun runTest name wfs pos = 
    let
	val _  = trace wgen ("Starts runing one test ...\n")
	val _  = reset_buffer()
	val tc = valOf (List.find (fn a => name = (#name a)) testcases)
	val _ = trace wgen ("Accessing model ...\n")
	val s1 = (print_tc tc)
	val _ = (test tc wfs pos)
	    handle WFCPOG.WFC_FailedException (wfpo,s) =>  
		   let
		       val _ = buffer:=((!buffer)^s)
		   in
		       []
		   end
	val _ = buffer:=s1^(!buffer)
    in
	(if (String.isSubstring "[Error]" (!buffer))
	then print ((!buffer)^"\n\n !!!!!!!!!! WFCPOG still contains bugs !!!!!!!!!!!!!\n\n\n")
	else print ((!buffer)^"\n\n !!!!!!!!!!  Congratulations, no bugs  !!!!!!!!!!!!!!\n\n\n")
	) 
    end


fun runTests wfs pos = 
    let 
	val _ = trace wgen ("Starts running tests ...\n")
	val _ = reset_buffer()
	val _ = List.map (fn a => 
			     let 
				 val s1 = (print_tc a)
				 val _ = buffer:=(!buffer)^s1
				 val data = (test a wfs pos)
				     handle WFCPOG.WFC_FailedException (wfpo,s) =>  
					    let
						val _ = buffer:=((!buffer)^s)
						val _ = trace wgen (!buffer)
					    in
						raise WFCPOG_TestSuiteError ("one wfc failed\n")
					    end
			     in
				 data
			     end) testcases
    in
	if (String.isSubstring "[ERROR]" (!buffer)) 
	then print ((!buffer)^"\n\n !!!!!!!!!! WFCPOG still contains bugs !!!!!!!!!!!!!\n\n\n")
	else print ((!buffer)^"\n\n !!!!!!!!!!  Congratulations, no bugs  !!!!!!!!!!!!!!\n\n\n")
    end


fun runTest_ret_pos name wfs pos = 
    let
	val _  = trace wgen ("Starts runing one test ...\n")
	val _  = reset_buffer()
	val tc = valOf (List.find (fn a => name = (#name a)) testcases)
	val _ = trace wgen ("Accessing model ...\n")
	val s1 = (print_tc tc)
	val pos = List.concat (test tc wfs pos)
	val _ = buffer:=s1^(!buffer)
	val _ = 
	     if (String.isSubstring "[Error]" (!buffer))
	     then print ((!buffer)^"\n\n !!!!!!!!!! WFCPOG still contains bugs !!!!!!!!!!!!!\n\n\n")
	     else print ((!buffer)^"\n\n !!!!!!!!!!  Congratulations, no bugs  !!!!!!!!!!!!!!\n\n\n")
    in
	pos
    end


fun runTests_ret_pos wfs pos =
    let 
	val _ = trace wgen ("Starts running tests ...\n")
	val _ = reset_buffer()
	val pos = List.concat (List.concat (List.map (fn a => 
					    let 
						val s1 = (print_tc a)
						val _ = buffer:=(!buffer)^s1
						val x = (test a wfs pos)
					    in
						x
					    end) testcases))
	val _ = 
	    if (String.isSubstring "[ERROR]" (!buffer)) 
	    then print ((!buffer)^"\n\n !!!!!!!!!! WFCPOG still contains bugs !!!!!!!!!!!!!\n\n\n")
	    else print ((!buffer)^"\n\n !!!!!!!!!!  Congratulations, no bugs  !!!!!!!!!!!!!!\n\n\n")
    in
	 pos
    end
end
