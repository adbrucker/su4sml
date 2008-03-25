signature WFCPOG_TESTSUITE =
sig
    val runTests          : WFCPOG.wfpo list -> WFCPOG.wfpo list -> unit
    val runTest           : WFCPOG.wfpo list -> WFCPOG.wfpo list -> unit
    val set_printDepth    : int -> unit
    val set_printLength   : int -> unit
end

structure WFCPOG_TestSuite : WFCPOG_TESTSUITE =
struct

open library
open WFCPOG_Registry
open WFCPOG
open OclLibrary


type testcase = 
     { 
      name        : string,
      uml         : string,
      ocl         : string
     }

exception TestSuitError of string
exception WFCPOG_RefineError = Refine_Constraint.WFCPOG_RefineError

val prefix = "../../../examples/"

val testcase = 
    { 
     name = "Ebank",
     uml = prefix^"ebank/ebank.zargo",
     ocl = prefix^"ebank/ebank.ocl"
    }


val testcases = [
   { 
    name = "Stack Manuel",
    uml  = prefix^"stack_manu/stack.zargo",
    ocl  = prefix^"stack_manu/stack.ocl"
   },{ 
    name = "Stack",
    uml  = prefix^"stack/stack.zargo",
    ocl  = prefix^"stack/stack.ocl"
   },
   {
    name = "Company",
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
    name = "Royals and Loyals",
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
   }:testcase
]

fun set_printDepth x = 
    let 
	val _ = Control.Print.printDepth:=x
    in
	print ("printDepth set to " ^ (Int.toString (x)) ^ ".\n")
    end

fun set_printLength x = 
    let 
	val _ = Control.Print.printLength:=x
    in
	print ("printLength set to " ^ (Int.toString (x)) ^ ".\n")
    end

fun add_dot 1 = ["."]
  | add_dot x = (".")::(add_dot (x-1))

fun insert_dots string = String.concat (add_dot (40 - String.size(string)))
    

fun start_tests model [] = []
  | start_tests model (h::wfpos) = 
    case (apply_of h) of
	WFC (a) =>
	(let 
	    val _ = trace wgen ("Testing a wellformed constraint: \n")
	in
	    case check_wfc model h of
		false => (((name_of h) ^ (insert_dots (name_of h)) ^ "[FAILED]\n"))::(start_tests model wfpos)
	      | true =>  (((name_of h) ^ (insert_dots (name_of h)) ^ "[OK]\n"))::(start_tests model wfpos)
	end
	handle WFCPOG_RefineError s => ((name_of h) ^ (insert_dots (name_of h)) ^ "[RefineEXCP]\n" ^ "       Error Msg: " ^ s ^ "\n")::(start_tests model wfpos)
	     | x =>((name_of h) ^ (insert_dots (name_of h)) ^ "[ERROR]\n")::(start_tests model wfpos)
	)
      | POG (a) =>
	(let
	     val _ = trace wgen ("Testing a proof obligation constraint: \n")
	 in
	     case generate_po model h of
		 (wfc,list) => ((name_of h ^ (insert_dots (name_of h)) ^ "[ " ^ (Int.toString(List.length(list))) ^ " Terms ]\n"))::(start_tests model wfpos)
	 end
	 handle x =>((name_of h ^ (insert_dots (name_of h)) ^ "[ERROR]\n"))::(start_tests model wfpos)
	)

fun test (tc:testcase) wfs pos = 
    let 
	val i_model = ModelImport.import (#uml tc) (#ocl tc) []

	val (clist,alist) = Rep_Core.normalize_ext i_model
	val model = ((clist@oclLib),(alist))
	val msg_list = start_tests model (wfs@pos)

    in
	String.concat (msg_list)
    end

fun printResult s = print s

fun print_tc (tc:testcase)= 
	    let
		val start_tc = "\n\n\n***************************************************\n\n"
		val name = (#name tc) ^ "\n\n"
	    in
		start_tc^name
	    end

fun runTest wfs pos = 
    let
	val string = (print_tc testcase)^(test testcase wfs pos)
    in
	if (String.isSubstring "[Error]" string)
	then print(string^"\n\n !!!!!!!!!! WFCPOG still contains bugs !!!!!!!!!!!!!\n\n\n")
	else print (string^"\n\n !!!!!!!!!!  Congratulations, no bugs  !!!!!!!!!!!!!!\n\n\n")
    end


fun runTests wfs pos = 
    let 
	val msg_list = List.map (fn a => (print_tc a)^(test a wfs pos)) testcases
	val string = String.concat msg_list
    in
	if (String.isSubstring "[ERROR]" string) 
	then print (string^"\n\n !!!!!!!!!! WFCPOG still contains bugs !!!!!!!!!!!!!\n\n\n")
	else print (string^"\n\n !!!!!!!!!!  Congratulations, no bugs  !!!!!!!!!!!!!!\n\n\n")
    end
end
