signature WFCPOG_TESTSUITE =
sig
    val runTest          : WFCPOG.wfpo list -> WFCPOG.wfpo list -> unit
    val set_printDepth   : int -> unit
    val set_printLength  : int -> unit
end

structure WFCPOG_TestSuite : WFCPOG_TESTSUITE =
struct

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


val prefix = "../../../examples/"

val testcases = [
   { 
    name = "Stack Manuel",
    uml  = prefix^"stack_manu/stack.zargo",
    ocl  = prefix^"stack_manu/stack.ocl"
   } (*,{ 
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
   }:testcase ,
   {
    name = "SimpleChair",
    uml  = prefix^"SimpleChair/SimpleChair.zargo",
    ocl  = "SimpleChair/ConcreteSimpleChair01.ocl"
   }:testcase *)
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
    

fun start_wfc_tests model [] = []
  | start_wfc_tests model ((h)::wfcs) = 
    (case check_wfc model h of
	false => (((name_of h) ^ (insert_dots (name_of h)) ^ "[FAILED]\n"))::(start_wfc_tests model wfcs)
      | true =>  (((name_of h) ^ (insert_dots (name_of h)) ^ "[OK]\n"))::(start_wfc_tests model wfcs)
    ) handle x =>(((name_of h) ^ (insert_dots (name_of h)) ^ "[EXCP]\n"))::(start_wfc_tests model wfcs)

fun start_pog_tests model [] = []
  | start_pog_tests model (h::wfcs) = 
    (case generate_po model h of
	 (wfc,list) => ((name_of h ^ (insert_dots (name_of h)) ^ "[ " ^ (Int.toString(List.length(list))) ^ " Terms ]\n"))::(start_wfc_tests model wfcs)
    ) handle x =>((id_of h ^ (insert_dots (id_of h)) ^ "[EXCP]\n"))::(start_wfc_tests model wfcs)



fun test (tc:testcase) wfs pos = 
    let 
	val i_model = ModelImport.import (#uml tc) (#ocl tc) []
	val (clist,alist) = Rep_Core.normalize_ext i_model
	val model = ((clist@oclLib),(alist))
			
	val msg1 = (String.concat (start_wfc_tests model wfs))
	val msg2 = (String.concat (start_pog_tests model pos))
    in
	msg1^msg2
    end

fun printResult s = print s


fun runTest wfs pos = 
    let 
	fun print_tc (tc:testcase)= 
	    let
		val start_tc = "\n\n\n***************************************************\n\n"
		val name = (#name tc) ^ "\n\n"
	    in
		start_tc^name
	    end
	val msg_list = List.map (fn a => (print_tc a)^(test a wfs pos)) testcases
	val string = String.concat msg_list
    in
	if (String.isSubstring "[EXCP]" string) 
	then print (string^"\n\n !!!!!!!!!! WFCPOG still contains bugs !!!!!!!!!!!!!\n\n\n")
	else print (string^"\n\n !!!!!!!!!!  Congratulations, no bugs  !!!!!!!!!!!!!!\n\n\n")
    end
end
