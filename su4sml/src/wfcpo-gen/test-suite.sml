open WFCPOG_Registry
open WFCPOG
open OclLibrary

(* set debugging settings *)
val _ = Control.Print.printDepth:=20
val _ = Control.Print.printLength:=30



(************* GET BASE CONSTRAINT *************************)

(** LISKOV CONSTRAINT **)
val lsk = WFCPOG_Registry.get_wfpo WFCPOG_Registry.supported "lsk"

(** INTERFACE CONSTRAINT **)
val inf = WFCPOG_Registry.get_wfpo WFCPOG_Registry.supported "inf"

(** DATA MODEL CONSTRAINT **)
val cm = WFCPOG_Registry.get_wfpo WFCPOG_Registry.supported "class_model"
val sm = WFCPOG_Registry.get_wfpo WFCPOG_Registry.supported "strong_model"

(** OPERATIONAL CONSTRAINT **)
val om = WFCPOG_Registry.get_wfpo WFCPOG_Registry.supported "oper_model"

(** COMMAND/QUERY CONSTRAINT **)
val cmd = WFCPOG_Registry.get_wfpo WFCPOG_Registry.supported "cmd"
val quy = WFCPOG_Registry.get_wfpo WFCPOG_Registry.supported "quy"

(** VISIBILITY CONSTRAINT **)
val vis = WFCPOG_Registry.get_wfpo WFCPOG_Registry.supported "vis"

(** REFINEMENT CONSTRAINT **)
val rfm_syn = WFCPOG_Registry.get_wfpo WFCPOG_Registry.supported "rfm_syn"

(* TAXONOMY CONSTRAINT *)
val tax = WFCPOG_Registry.get_wfpo WFCPOG_Registry.supported "tax"


(************** CREATE STATIC CONSTRAINTS ********************)

val md0 = WFCPOG_Registry.rename_wfpo "md0" (WFCPOG_Registry.TAX_Data.put ({key=9,max_depth=0}) tax)
val md1 = WFCPOG_Registry.rename_wfpo "md1" (WFCPOG_Registry.TAX_Data.put ({key=9,max_depth=1}) tax)
val md2 = WFCPOG_Registry.rename_wfpo "md2" (WFCPOG_Registry.TAX_Data.put ({key=9,max_depth=2}) tax)
val md3 = WFCPOG_Registry.rename_wfpo "md3" (WFCPOG_Registry.TAX_Data.put ({key=9,max_depth=3}) tax)
val md4 = WFCPOG_Registry.rename_wfpo "md4" (WFCPOG_Registry.TAX_Data.put ({key=9,max_depth=4}) tax)
val md5 = WFCPOG_Registry.rename_wfpo "md5" (WFCPOG_Registry.TAX_Data.put ({key=9,max_depth=5}) tax)
val md6 = WFCPOG_Registry.rename_wfpo "md6" (WFCPOG_Registry.TAX_Data.put ({key=9,max_depth=6}) tax)
val md7 = WFCPOG_Registry.rename_wfpo "md7" (WFCPOG_Registry.TAX_Data.put ({key=9,max_depth=7}) tax)
val md8 = WFCPOG_Registry.rename_wfpo "md8" (WFCPOG_Registry.TAX_Data.put ({key=9,max_depth=8}) tax)

val wfcs = [inf,vis,md0,md1,md2,md3,md4,md5,md6,md7,md8]
val pos = [lsk,cm,sm,om,cmd,quy]

(*
val wfcs = [vis]
val pos = []
*)
fun add_dot 1 = ["."]
  | add_dot x = (".")::(add_dot (x-1))

fun insert_dots string = String.concat (add_dot (20 - String.size(string)))
    

fun start_wfc_tests model [] = []
  | start_wfc_tests model (h::wfcs) = 
    (case check_wfc model h of
	false => ((id_of h ^ (insert_dots (id_of h)) ^ "[FAILED]\n"))::(start_wfc_tests model wfcs)
      | true =>  ((id_of h ^ (insert_dots (id_of h)) ^ "[OK]\n"))::(start_wfc_tests model wfcs)
    ) handle x =>((id_of h ^ (insert_dots (id_of h)) ^ "[EXCP]\n"))::(start_wfc_tests model wfcs)

fun start_pog_tests model [] = []
  | start_pog_tests model (h::wfcs) = 
    (case generate_po model h of
	(wfc,list) => ((id_of h ^ (insert_dots (id_of h)) ^ "[ " ^ (Int.toString(List.length(list))) ^ " Terms ]\n"))::(start_wfc_tests model wfcs)
    ) handle x =>((id_of h ^ (insert_dots (id_of h)) ^ "[EXCP]\n"))::(start_wfc_tests model wfcs)




val buffer = ref []:string list ref

val _ = buffer := (!buffer)@["\n\n\n\n"]

(** MODELS **)
(*
val zargo = "../../../examples/SimpleChair/SimpleChair.zargo"
val ocl = "../../../examples/SimpleChair/ConcreteSimpleChair01.ocl"
val simple = import zargo ocl []


val zargo = "../../../examples/ebank/ebank.zargo"
val ocl = "../../../examples/ebank/ebank.ocl"
val ebank = import zargo ocl []
*)


(** STACK_MANU **)
val _ = buffer := (!buffer)@["***********************************\n\n"]
val _ = buffer := (!buffer)@["STACK_MANU (without Refinement)..........[OK]\n\n"]
val zargo = "../../../examples/stack_manu/stack.zargo"
val ocl = "../../../examples/stack_manu/stack.ocl"
val i_model = ModelImport.import zargo ocl []
val (clist,alist) = Rep_Core.normalize_ext i_model
val model = ((clist@oclLib),(alist))
val _ = buffer := (!buffer)@[(String.concat (start_wfc_tests model wfcs))]
val _ = buffer := (!buffer)@[(String.concat (start_pog_tests model pos))]
val _ = buffer := (!buffer)@[("\nSTACK_MANU finished ...\n\n\n")]
val _ = buffer := (!buffer)@["***********************************\n\n\n"]

(** STACK **)
val _ = buffer := (!buffer)@["***********************************\n\n"]
val _ = buffer := (!buffer)@["STACK (without Refinement)..........[FIXME] \n\n"]
val zargo = "../../../examples/stack/stack.zargo"
val ocl = "../../../examples/stack/stack.ocl"
val i_model = ModelImport.import zargo ocl []
val (clist,alist) = Rep_Core.normalize_ext i_model
val model = ((clist@oclLib),(alist))
val _ = buffer := (!buffer)@[(String.concat (start_wfc_tests model wfcs))]
val _ = buffer := (!buffer)@[(String.concat (start_pog_tests model pos))]
val _ = buffer := (!buffer)@[("\nSTACK finished ...\n\n\n")]
val _ = buffer := (!buffer)@["***********************************\n\n\n"]

(** EBANK **)
val _ = buffer := (!buffer)@["***********************************\n\n"]
val _ = buffer := (!buffer)@["EBANK (without Refinement) \n\n"]
val zargo = "../../../examples/ebank/ebank.zargo"
val ocl = "../../../examples/ebank/ebank.ocl"
val i_model = ModelImport.import zargo ocl []
val (clist,alist) = Rep_Core.normalize_ext i_model
val model = ((clist@oclLib),(alist))
val _ = buffer := (!buffer)@[(String.concat (start_wfc_tests model wfcs))]
val _ = buffer := (!buffer)@[(String.concat (start_pog_tests model pos))]
val _ = buffer := (!buffer)@[("\nEBANK finished ...\n\n\n")]
val _ = buffer := (!buffer)@["***********************************\n\n\n"]

(** DIGRAPH **)
val _ = buffer := (!buffer)@["***********************************\n\n"]
val _ = buffer := (!buffer)@["DIGRAPH (without Refinement) \n\n"]
val zargo = "../../../examples/digraph/digraph.zargo"
val ocl = "../../../examples/digraph/digraph.ocl"
val i_model = ModelImport.import zargo ocl []
val (clist,alist) = Rep_Core.normalize_ext i_model
val model = ((clist@oclLib),(alist))
val _ = buffer := (!buffer)@[(String.concat (start_wfc_tests model wfcs))]
val _ = buffer := (!buffer)@[(String.concat (start_pog_tests model pos))]
val _ = buffer := (!buffer)@[("\nDIGRAPH finished ...\n\n\n")]
val _ = buffer := (!buffer)@["***********************************\n\n\n"]

(** VEHICLES **)
val _ = buffer := (!buffer)@["***********************************\n\n"]
val _ = buffer := (!buffer)@["VEHICLES (without Refinement) \n\n"]
val zargo = "../../../examples/vehicles/vehicles.zargo"
val ocl = "../../../examples/vehicles/vehicles.ocl"
val i_model = ModelImport.import zargo ocl []
val (clist,alist) = Rep_Core.normalize_ext i_model
val model = ((clist@oclLib),(alist))
val _ = buffer := (!buffer)@[(String.concat (start_wfc_tests model wfcs))]
val _ = buffer := (!buffer)@[(String.concat (start_pog_tests model pos))]
val _ = buffer := (!buffer)@[("\nVEHICLES finished ...\n\n\n")]
val _ = buffer := (!buffer)@["***********************************\n\n\n"]

(** ROYALS_AND_LOYALS **)
val _ = buffer := (!buffer)@["***********************************\n\n"]
val _ = buffer := (!buffer)@["ROYALS_AND_LOYALS (without Refinement) \n\n"]
val zargo = "../../../examples/royals_and_loyals/royals_and_loyals.zargo"
val ocl = "../../../examples/royals_and_loyals/royals_and_loyals.ocl"
val i_model = ModelImport.import zargo ocl []
val (clist,alist) = Rep_Core.normalize_ext i_model
val model = ((clist@oclLib),(alist))
val _ = buffer := (!buffer)@[(String.concat (start_wfc_tests model wfcs))]
val _ = buffer := (!buffer)@[(String.concat (start_pog_tests model pos))]
val _ = buffer := (!buffer)@[("\nROYALS_AND_LOYALS finished ...\n\n\n")]
val _ = buffer := (!buffer)@["***********************************\n\n\n"]

(** CALENDAR **)
(*
val _ = buffer := (!buffer)@["***********************************\n\n"]
val _ = buffer := (!buffer)@["CALENDAR (without Refinement) \n\n"]
val zargo = "../../../examples/calendar/calendar.zargo"
val ocl = "../../../examples/calendar/calendar.ocl"
val i_model = ModelImport.import zargo ocl []
val (clist,alist) = Rep_Core.normalize_ext i_model
val model = ((clist@oclLib),(alist))
val _ = buffer := (!buffer)@[(String.concat (start_wfc_tests model wfcs))]
val _ = buffer := (!buffer)@[(String.concat (start_pog_tests model pos))]
val _ = buffer := (!buffer)@[("\nCALENDAR finished ...\n\n\n")]
val _ = buffer := (!buffer)@["***********************************\n\n\n"]
*)
val output = String.concat (!buffer)
(*
val model = []
*)
