(* open structures *)

(* SU4SML *)
open Rep_Logger
open OclLibrary
open ModelImport
open Rep_Core


val _ = init_offset()

val prefix = "../../../hol-ocl/examples/"


(* set debugging settings *)
val _ = Control.Print.printDepth:=20
val _ = Control.Print.printLength:=30

(* 
val zargo = prefix^"simple_rfm/simple_rfm.zargo"
val ocl = prefix^"simple_rfm/simple_rfm.ocl"
val remP = []
*)
(* 
val zargo = "../../../../examples/meeting/Meeting.zargo"
val ocl = ""
*)
(*
val zargo = "../../../hol-ocl/examples/SimpleChair/SimpleChair.zargo"
val ocl = "../../../hol-ocl/examples/SimpleChair/AbstractSimpleChair04.ocl"
val remP = ["AbstractSimpleChair02", "AbstractSimpleChair03","AbstractSimpleChair01","ConcreteSimpleChair01"] ;
*)

(** EBANK **)
(* 
val zargo = "../../../examples/ebank/ebank.zargo"
val ocl="../../../examples/ebank/ebank.ocl"
val remP = []
*)


(** OVERRIDING **)
(*
val zargo = "../../../hol-ocl/examples/overriding/overriding.zargo"
val ocl="../../../hol-ocl/examples/overriding/overriding.ocl"
val remP = []
*)

(** ISP **)
(* 
val zargo = "../../../examples/isp/isp.zargo"
val ocl="../../../examples/isp/isp.ocl"
*)


(** ROYALS AND LOYASL **)
(* 
val zargo = "../../../examples/royals_and_loyals/royals_and_loyals.zargo"
val ocl="../../../examples/royals_and_loyals/royals_and_loyals.ocl"
val remP = []
*)


(** SIMPLE **)
(*
val zargo = "../../../examples/simple/simple.zargo"
val ocl="../../../examples/simple/simple.ocl"
val remP = []
*)
(** DIGRAPH **)
(*
val zargo = "../../../examples/digraph/digraph.zargo"
val ocl = "../../../examples/digraph/digraph.ocl"
val remP = []
*)

(** VEHICLES **)
(* 
val zargo = "../../../examples/vehicles/vehicles.zargo"
val ocl = "../../../examples/vehicles/vehicles.ocl"
*)

(** OVERRRIDING **)
(*
val zargo = "../../../hol-ocl/examples/overriding/overriding.zargo"
val ocl="../../../hol-ocl/examples/overriding/overriding.ocl"
val remP = []
*)

val zargo = "../../../hol-ocl/examples/stack_manu/stack.zargo"
val ocl="../../../hol-ocl/examples/stack_manu/stack.ocl"
val remP = []

(** import model *)
val XMI = parseUML zargo
val _ = init_offset()
val OCL = parseOCL ocl
val _ = init_offset()
val (xmi_cls,xmi_assocs) = XMI
val _ = init_offset()
val fixed_ocl = Preprocessor.preprocess_context_list OCL ((OclLibrary.oclLib)@xmi_cls)
val typed_cl = TypeChecker.check_context_list fixed_ocl (((OclLibrary.oclLib)@xmi_cls),xmi_assocs)
val model = Update_Model.gen_updated_classifier_list typed_cl ((OclLibrary.oclLib)@xmi_cls)
val model = removePackages remP (model,xmi_assocs)

