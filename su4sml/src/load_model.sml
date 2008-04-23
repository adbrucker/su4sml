(* open structures *)

(* SU4SML *)
open Rep_Logger
open OclLibrary
open ModelImport
open Rep_Core


val _ = init_offset()

(* set debugging settings *)
val _ = Control.Print.printDepth:=20
val _ = Control.Print.printLength:=30
(* 
val zargo = "../../../examples/meeting/Meeting.zargo"
val ocl = ""
*)
val zargo = "../../examples/SimpleChair/SimpleChair.zargo"
val ocl = "../../examples/SimpleChair/AbstractSimpleChair01.ocl"
val remP = ["AbstractSimpleChair02", "AbstractSimpleChair03","AbstractSimpleChair04","ConcreteSimpleChair01","ConcreteSimpleChair02"] ;

(* 
(** EBANK **)
val zargo = "../../examples/ebank/ebank.zargo"
val ocl="../../examples/ebank/ebank.ocl"
val remP = []
*)
(*
(** ISP **)
val zargo = "../../examples/isp/isp.zargo"
val ocl="../../examples/isp/isp.ocl"
*)

(*
(** ROYALS AND LOYASL **)
val zargo = "../../examples/royals_and_loyals/royals_and_loyals.zargo"
val ocl="../../examples/royals_and_loyals/royals_and_loyals.ocl"
val remP = []
*)


(** SIMPLE **)
(*
val zargo = "../../examples/simple/simple.zargo"
val ocl="../../examples/simple/simple.ocl"
val remP = []
*)
(** DIGRAPH **)
(*
val zargo = "../../examples/digraph/digraph.zargo"
val ocl = "../../examples/digraph/digraph.ocl"
val remP = []
*)
(*
(** VEHICLES **)
val zargo = "../../examples/vehicles/vehicles.zargo"
val ocl = "../../examples/vehicles/vehicles.ocl"
*)

(** OVERRRIDING **)
(* 
val zargo = "../../hol-ocl/examples/overriding/overriding.zargo"
val ocl="../../hol-ocl/examples/overriding/overriding.ocl"
val remP = []
*)
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
val model = removeOclLibrary model
val model = removePackages remP (model,xmi_assocs)

