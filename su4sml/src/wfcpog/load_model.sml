(* open structures *)

(* SU4SML *)
open Rep_Logger
open OclLibrary
open ModelImport
open Rep_Core


(* WFCPO-GEN *)
open WFCPOG_Library


(* set debugging settings *)
val _ = Control.Print.printDepth:=20
val _ = Control.Print.printLength:=30
(* 
val zargo = "../../../examples/meeting/Meeting.zargo"
val ocl = ""
*)

val zargo = "../../../hol-ocl/examples/overriding/overriding.zargo"
val ocl="../../../hol-ocl/examples/overriding/overriding.ocl"
val remP = []



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


