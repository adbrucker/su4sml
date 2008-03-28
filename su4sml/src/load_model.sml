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
*)
(* 
(** SIMPLE **)
val zargo = "../../examples/simple/simple.zargo"
val ocl="../../examples/simple/simple.ocl"
*)
(* 
(** DIGRAPH **)
val zargo = "../../examples/digraph/digraph.zargo"
val ocl = "../../examples/digraph/digraph.ocl"
*)
(*
(** VEHICLES **)
val zargo = "../../examples/vehicles/vehicles.zargo"
val ocl = "../../examples/vehicles/vehicles.ocl"
*)


(** import model *)
val i_model = import zargo ocl remP
val (clist,alist) = normalize_ext i_model
val model = ((clist@oclLib),(alist))
val classifiers = removeOclLibrary clist
