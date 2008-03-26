(* open structures *)

(* SU4SML *)
open library
open OclLibrary
open ModelImport
open Rep_Core




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



(** EBANK **)
(*
val zargo = "../../examples/ebank/ebank.zargo"
val ocl="../../examples/ebank/ebank.ocl"
*)
(** import model *)
val i_model = import zargo ocl remP
val (clist,alist) = normalize_ext i_model
val model = ((clist@oclLib),(alist))
val classifiers = removeOclLibrary clist
