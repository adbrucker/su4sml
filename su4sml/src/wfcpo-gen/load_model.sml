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

val zargo = "../../../examples/ebank/ebank.zargo"
val ocl="../../../examples/ebank/ebank.ocl"

(** import model *)
val i_model = import zargo ocl [] 
val (clist,alist) = normalize_ext i_model
val model = ((clist@oclLib),(alist))
val classifiers = removeOclLibrary clist
