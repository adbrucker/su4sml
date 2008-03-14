(* open structures *)

(* SU4SML *)
open OclLibrary
open ModelImport
open Rep_Core
open Ext_Library

(* WFCPO-GEN *)
open WFCPO_Library


(* set debugging settings *)
val _ = Control.Print.printDepth:=20
val _ = Control.Print.printLength:=30
(*
val zargo = "../../../stack_manu/stack.zargo"
val ocl = "../../../stack_manu/stack.ocl"
*)

val zargo = "../../../examples/ebank/ebank.zargo"
val ocl="../../../examples/ebank/ebank.ocl"

(** import model *)
val i_model = import zargo ocl [] 
val (clist,alist) = normalize_ext i_model
val model = ((clist@oclLib),(alist))
val classifiers = removeOclLibrary clist


