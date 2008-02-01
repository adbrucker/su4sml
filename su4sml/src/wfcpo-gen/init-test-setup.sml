(* open structures *)

(* SU4SML *)
open OclLibrary
open ModelImport
open Rep_Core
open Ext_Library

(* WFCPO-GEN *)
open WFCPO_Library
open WFCPO_Registry

(* set debugging settings *)
val _ = Control.Print.printDepth:=10
val _ = Control.Print.printLength:=30



val zargo = "../../../examples/calendar/calendar.zargo" 
val ocl="../../../examples/calendar/calendar.ocl"

(** impor model *)
val i_model = import zargo ocl [] 
val (clist,alist) = normalize_ext i_model
val model = ((clist@oclLib),(alist))


