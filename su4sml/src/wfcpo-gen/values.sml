open Rep_Core
open Rep
open ModelImport
open Rep_OclType
open Ext_Library


open WFCPO_Library
open WFCPO_Registry
open Rep2String


(* impor model *)
val i_model = import "../../../examples/stack_manu/stack.zargo" "../../../examples/stack_manu/stack.ocl" [] 
val n_model = normalize_ext i_model
val model = ((#1 n_model@oclLib),(#2 n_model))

(* extract classifier *)
 
