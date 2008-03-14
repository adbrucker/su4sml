
(**************************************************************)
(****************** OPEN STRUCTURES ***************************)
(**************************************************************)

(* SU4SML *)
open OclLibrary
open ModelImport
open Rep_Core
open Ext_Library

(* WFCPO-GEN *)
open WFCPO_Library
open WFCPO_Naming
open WFCPO_Registry
open WFCPO_Constraint_Library

(* set debugging settings *)
val _ = Control.Print.printDepth:=20
val _ = Control.Print.printLength:=80
val _ = log_level:=2

val z = analyze_model [Liskov_all,DataModel_cmc,DataModel_smc] model 
val w = analyze_model [Constructor_all] model
val y = analyze_model supported_constraints model
