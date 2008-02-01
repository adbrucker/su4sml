(* fun import xmifile oclfile ExcludePackages : string -> string -> string list -> Rep_Core.transform_model *)




open Rep_Core
open ModelImport
open Rep_Transform
open OclLibrary
open Ext_Library


(* adjust print depth *)
val _ = Control.Print.printDepth:=100
val _ = Control.Print.printLength:=100
val _ = log_level:=25

(*
use "constraints.sml"
open WFCPO_Constraints;

use "values.sml"
*)
(* 
val model = transformClassifiers_ext n_model
*)
