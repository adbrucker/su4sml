open OclLibrary
open Rep_Logger
open WFCPOG
open WFCPOG_Registry
open WFCPOG_TestSuite


structure RFM_Data = WFCPOG_Refine_Constraint.WFCPOG_RFM_Data
structure TAX_Data = WFCPOG_Taxonomy_Constraint.WFCPOG_TAX_Data

(** ################# **)
(** WELLFORMED-CHECKS **)
(** ################# **)

(** INTERFACE CONSTRAINT **)
val wfc_inf = get_wfpo supported_wfs "wfc_inf"
val _ = trace high ("............. interface constraint loaded ...\n")

(** VISIBILITY CONSTRAINT **)
val wfc_vis = get_wfpo supported_wfs "wfc_vis"
val _ = trace high ("............. visibility constraint loaded ...\n")

(* TAXONOMY CONSTRAINT *)
val wfc_tax = get_wfpo supported_wfs "wfc_tax"
val _ = trace high ("............. taxonomy constraint loaded ...\n")

(** REFINEMENT CONSTRAINT **)
val wfc_rfm = get_wfpo supported_wfs "wfc_rfm"
val _ = trace high ("............. refinement constraints loaded ...\n")
val wfc_rfm_SC = rename_wfpo "wfc_rfm_SC" (RFM_Data.put ({key=10,rfm_tuples=[(["AbstractSimpleChair04"],["ConcreteSimpleChair02"])]}) wfc_rfm)
val _ = trace high ("............. refine wfc constraint loaded ...\n")



(** ################# **)
(** PROOF OBLIGATIONS **)
(** ################# **)

(** LISKOV CONSTRAINT **)
val po_lsk = get_wfpo supported_pos "po_lsk"
val _ = trace high ("............. liskov constraint loaded ...\n")

(** DATA MODEL CONSTRAINT **)
val po_cm = get_wfpo supported_pos "po_class_model"
val po_sm = get_wfpo supported_pos "po_strong_model"
val _ = trace high ("............. data model constraint loaded ...\n")

(** OPERATIONAL CONSTRAINT **)
(*
val om = WFCPOG_Registry.get_wfpo WFCPOG_Registry.supported "oper_model"
val _ = trace high ("............. operational constraint loaded ...\n")
*)

(** CONSTRUCTOR CONSTRAINT **)
val po_cstr = get_wfpo supported_pos "po_cstr"
val _ = trace high ("............. constructor constraints loaded ...\n")

(** COMMAND/QUERY CONSTRAINT **)
val po_cmd = get_wfpo supported_pos "po_cmd"
val po_quy = get_wfpo supported_pos "po_quy"
val _ = trace high ("............. command/query constraints loaded ...\n")

(** REFINEMENT CONSTRAINT **)
val po_rfm = get_wfpo supported_pos "po_rfm"
val _ = trace high ("............. refinement constraints loaded ...\n")
val po_rfm_SC = rename_wfpo "po_rfm_SC" (RFM_Data.put ({key=10,rfm_tuples=[(["AbstractSimpleChair04"],["ConcreteSimpleChair02"])]}) po_rfm)
val _ = trace high ("............. refine pog constraint loaded ...\n")

(* 
val md0 = rename_wfpo "md0" (TAX_Data.put ({key=8,max_depth=0}) tax)
val md1 = rename_wfpo "md1" (TAX_Data.put ({key=9,max_depth=1}) tax)
val md2 = rename_wfpo "md2" (TAX_Data.put ({key=9,max_depth=2}) tax)
val md3 = rename_wfpo "md3" (TAX_Data.put ({key=9,max_depth=3}) tax)
val md4 = rename_wfpo "md4" (TAX_Data.put ({key=9,max_depth=4}) tax)
val md5 = rename_wfpo "md5" (TAX_Data.put ({key=9,max_depth=5}) tax)
val md6 = rename_wfpo "md6" (TAX_Data.put ({key=9,max_depth=6}) tax)
val md7 = rename_wfpo "md7" (TAX_Data.put ({key=9,max_depth=7}) tax)
val md8 = rename_wfpo "md8" (TAX_Data.put ({key=9,max_depth=8}) tax)
*)

(*  
val wfs = [inf,vis,md0,md1,md2,md3,md4,md5,md6,md7,md8,rfm_SC_wfc]
val pos = [lsk,cm,sm,om,cmd,quy]
*)

(* 
val wfs = [rfm_SC_wfc]
val pos = [rfm_SC_pog]
*)

val wfs = []
val pos = [po_cm,po_sm]
