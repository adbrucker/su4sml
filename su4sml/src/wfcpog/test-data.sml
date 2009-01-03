

open OclLibrary
open WFCPOG
open WFCPOG_Registry
open WFCPOG_TestSuite


structure RFM_Data = WFCPOG_Refine_Constraint.WFCPOG_RFM_Data
structure TAX_Data = WFCPOG_Taxonomy_Constraint.WFCPOG_TAX_Data


(** ################# **)
(** WELLFORMED-CHECKS **)
(** ################# **)

(** INTERFACE CONSTRAINT **)
val wfc_inf_nameclashes = get_wfpo supported_wfs "wfc_inf_nameclashes"
val wfc_inf_stereotypes = get_wfpo supported_wfs "wfc_inf_stereotypes"
val wfc_inf_all = get_wfpo supported_wfs "wfc_inf_all"
val _ = trace high ("............. interface constraint loaded ...\n")

(** VISIBILITY CONSTRAINT **)
val wfc_vis_class = get_wfpo supported_wfs "wfc_vis_class"
val wfc_vis_inheritance = get_wfpo supported_wfs "wfc_vis_inheritance"
val wfc_vis_runtime = get_wfpo supported_wfs "wfc_vis_runtime"
val wfc_vis_design_by_contract = get_wfpo supported_wfs "wfc_vis_design_by_contract"
val wfc_vis_all = get_wfpo supported_wfs "wfc_vis_all"
val _ = trace high ("............. visibility constraint loaded ...\n")

(* TAXONOMY CONSTRAINT *)
val wfc_tax = get_wfpo supported_wfs "wfc_tax"
val wfc_tax_5 = rename_wfpo "wfc_tax_5" (TAX_Data.put ({key=2,max_depth=5}) wfc_tax)
val _ = trace high ("............. taxonomy constraint loaded ...\n")

(** REFINEMENT CONSTRAINT **)
val wfc_rfm = get_wfpo supported_wfs "wfc_rfm"
val _ = trace high ("............. refinement constraints loaded ...\n")
val wfc_rfm_SC = rename_wfpo "wfc_rfm_SC" (RFM_Data.put ({key=10,rfm_tuple=(["AbstractSimpleChair04"],["ConcreteSimpleChair02"])}) wfc_rfm)
val wfc_rfm_SR= rename_wfpo "wfc_rfm_SR" (RFM_Data.put ({key=10,rfm_tuple=(["AbstractOverriding"],["ConcreteOverriding"])}) wfc_rfm)
val _ = trace high ("............. refine wfc constraint loaded ...\n")

(** QUERY CONSISTENCY **)
val wfc_quy_strong = get_wfpo supported_wfs "wfc_quy_strong"
val wfc_quy_weak = get_wfpo supported_wfs "wfc_quy_weak"


(** ################# **)
(** PROOF OBLIGATIONS **)
(** ################# **)

(** LISKOV CONSTRAINT **)
val po_lsk_pre = get_wfpo supported_pos "po_lsk_pre"
val po_lsk_post = get_wfpo supported_pos "po_lsk_post"
val po_lsk_inv = get_wfpo supported_pos "po_lsk_inv"
val po_lsk_all = get_wfpo supported_pos "po_lsk_all"
val _ = trace high ("............. liskov constraint loaded ...\n")

(** DATA MODEL CONSTRAINT **)
val po_cm = get_wfpo supported_pos "po_class_model"
val po_sm = get_wfpo supported_pos "po_strong_model"
val _ = trace high ("............. data model constraint loaded ...\n")

(** OPERATIONAL CONSTRAINT **)
val po_om = WFCPOG_Registry.get_wfpo WFCPOG_Registry.supported "po_oper_model"
val _ = trace high ("............. operational constraint loaded ...\n")

(** CONSTRUCTOR CONSTRAINT **)
val po_cstr_post = get_wfpo supported_pos "po_cstr_post"
val po_cstr_attribute = get_wfpo supported_pos "po_cstr_attribute"
val po_cstr_all = get_wfpo supported_pos "po_cstr_all"
val _ = trace high ("............. constructor constraints loaded ...\n")

(** COMMAND/QUERY CONSTRAINT **)
(* 
val po_cmd = get_wfpo supported_pos "po_"
val po_quy = get_wfpo supported_pos "po_quy"
*)
val _ = trace high ("............. command/query constraints loaded ...\n")

(** REFINEMENT CONSTRAINT **)
val po_rfm = get_wfpo supported_pos "po_rfm"
val _ = trace high ("............. refinement constraints loaded ...\n")
val po_rfm_SC = rename_wfpo "po_rfm_SR" (RFM_Data.put ({key=10,rfm_tuple=(["AbstractSimpleChair04"],["ConcreteSimpleChair02"])}) po_rfm)
val _ = trace high ("............. refine pog constraint loaded ...\n")
val po_rfm_SR = rename_wfpo "po_rfm_SR" (RFM_Data.put ({key=10,rfm_tuple=(["AbstractOverriding"],["ConcreteOverriding"])}) po_rfm)
val _ = trace high ("............. refine pog constraint loaded ...\n")

 
(* ALL CONSTRAINTS:  *)

val wfc = 
[wfc_inf_nameclashes,wfc_inf_stereotypes,wfc_inf_all,wfc_vis_class,wfc_vis_inheritance,wfc_vis_runtime,wfc_vis_design_by_contract,wfc_vis_all,wfc_tax]
val pos =
[po_lsk_pre,po_lsk_post,po_lsk_inv,po_cm,po_sm,po_om,po_cstr_post,po_cstr_attribute,po_cstr_all]

(*
val wfcs = [wfc_vis_runtime]
val pos = [po_lsk_inv]
*)
