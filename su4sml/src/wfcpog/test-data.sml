open OclLibrary
open Rep_Logger
open WFCPOG
open WFCPOG_Registry
open WFCPOG_TestSuite


structure RFM_Data = WFCPOG_Refine_Constraint.WFCPOG_RFM_Data
structure TAX_Data = WFCPOG_Taxonomy_Constraint.WFCPOG_TAX_Data





(** LISKOV CONSTRAINT **)
val lsk = WFCPOG_Registry.get_wfpo WFCPOG_Registry.supported "lsk"
val _ = trace high ("............. liskov constraint loaded ...\n")

(** INTERFACE CONSTRAINT **)
val inf = WFCPOG_Registry.get_wfpo WFCPOG_Registry.supported "inf"
val _ = trace high ("............. interface constraint loaded ...\n")

(** DATA MODEL CONSTRAINT **)
val cm = WFCPOG_Registry.get_wfpo WFCPOG_Registry.supported "class_model"
val sm = WFCPOG_Registry.get_wfpo WFCPOG_Registry.supported "strong_model"
val _ = trace high ("............. data model constraint loaded ...\n")

(** OPERATIONAL CONSTRAINT **)
val om = WFCPOG_Registry.get_wfpo WFCPOG_Registry.supported "oper_model"
val _ = trace high ("............. operational constraint loaded ...\n")

(** COMMAND/QUERY CONSTRAINT **)
val cmd = WFCPOG_Registry.get_wfpo WFCPOG_Registry.supported "cmd"
val quy = WFCPOG_Registry.get_wfpo WFCPOG_Registry.supported "quy"
val _ = trace high ("............. command/query constraints loaded ...\n")

(** VISIBILITY CONSTRAINT **)
val vis = WFCPOG_Registry.get_wfpo WFCPOG_Registry.supported "vis"
val _ = trace high ("............. visibility constraint loaded ...\n")

(** REFINEMENT CONSTRAINT **)
val rfm_wfc = WFCPOG_Registry.get_wfpo WFCPOG_Registry.supported "rfm_wfc"
val _ = trace high ("............. refinement constraints loaded ...\n")
val rfm_pog = WFCPOG_Registry.get_wfpo WFCPOG_Registry.supported "rfm_pog"
val _ = trace high ("............. refinement constraints loaded ...\n")

(* TAXONOMY CONSTRAINT *)
val tax = WFCPOG_Registry.get_wfpo WFCPOG_Registry.supported "tax"
val _ = trace high ("............. taxonomy constraint loaded ...\n")

val rfm_SC_wfc = WFCPOG_Registry.rename_wfpo "rfm_SC_wfc" (RFM_Data.put ({key=10,rfm_tuples=[(["AbstractSimpleChair01"],["ConcreteSimpleChair01"])]}) rfm_wfc)
val rfm_SC_pog = WFCPOG_Registry.rename_wfpo "rfm_SC_pog" (RFM_Data.put ({key=10,rfm_tuples=[(["AbstractSimpleChair01"],["ConcreteSimpleChair01"])]}) rfm_pog)
 
val md0 = WFCPOG_Registry.rename_wfpo "md0" (TAX_Data.put ({key=9,max_depth=0}) tax)
val md1 = WFCPOG_Registry.rename_wfpo "md1" (TAX_Data.put ({key=9,max_depth=1}) tax)
val md2 = WFCPOG_Registry.rename_wfpo "md2" (TAX_Data.put ({key=9,max_depth=2}) tax)
val md3 = WFCPOG_Registry.rename_wfpo "md3" (TAX_Data.put ({key=9,max_depth=3}) tax)
val md4 = WFCPOG_Registry.rename_wfpo "md4" (TAX_Data.put ({key=9,max_depth=4}) tax)
val md5 = WFCPOG_Registry.rename_wfpo "md5" (TAX_Data.put ({key=9,max_depth=5}) tax)
val md6 = WFCPOG_Registry.rename_wfpo "md6" (TAX_Data.put ({key=9,max_depth=6}) tax)
val md7 = WFCPOG_Registry.rename_wfpo "md7" (TAX_Data.put ({key=9,max_depth=7}) tax)
val md8 = WFCPOG_Registry.rename_wfpo "md8" (TAX_Data.put ({key=9,max_depth=8}) tax)

(*  
val wfs = [inf,vis,md0,md1,md2,md3,md4,md5,md6,md7,md8,rfm_SC_wfc]
val pos = [lsk,cm,sm,om,cmd,quy]
*)

val wfs = [rfm_SC_wfc]
val pos = [rfm_SC_pog]
