open OclLibrary
open WFCPOG
open WFCPOG_Registry
open WFCPOG_TestSuite

(** LISKOV CONSTRAINT **)
val lsk = WFCPOG_Registry.get_wfpo WFCPOG_Registry.supported "lsk"

(** INTERFACE CONSTRAINT **)
val inf = WFCPOG_Registry.get_wfpo WFCPOG_Registry.supported "inf"

(** DATA MODEL CONSTRAINT **)
val cm = WFCPOG_Registry.get_wfpo WFCPOG_Registry.supported "class_model"
val sm = WFCPOG_Registry.get_wfpo WFCPOG_Registry.supported "strong_model"

(** OPERATIONAL CONSTRAINT **)
val om = WFCPOG_Registry.get_wfpo WFCPOG_Registry.supported "oper_model"

(** COMMAND/QUERY CONSTRAINT **)
val cmd = WFCPOG_Registry.get_wfpo WFCPOG_Registry.supported "cmd"
val quy = WFCPOG_Registry.get_wfpo WFCPOG_Registry.supported "quy"

(** VISIBILITY CONSTRAINT **)
val vis = WFCPOG_Registry.get_wfpo WFCPOG_Registry.supported "vis"

(** REFINEMENT CONSTRAINT **)
val rfm_syn = WFCPOG_Registry.get_wfpo WFCPOG_Registry.supported "rfm_syn"

(* TAXONOMY CONSTRAINT *)
val tax = WFCPOG_Registry.get_wfpo WFCPOG_Registry.supported "tax"



val md0 = WFCPOG_Registry.rename_wfpo "md0" (WFCPOG_Registry.TAX_Data.put ({key=9,max_depth=0}) tax)
val md1 = WFCPOG_Registry.rename_wfpo "md1" (WFCPOG_Registry.TAX_Data.put ({key=9,max_depth=1}) tax)
val md2 = WFCPOG_Registry.rename_wfpo "md2" (WFCPOG_Registry.TAX_Data.put ({key=9,max_depth=2}) tax)
val md3 = WFCPOG_Registry.rename_wfpo "md3" (WFCPOG_Registry.TAX_Data.put ({key=9,max_depth=3}) tax)
val md4 = WFCPOG_Registry.rename_wfpo "md4" (WFCPOG_Registry.TAX_Data.put ({key=9,max_depth=4}) tax)
val md5 = WFCPOG_Registry.rename_wfpo "md5" (WFCPOG_Registry.TAX_Data.put ({key=9,max_depth=5}) tax)
val md6 = WFCPOG_Registry.rename_wfpo "md6" (WFCPOG_Registry.TAX_Data.put ({key=9,max_depth=6}) tax)
val md7 = WFCPOG_Registry.rename_wfpo "md7" (WFCPOG_Registry.TAX_Data.put ({key=9,max_depth=7}) tax)
val md8 = WFCPOG_Registry.rename_wfpo "md8" (WFCPOG_Registry.TAX_Data.put ({key=9,max_depth=8}) tax)


val wfs = [inf,vis,md0,md1,md2,md3,md4,md5,md6,md7,md8]
val pos = [lsk,cm,sm,om,cmd,quy]
