open WFCPOG_Registry
open WFCPOG_Constraint_Library

(* GET BASE CONSTRAINT *)
val tax = get_wfpo supported "tax"
val rfm_syn = get_wfpo supported "rfm_syn"

(* EXTEND BASE CONSTRAINT WITH ADDITIONAL DATA *)

(* MAXIMUM DEPTH OF THE INHERITANCE STRUCTURE *)
val md0 = TAX_Data.put ({key=9,max_depth=0}) tax
val md1 = TAX_Data.put ({key=9,max_depth=1}) tax
val md2 = TAX_Data.put ({key=9,max_depth=2}) tax
val md3 = TAX_Data.put ({key=9,max_depth=3}) tax
val md4 = TAX_Data.put ({key=9,max_depth=4}) tax
val md5 = TAX_Data.put ({key=9,max_depth=5}) tax
val md6 = TAX_Data.put ({key=9,max_depth=6}) tax
val md7 = TAX_Data.put ({key=9,max_depth=7}) tax
val md8 = TAX_Data.put ({key=9,max_depth=8}) tax

(* REFINEMENT OF PACKAGES *)
val sc1 = RFM_Data.put ({key=10,rfm_tuples=[(["AbstractSimpleChair01"],["ConcreteSimpleChair01"])]}) rfm_syn
val sc2 = RFM_Data.put ({key=10,rfm_tuples=[(["AbstractSimpleChair02"],["ConcreteSimpleChair02"])]}) rfm_syn
val sc3 = RFM_Data.put ({key=10,rfm_tuples=[(["AbstractSimpleChair01"],["ConcreteSimpleChair01"]),(["AbstractSimpleChair02"],["ConcreteSimpleChair02"])]}) rfm_syn
