
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


(* set debugging settings *)
val _ = Control.Print.printDepth:=20
val _ = Control.Print.printLength:=80
val _ = log_level:=2

(**************************************************************)
(************** IMPORT PLUGING STRUCTURES *********************)
(**************************************************************)

structure Plugin = Plugin_Constraint
structure Refine = Refine_Constraint(Plugin)


(**************************************************************)
(***** USE REGISTRY FOR GENERATING PROOF OBLIGATIONS  *********)
(**************************************************************)
(* There are several ways of getting a constraint:            
 *                                                            
 * i.)     using the default constraints:                     
 *		 
 *		   1.) using all the default constraints:
 *		       val supported_constraints
 *                 2.) using individual default Constraints: 
 *		       fun getConstraint con_name
 *       	       (e.g. getConstraint "liskov")
 *
 * ii.)    using the plugin constraints :
 *	
 *                 ATTENTION: Configure the constraint according 
 *                             to the info provided!
 *
 * iii.)   using sub constraints of the default constraints:
 *                 
 *                 The corresponding interface of the constraints
 *                 tells you which sub constraints can be exported.
 *                 All the operations with the type:
 *           
 *                 Rep.Model -> OclTerm list
 * 
 *                 Then you can create manually new sub constraints.
 *                 E.g.  weaken precondition:
 *                 structure Liskov = Liskov_Constraint(Base_Constraint)
 *		   val newC = 
 *		       {
 *			name="liskov",
 *			description="liskov substitution principle",
 *			generator=Liskov.weaken_precondition
 *		       }
 *
 *
 ***************************************************************)


(** get the static constraints *)
val l = getConstraint "liskov" supported_constraints
val dmc = getConstraint "data model consistency" supported_constraints

(** configure the plugin constraints *)
val x = Refine.setPackages ["AbstractSimpleChair01"] ["ConcreteSimpleChari01"];

(** get the plugin constraints *)
val r = Refine.getConstraint

(** build constraint list *)
val my_con_list = [l,dmc,r]


(** execute wfcpo-generator *)
(** REMARK:
 *  in the end the normal call will be:
 *
 *  analyze_model "stack.zargo" "stack.ocl" my_con_list
 *
 *  so the reset_po_nr() is not necessary any more! 
 **)

val _ = reset_po_nr()
val z = analyze_model_m my_con_list model
