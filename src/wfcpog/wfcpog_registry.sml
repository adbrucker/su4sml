(*****************************************************************************
 * su4sml --- an SML repository for managing (Secure)UML/OCL models
 *             http://projects.brucker.ch/su4sml/
 *                                                                            
 *  ---
 * This file is part of su4sml.
 *
 * Copyright (c) 2008-2009 Achim D. Brucker, Germany
 *
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *
 *     * Redistributions in binary form must reproduce the above
 *       copyright notice, this list of conditions and the following
 *       disclaimer in the documentation and/or other materials provided
 *       with the distribution.
 *
 *     * Neither the name of the copyright holders nor the names of its
 *       contributors may be used to endorse or promote products derived
 *       from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 ******************************************************************************)
(* $Id: wfcpog_registry.sml 7273 2008-02-18 07:18:05Z brucker $ *)

signature WFCPOG_REGISTRY = 
sig
    (**
     * Get information about the WFCPO-Generator
     *)
    (** README *)
    val README             : unit -> unit
    (** Infos about execution of wfcs and the supported wfc library.*)
    val info_wfs          : unit -> unit
    (** Infos about the generation of pos and the supported pos library.*)
    val info_pos           : unit -> unit

    (**
     * Customize your own wfpo list:
     *)
    (** Customized wfc and po list, at the beginning empty.*)
    val wfpos              : WFCPOG.wfpo list ref
    (** Add wfpo to customized list.*)
    val add_wfpo           : WFCPOG.wfpo -> unit
    (** Delete wfpo from customized list.*)
    val del_wfpo           : WFCPOG.wfpo_id -> unit


    (** All the supported well-formedness checks. *)					       
    val supported_wfs      : WFCPOG.wfpo list
    (** All the supported proof obligations. *)
    val supported_pos      : WFCPOG.wfpo list
    (** All the supported well-formedness checks and proof obligations together.*)
    val supported          : WFCPOG.wfpo list 

    (** Checks if the wfpo is a well-formedness check. *)
    val is_wfc             : WFCPOG.wfpo -> bool
    (** Checks if the wfpo is a proof obligation.*)
    val is_pog             : WFCPOG.wfpo -> bool

    (** Checks if a given name is a supported wfc.*)
    val wf_is_supported_id : WFCPOG.wfpo_id -> bool
    (** Checks if a given name is a supported po.*)
    val po_is_supported_id : WFCPOG.wfpo_id -> bool
    
    val rename_wfpo        : string -> WFCPOG.wfpo -> WFCPOG.wfpo
    (**  Get a wfpo from a given list by the id string.*)
    val get_wfpo           : WFCPOG.wfpo list -> WFCPOG.wfpo_id -> WFCPOG.wfpo
							 
    (** Execute a list of wfcs.*)
    val check_wfcs         : Rep.Model -> WFCPOG.wfpo list -> (WFCPOG.wfpo * bool) list

    (** Execute all recommended wfcs.*)
    val check_recommended_wfcs : Rep.Model -> (WFCPOG.wfpo * bool) list


    (** Generate pos for a list of wfpos.*)
    val generate_pos       : Rep.Model -> WFCPOG.wfpo list -> (WFCPOG.wfpo * (Rep_OclType.Path * Rep_OclTerm.OclTerm) list) list

    (** Generate all recommended pos.*)							      
    val generate_recommended_pos : Rep.Model -> (WFCPOG.wfpo * (Rep_OclType.Path * Rep_OclTerm.OclTerm) list) list

    val generate_void_po         : WFCPOG.wfpo -> Rep.Model -> (Rep_OclType.Path * Rep_OclTerm.OclTerm) list
    val generate_void_wfc        : WFCPOG.wfpo -> Rep.Model -> bool
    (** Argument Wrappers *)
    (** Create wfpo for wfc max_depth.*)
    val create_wfc_tax     : int -> WFCPOG.wfpo
    exception WFCPOG_RegistryError of string
    exception WFCPOG_MethodologyError of string
end


structure WFCPOG_Registry :WFCPOG_REGISTRY  = 
struct

exception WFCPOG_RegistryError of string
exception WFCPOG_MethodologyError of string

open WFCPOG
open Datatab

val wfpos = ref ([]:(WFCPOG.wfpo list))


fun wfcpog_of id = hd  (List.filter (fn m => WFCPOG.id_of m = id) (!wfpos))


fun add_wfpo wfpo = ((wfpos := [wfpo]@(!wfpos));())

fun del_wfpo wfpo_id = ((wfpos := List.filter (fn w => not ((WFCPOG.id_of w) = (wfpo_id)) ) 
			   (!wfpos));())

fun get_wfpo [] x = 
    let
	val _ = Logger.warn ("No ID = " ^ x ^ " found in given list!\n")
    in 
	raise WFCPOG_RegistryError ("No ID = " ^ x ^ " found in given list!\n")
    end
  | get_wfpo (h::tail) x = 
    if (id_of h = x) 
    then h
    else get_wfpo tail x 


fun generate_void_po wfpo model = []

fun generate_void_wfc wfpo model = true

fun rename_wfpo new_name (WFCPOG.WFPO{identifier=identifier,name=name,description=description,recommended=recommended,depends=depends,recommends=recommends,apply=apply,data=data}:WFCPOG.wfpo) =
    WFCPOG.WFPO{
      identifier=new_name,
      name=name,
      description=description,
      recommended=recommended,
      depends=depends,
      recommends=recommends,
      apply=apply,
      data=data
    }


val tax_workaround = 
    WFCPOG.WFPO{
     identifier      = "wfc_tax", 
     name            = "WFC Taxonomy Consistency",
     description     = "Checks if the inheritance hierarchy is not deeper than n (default value n=5)\n",
     recommended     = true,
     depends         = [],
     recommends      = [],
     apply           = WFCPOG.WFC(WFCPOG_Taxonomy_Constraint.check_depth),
     data            = Datatab.empty 
    }
		       
val supported_wfs = [ 
    (* SUPPORTED IN ISABLLE *)
    WFCPOG.WFPO{
     identifier      = "wfc_inf_stereotypes",
     name            = "WFC Interface Consistency consistent stereotypes (subconstraint)",
     description     = "Checks if all operations of an interface don't have the stereotypes 'create' or 'destroy'.\n",
     recommended     = false,
     depends         = [],
     recommends      = [],
     apply           = WFCPOG.WFC(WFCPOG_Interface_Constraint.check_stereotypes),
     data            = Datatab.empty
    },
    (* SUPPORTED IN ISABELLE *)
    WFCPOG.WFPO{
     identifier      = "wfc_inf_nameclashes",
     name            = "WFC Interface Consistency no nameclashes (subconstraint)",
     description     = "Checks for classes inheriting from more than one interface that there are no nameclashes.\n",
     recommended     = false,
     depends         = [],
     recommends      = [],
     apply           = WFCPOG.WFC(WFCPOG_Interface_Constraint.check_nameclashes),
     data            = Datatab.empty
    },
    (* SUPPORTED IN ISABLLE *)
    WFCPOG.WFPO{
     identifier      = "wfc_inf_all",
     name            = "WFC Interface Consistency (complete)",
     description     = "Checking of two subconstraints: \n wfc_inf_ster: Checks if all operations of an interface don't have the stereotypes 'create' or 'destroy'. \n wfc_inf_name : Checks for classes inheriting from more than one interface that there are no nameclashes.\n",
     recommended     = true,
     depends         = ["wfc_inf_stereotypes","wfc_inf_nameclashes"],
     recommends      = [],
     apply           = WFCPOG.WFC(generate_void_wfc),
     data            = Datatab.empty
    },
    (* SUPPORTED IN ISABLLE *)
    WFCPOG.WFPO{
     identifier      = "wfc_vis_class", 
     name            = "WFC Visibility Consistency (class)",
     description     = "Checks if the visibility of the class is at least as visible as the most visible member.\n",
     recommended     = true,
     depends         = [],
     recommends      = [],
     apply           = WFCPOG.WFC(WFCPOG_Visibility_Constraint.model_entity_consistency),
     data = Datatab.empty
    },
    (* SUPPORTED IN ISABLLE *)
    WFCPOG.WFPO{
     identifier      = "wfc_vis_inheritance", 
     name            = "WFC Visibility Consistency (inheritance)",
     description     = "Checks if the modificators of overriden features are maintained in the subclasses.\n",
     recommended     = true,
     depends         = [],
     recommends      = [],
     apply           = WFCPOG.WFC(WFCPOG_Visibility_Constraint.model_inheritance_consistency),
     data = Datatab.empty
    },
    (* SUPPORTED IN ISABLLE *)
    WFCPOG.WFPO{
     identifier      = "wfc_vis_runtime", 
     name            = "WFC Visibility Consistency (runtime)",
     description     = "Runtime checking/enforcement in mind:\n pre-condition, post-conditions, invariants are shall only contain \n calls to visible features (i.e., public features of other classes, \n package features of other classes within the same package, \n protected features of superclasses, and own private features).\n",
     recommended     = false,
     depends         = [],
     recommends      = [],
     apply           = WFCPOG.WFC(WFCPOG_Visibility_Constraint.constraint_check_by_runtime_consistency),
     data = Datatab.empty
    },
    (* SUPPORTED IN ISABLLE *)
    WFCPOG.WFPO{
     identifier      = "wfc_vis_design_by_contract", 
     name            = "WFC Visibility Consistency (design_by_contract)",
     description     = "Design by contract in mind: \n Here, clients (callers) should be able to check/establish the pre-condition of operations: \n pre-conditions should only contain feature calls that are at least as visible as \n the operation itself.",
     recommended     = false,
     depends         = [],
     recommends      = [],
     apply           = WFCPOG.WFC(WFCPOG_Visibility_Constraint.constraint_design_by_contract_consistency),
     data = Datatab.empty
    },
    (* SUPPORTED IN ISABLLE *)
    WFCPOG.WFPO{
     identifier      = "wfc_vis_all", 
     name            = "WFC Visibility Consistency (complete)",
     description     = "Three checks forced: \n wfc_vis_runtime \n wfc_vis_class \n wfc_vis_inheritance",
     recommended     = false,
     depends         = ["wfc_vis_class","wfc_vis_inheritance","wfc_vis_runtime","wfc_vis_design_by_contract"],
     recommends      = [],
     apply           = WFCPOG.WFC(generate_void_wfc),
     data = Datatab.empty
    },
     
     (* TODO: insert this constraint for having a default value.                                       *)
     (*                                                                                                *)
     (*    (WFCPOG_Taxonomy_Constraint.WFCPOG_TAX_Data.put ({key=9,max_depth=5}) tax_workaround)       *)
     (*                                                                                                *)
    (* SUPPORTED IN ISABLLE *)
    WFCPOG.WFPO{
     identifier      = "wfc_tax", 
     name            = "WFC Taxonomy Consistency",
     description     = "Checks if the inheritance hierarchy is not deeper than n (default value n=5)\n",
     recommended     = true,
     depends         = [],
     recommends      = [],
     apply           = WFCPOG.WFC(WFCPOG_Taxonomy_Constraint.check_depth),
     data            = Datatab.empty 
    }
    , 
    (* NOT SUPPORTED IN ISABLLE (is not needed)*)
    WFCPOG.WFPO{ 
     identifier      = "wfc_rfm", 
     name            = "WFC OO Refinement",  
     description     = "Checks if public classes of aboriginal package are also public in new package\n",
     recommended     = false,
     depends         = [],
     recommends      = [],
     apply           = WFCPOG.WFC(WFCPOG_Refine_Constraint.check_syntax),
     data            = Datatab.empty
    },
    (* SUPPORTED IN ISABLLE *)
    WFCPOG.WFPO{ 
     identifier      = "wfc_cstr_override", 
     name            = "WFC Constructor Consistency",  
     description     = "Checks if a given class overrrides all old creators\n.",
     recommended     = true,
     depends         = [],
     recommends      = [],
     apply           = WFCPOG.WFC(WFCPOG_Constructor_Constraint.override_old_creators),
     data            = Datatab.empty
    },
    (* SUPPORTED IN ISABLLE *)
    WFCPOG.WFPO{ 
     identifier      = "wfc_quy_strong", 
     name            = "WFC Command Query Constraint",  
     description     = "Checks if all ocl formualas just contains operationcalls which are isQuery.\n",
     recommended     = false,
     depends         = [],
     recommends      = [],
     apply           = WFCPOG.WFC(WFCPOG_Command_Query_Constraint.strong_is_query),
     data            = Datatab.empty
    },
    (* SUPPORTED IN ISABLLE *)
    WFCPOG.WFPO{ 
     identifier      = "wfc_quy_weak", 
     name            = "WFC Command Query Constraint",  
     description     = "Checks if operations declared to be isQuery just contains ocl formulas which contain just operationcalls which are isQuery.\n",
     recommended     = false,
     depends         = [],
     recommends      = [],
     apply           = WFCPOG.WFC(WFCPOG_Command_Query_Constraint.weak_is_query),
     data            = Datatab.empty
    }
]

val supported_pos = [
    WFCPOG.WFPO{
     identifier      = "po_lsk_pre",
     name            = "Liskov weaken_precondition",
     description     = "Generate Proof Obligations for weaking precondition",
     recommended     = false,
     depends         = [],
     recommends      = [],
     apply           = WFCPOG.POG(WFCPOG_Liskov_Constraint.weaken_precondition),
     data            = Datatab.empty
    }, 
    WFCPOG.WFPO{
     identifier      = "po_lsk_post", 
     name            = "Liskov strengthen_postcondition",   (* short description (for output) *)
     description     = "Generate Proof Obligations following from strengthen_postcondition",
     recommended     = false,
     depends         = [],
     recommends      = [],
     apply           = WFCPOG.POG(WFCPOG_Liskov_Constraint.strengthen_postcondition),
     data            = Datatab.empty
    },
    WFCPOG.WFPO{
     identifier      = "po_lsk_inv", 
     name            = "Liskov conjugate_invariants", 
     description     = "Generate Proof Obligations following from conjugate_invariants",
     recommended     = false,
     depends         = [],
     recommends      = [],
     apply           = WFCPOG.POG(WFCPOG_Liskov_Constraint.conjugate_invariants),
     data            = Datatab.empty
    },
    WFCPOG.WFPO{
     identifier      = "po_lsk_all", 
     name            = "Liskov's subtitution principle", 
     description     = "Generate Proof Obligations following the idea from Barbara Liskov",
     recommended     = true,
     depends         = ["po_lsk_pre","po_lsk_post","po_lsk_inv"],
     recommends      = [],
     apply           = WFCPOG.POG(generate_void_po),
     data            = Datatab.empty
    }, 
    WFCPOG.WFPO{
     identifier      = "po_rfm", 
     name            = "OO Refinement",
     description     = "Generate Proof Obligations for OO data refinement",
     recommended     = true,
     depends         = ["wfc_rfm"],
     recommends      = [],
     apply           = WFCPOG.POG(WFCPOG_Refine_Constraint.generate_pos),
     data            = Datatab.empty
    },(*
    WFCPOG.WFPO{
     identifier      = "po_cmd", 
     name            = "Query Command Constraint",
     description     = "Check if operations which are declared as command are really commands",
     recommended     = false,
     depends         = [],
     recommends      = [],
     apply           = WFCPOG.POG(WFCPOG_Command_Query_Constraint.semantics),
     data = Datatab.empty
    },*)
    WFCPOG.WFPO{
     identifier      = "po_class_model", (* identifier                     *) 
     name            = "Data model consistency: class model",
     description     = "Data model consistency: a classes should be able to be instantiated from a state.",
     recommended     = false,
     depends         = [],
     recommends      = [],
     apply           = WFCPOG.POG(WFCPOG_Data_Model_Consistency_Constraint.class_model_consistency),
     data = Datatab.empty
    },
    WFCPOG.WFPO{
     identifier      = "po_strong_model", (* identifier                     *) 
     name            = "Data model consistency: strong model",
     description     = "Data model consistency; all classes should be able to be instantiated from a state.",
     recommended     = false,
     depends         = [],
     recommends      = [],
     apply           = WFCPOG.POG(WFCPOG_Data_Model_Consistency_Constraint.strong_model_consistency),
     data = Datatab.empty
    },
    WFCPOG.WFPO{ 
     identifier      = "po_cstr_post", 
     name            = "PO Constructor Consistency post implies invariants(subconstraint)",  
     description     = "Checks if the postcondition of any constructor operation imples the class' invariant.\n",
     recommended     = true,
     depends         = ["wfc_cstr_override"],
     recommends      = [],
     apply           = WFCPOG.POG(WFCPOG_Constructor_Constraint.post_implies_invariant),
     data            = Datatab.empty
    },
    WFCPOG.WFPO{ 
     identifier      = "po_cstr_attribute", 
     name            = "PO Constructor Consistency attributes are inited(subconstraint)",  
     description     = "Checks if after the execution of any constructor operation all the attributes are initialized.\n",
     recommended     = false,
     depends         = [],
     recommends      = [],
     apply           = WFCPOG.POG(WFCPOG_Constructor_Constraint.force_initialize_attributes),
     data            = Datatab.empty
    },
    WFCPOG.WFPO{ 
     identifier      = "po_cstr_all", 
     name            = "PO Constructor Consistency (complete)",
     description     = "Checks two subconstraints: \n cstr_post : Checks if after the execution of any constructor operation all the attributes are initialized.\n cstr_attr: Checks if after the execution of any constructor operation all the attributes are initialized.\n",
     recommended     = false,
     depends         = ["po_cstr_post","po_cstr_attribute"],
     recommends      = [],
     apply           = WFCPOG.POG(generate_void_po),
     data            = Datatab.empty
    }
    ,
    WFCPOG.WFPO{
     identifier      = "po_oper_model", (* identifier                     *) 
     name            = "Operational model consistency",
     description     = "Inserts the additional proof obligations resulting from the operational model consistency.",
     recommended     = false,
     depends         = [],
     recommends      = [],
     apply           = WFCPOG.POG(WFCPOG_Operational_Constraint.implementable_operation),
     data = Datatab.empty
    }
]

val supported = supported_wfs@supported_pos


fun is_wfc (WFCPOG.WFPO wfpo) = case #apply wfpo of 
				  WFCPOG.WFC _ => true
				| _            => false

fun is_pog (WFCPOG.WFPO wfpo) = case #apply wfpo of 
				  WFCPOG.POG _ => true
				| _            => false

fun set_data (new_data:Object.T table) (WFCPOG.WFPO{identifier,name,description,recommended,depends,recommends,apply,data}) =  
    WFCPOG.WFPO{ 
      identifier = identifier, 
      name = name, 
      description=description, 
      recommended=recommended, 
      depends=depends, 
      recommends=recommends, 
      apply=apply, 
      data=new_data 
    } 				



fun check_wfc model wfc = 
    let
	val _ = Logger.info ("WFCPOG_Registry.check_wfc\n")
	val _ = Logger.debug1 (name_of wfc ^ ".............." ^ "\n")
	val res = 
	    case (WFCPOG.apply_of wfc) of
		WFCPOG.WFC(a) => (wfc,a wfc model)
	      | x => raise WFCPOG_RegistryError ("A assumed wfc " ^ (name_of wfc) ^ " is not a wfc!\n")
	val _ = Logger.info ("WFCPOG_Registry.check_wfc\n")
    in
	res
    end
    
fun check_wfcs model wfcs =  
    let
	val _ = Logger.info ("WFCPOG_Registry.check_wfcs\n")
	val res =     List.concat (map (fn (a as WFCPOG.WFPO{identifier,name,description,recommended,depends,recommends,apply,data}:WFCPOG.wfpo) =>
					   if (depends = [])
					   then [(check_wfc model a)]
					   else
					       let
						   val depending_wfpos = List.map (fn m => (set_data data (get_wfpo supported m))) depends 
						   val depending_wfcs = List.filter (fn b => 
											case (WFCPOG.apply_of b) of
											    WFCPOG.WFC(x) => true
											  | WFCPOG.POG(x) => false) depending_wfpos
						   val depending_pos = List.filter (fn b =>
										       case (WFCPOG.apply_of b) of
											   WFCPOG.WFC(x) => false
											 | WFCPOG.POG(x) => true) depending_wfpos
					       in
						   if (List.length depending_pos <> 0)
						   then raise WFCPOG_RegistryError ("A wellformedness check has a proof obligation marked as depending. But this is not allowed! \n\nCHANGE WFCPOG_Registry.supported_wfs ENTRY(IES)!!!")
						   else (List.map (check_wfc model) depending_wfcs)@[(check_wfc model a)]
					       end) wfcs)
	val _ = Logger.info ("WFCPOG_Registry.check_wfcs\n")
    in
	res
    end

fun generate_po model po =  
    let
	val _ = Logger.info ("WFCPOG_Registry.generate_po\n")
	val _ = Logger.debug1 (name_of po ^ " ...............\n")
	val res = 
	    case (WFCPOG.apply_of po) of
		WFCPOG.POG (a) => (po,a po model)
	      | x  => raise WFCPOG_RegistryError ("A assumed po " ^ (name_of po) ^ " is not a po!\n")
	val _ =Logger.info ("WFCPOG_Registry.generate_po\n")
    in
	res
    end

fun generate_pos model pos =
    let
	val _ = Logger.info ("WFCPOG_Registry.generate_pos\n")
	val res = 
	    List.concat (map (fn (a as (WFCPOG.WFPO{identifier,name,description,recommended,depends,recommends,apply,data}:WFCPOG.wfpo)) => 
				 if (depends = [])
				 then [(generate_po model a)]
				 else
				     let
					 val depending_wfpos = List.map (fn m => (set_data data (get_wfpo supported m))) depends
					 val depending_wfcs = List.filter (fn b =>
									      case (WFCPOG.apply_of b)of
										  WFCPOG.WFC(x) => true
										| WFCPOG.POG(x) => false) depending_wfpos
					 val depending_pos = List.filter (fn b =>
									     case (WFCPOG.apply_of b) of
										 WFCPOG.WFC(x) => false
									       | WFCPOG.POG(x) => true) depending_wfpos
					 val check = List.map (check_wfc model) depending_wfcs
				     in
					 if (List.all (fn (wfc,x) => x = true) check)
					 then (List.map (generate_po model) depending_pos)@[(generate_po model a)]
					 else (* doesn't matter, because WFCPOG_WFC_FailedException is returned *) 
						  []
				     end) pos)
	val _ = Logger.info ("WFCPOG_Registry.generate_pos\n")
    in
	res
    end
    
fun create_wfc_tax i =     (WFCPOG_Taxonomy_Constraint.WFCPOG_TAX_Data.put ({key=9,max_depth=i}) tax_workaround)

fun wf_is_supported_id wfpo_id = List.exists (fn w => wfpo_id = id_of w) supported_wfs

fun po_is_supported_id wfpo_id = List.exists (fn w => wfpo_id = id_of w) supported_pos

fun is_recommended (WFCPOG.WFPO{recommended,...}) = recommended 


fun check_recommended_wfcs model = check_wfcs model (List.filter (is_recommended) supported_wfs) 

fun generate_recommended_pos model = generate_pos model (List.filter (is_recommended) supported_pos) 


fun README () = 
    let
	val string = ("WFCPO-Generator\n")
    in
	print string
    end


fun info_wfs () = 
    let
	val string = ("...")
    in
	print string
    end

fun info_pos () = 
    let
	val string = ("...")
    in
	print string
    end

end;
