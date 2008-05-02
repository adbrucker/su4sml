(*****************************************************************************
 * su4sml --- a SML repository for managing (Secure)UML/OCL models
 *             http://projects.brucker.ch/su4sml/
 *                                                                            
 *  ---
 * This file is part of su4sml.
 *
 * Copyright (c) 2008 Achim D. Brucker, Germany
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
							 
    (** Execute a wfc.*)    
    val check_wfc          : Rep.Model -> WFCPOG.wfpo -> bool
    (** Execute a wfc with verbose output in case of a false wfc. *)
    val check_wfc_verbose  : Rep.Model -> WFCPOG.wfpo -> bool
    (** Execute a list of wfcs.*)
    val check_wfcs         : Rep.Model -> WFCPOG.wfpo list -> bool

    (** Execute all recommended wfcs.*)
    val check_recommended_wfcs : Rep.Model -> bool

    (** Generate pos for a given wfpo.*)
    val generate_po        : Rep.Model -> WFCPOG.wfpo -> (WFCPOG.wfpo * (Rep_OclType.Path * Rep_OclTerm.OclTerm) list) 
    (** Generate pos for a list of wfpos.*)
    val generate_pos       : Rep.Model -> WFCPOG.wfpo list -> (WFCPOG.wfpo * (Rep_OclType.Path * Rep_OclTerm.OclTerm) list) list

    (** Generate all recommended pos.*)							      
    val generate_recommended_pos : Rep.Model -> (WFCPOG.wfpo * (Rep_OclType.Path * Rep_OclTerm.OclTerm) list) list

    (** Argument Wrappers *)
    (** Create wfpo for wfc max_depth.*)
    val create_wfc_tax     : int -> WFCPOG.wfpo
    exception WFCPOG_RegistryError of string
end


structure WFCPOG_Registry :WFCPOG_REGISTRY  = 
struct

exception WFCPOG_RegistryError of string

open Rep_Logger
open WFCPOG
open Datatab

val wfpos = ref ([]:(WFCPOG.wfpo list))


fun wfcpog_of id = hd  (List.filter (fn m => WFCPOG.id_of m = id) (!wfpos))


fun add_wfpo wfpo = ((wfpos := [wfpo]@(!wfpos));())

fun del_wfpo wfpo_id = ((wfpos := List.filter (fn w => not ((WFCPOG.id_of w) = (wfpo_id)) ) 
			   (!wfpos));())

fun get_wfpo [] x = raise WFCPOG_RegistryError ("No ID = " ^ x ^ " found in given list!\n")
  | get_wfpo (h::tail) x = 
    if (id_of h = x) 
    then h
    else get_wfpo tail x 

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
     apply           = WFCPOG.WFC(WFCPOG_Taxonomy_Constraint.has_maxDepth),
     data            = Datatab.empty 
    }
		       
val supported_wfs = [ 
    WFCPOG.WFPO{
     identifier      = "wfc_inf_ster",
     name            = "WFC Interface Consistency consistent stereotypes (subconstraint)",
     description     = "Checks if all operations of an interface don't have the stereotypes 'create' or 'destroy'.\n",
     recommended     = false,
     depends         = [],
     recommends      = [],
     apply           = WFCPOG.WFC(WFCPOG_Interface_Constraint.has_consistent_stereotypes),
     data            = Datatab.empty
    },
    WFCPOG.WFPO{
     identifier      = "wfc_inf_name",
     name            = "WFC Interface Consistency no nameclashes (subconstraint)",
     description     = "Checks for classes inheriting from more than one interface that there are no nameclashes.\n",
     recommended     = false,
     depends         = [],
     recommends      = [],
     apply           = WFCPOG.WFC(WFCPOG_Interface_Constraint.is_nameclash_free),
     data            = Datatab.empty
    },
    WFCPOG.WFPO{
     identifier      = "wfc_inf",
     name            = "WFC Interface Consistency (complete)",
     description     = "Checking of two subconstraints: \n wfc_inf_ster: Checks if all operations of an interface don't have the stereotypes 'create' or 'destroy'. \n wfc_inf_name : Checks for classes inheriting from more than one interface that there are no nameclashes.\n",
     recommended     = true,
     depends         = ["wfc_inf_ster"],
     recommends      = [],
     apply           = WFCPOG.WFC(WFCPOG_Interface_Constraint.is_nameclash_free),
     data            = Datatab.empty
    },
    WFCPOG.WFPO{
     identifier      = "wfc_vis", 
     name            = "WFC Visibility Consistency",
     description     = "Checks if the accessed operations in preconditions/postconditions/invariants are visible.\n",
     recommended     = true,
     depends         = [],
     recommends      = [],
     apply           = WFCPOG.WFC(WFCPOG_Visibility_Constraint.are_conditions_visible),
     data = Datatab.empty
    },     
     (* TODO: insert this constraint for having a default value.                                       *)
     (*                                                                                                *)
     (*    (WFCPOG_Taxonomy_Constraint.WFCPOG_TAX_Data.put ({key=9,max_depth=5}) tax_workaround)       *)
     (*                                                                                                *)
    WFCPOG.WFPO{
     identifier      = "wfc_tax", 
     name            = "WFC Taxonomy Consistency",
     description     = "Checks if the inheritance hierarchy is not deeper than n (default value n=5)\n",
     recommended     = true,
     depends         = [],
     recommends      = [],
     apply           = WFCPOG.WFC(WFCPOG_Taxonomy_Constraint.has_maxDepth),
     data            = Datatab.empty 
    }
    , 
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
    WFCPOG.WFPO{ 
     identifier      = "wfc_cstr", 
     name            = "WFC Constructor Consistency",  
     description     = "Checks if a given class overwrites all old creators\n.",
     recommended     = true,
     depends         = [],
     recommends      = [],
     apply           = WFCPOG.WFC(WFCPOG_Constructor_Constraint.overwrites_old_creators),
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
     identifier      = "po_lsk", 
     name            = "Liskov's subtitution principle", 
     description     = "Generate Proof Obligations following the idea from Barbara Liskov",
     recommended     = true,
     depends         = ["po_lsk_pre","po_lsk_post"],
     recommends      = [],
     apply           = WFCPOG.POG(WFCPOG_Liskov_Constraint.conjugate_invariants),
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
    },
    WFCPOG.WFPO{
     identifier      = "po_cmd", (* identifier                     *) 
     name            = "Query Command Constraint",
     description     = "Check if operations which are declared as command are really commands",
     recommended     = false,
     depends         = [],
     recommends      = [],
     apply           = WFCPOG.POG(WFCPOG_Command_Query_Constraint.ops_are_command),
     data = Datatab.empty
    },
    WFCPOG.WFPO{
     identifier      = "po_quy", (* identifier                     *) 
     name            = "Query Command Constraint",
     description     = "Check if operations which are declared as queries are really queries",
     recommended     = false,
     depends         = [],
     recommends      = [],
     apply           = WFCPOG.POG(WFCPOG_Command_Query_Constraint.ops_are_query),
     data = Datatab.empty
    },
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
     name            = "Constructor Consistency post implies invariants(subconstraint)",  
     description     = "Checks if the postcondition of any constructor operation imples the class' invariant.\n",
     recommended     = false,
     depends         = [],
     recommends      = [],
     apply           = WFCPOG.POG(WFCPOG_Constructor_Constraint.post_implies_invariant),
     data            = Datatab.empty
    },
    WFCPOG.WFPO{ 
     identifier      = "po_cstr_attr", 
     name            = "WFC Constructor Consistency attributes are inited(subconstraint)",  
     description     = "Checks if after the execution of any constructor operation all the attributes are initialized.\n",
     recommended     = false,
     depends         = [],
     recommends      = [],
     apply           = WFCPOG.POG(WFCPOG_Constructor_Constraint.attributes_are_inited),
     data            = Datatab.empty
    },
    WFCPOG.WFPO{ 
     identifier      = "po_cstr", 
     name            = "WFC Constructor Consistency (complete)",
     description     = "Checks two subconstraints: \n cstr_post : Checks if after the execution of any constructor operation all the attributes are initialized.\n cstr_attr: Checks if after the execution of any constructor operation all the attributes are initialized.\n",
     recommended     = false,
     depends         = ["po_cstr_post"],
     recommends      = [],
     apply           = WFCPOG.POG(WFCPOG_Constructor_Constraint.attributes_are_inited),
     data            = Datatab.empty
    }
    (*,
    WFCPOG.WFPO{
     identifier      = "oper_model", (* identifier                     *) 
     name            = "Operational model consistency",
     description     = "Inserts the additional proof obligations resulting from the SecureUML transformation",
     recommended     = false,
     depends         = [],
     recommends      = [],
     apply           = WFCPOG.POG(WFCPOG_Operational_Constraint.generate_pos),
     data = Datatab.empty
    }*)
]

val supported = supported_wfs@supported_pos


fun is_wfc (WFCPOG.WFPO wfpo) = case #apply wfpo of 
				  WFCPOG.WFC _ => true
				| _            => false

fun is_pog (WFCPOG.WFPO wfpo) = case #apply wfpo of 
				  WFCPOG.POG _ => true
				| _            => false


(* RETURN: ( bool list , po list) *)
fun process_depends [] model acc = acc 
  | process_depends (h::tail) model  acc =
    let
	val _ = trace function_calls ("WFCPOG_Registry.process_depends\n")
	val wfpo  = get_wfpo supported h
	val _ = trace wgen ("Dependent wfpo " ^ (name_of wfpo) ^ " found.\n")
	val res = 
	    case (WFCPOG.apply_of wfpo) of
		WFCPOG.WFC(a) => (process_depends tail model ((#1 acc)@[a wfpo model],(#2 acc)))
	      | WFCPOG.POG(a) => (process_depends tail model ((#1 acc),(#2 acc)@(a wfpo model)))
	val _ = trace function_ends ("WFCPOG_Registry.process_depends\n")
    in
	res
    end


fun check_wfc model (wfc_sel as WFCPOG.WFPO{identifier,name,description,recommended,depends,recommends,apply,data}:WFCPOG.wfpo)  = 
    let 
	val _ = trace function_calls ("WFCPOG_Registry.check_wfc\n")
	val _ = trace wgen (name_of wfc_sel ^ ".............." ^ "\n")
	val res = 
	    case WFCPOG.apply_of wfc_sel of
		WFCPOG.WFC(a) => if (depends = []) 
				 then 
				     let 
					 val _ = trace wgen ("No dependent wfpos.\n")
				     in
					 a wfc_sel model
				     end
				 else 
				     let
					 val _ = trace wgen ("Dependent wfpos detected.\n")
					 val _ = trace wgen ("Process dependencies ...\n")
					 val (wfs,pos) = process_depends depends model ([],[])
					 val _ = trace wgen ("Dependencies processed ...\n")
				     in
					 (List.all (fn a => a = true) wfs)
				     end
	      | x => raise WFCPOG_RegistryError ("A assumed wfc " ^ (name_of wfc_sel) ^ " is not a wfc!\n") 
	val _ = trace function_ends ("WFCPOG_Registry.check_wfc\n")
    in
	res
    end

fun check_wfc_verbose model wfc =
    check_wfc model wfc 
    handle WFCPOG_WFC_FailedException s => 
	   let
	       val _ = trace exce s 
	   in
	       false
	   end
    
fun check_wfcs model wfcs = List.all (fn v => (v = true)) (map (check_wfc model) wfcs) 


fun generate_po model (wfc_sel as WFCPOG.WFPO{identifier,name,description,recommended,depends,recommends,apply,data}:WFCPOG.wfpo)  = 
    let
	val _ = trace function_calls ("WFCPOG_Registry.generate_po\n")
	val _ = trace wgen (name_of wfc_sel ^ "...............")
	val res = 
	    case (WFCPOG.apply_of wfc_sel) of
		WFCPOG.POG (a) => (* (wfc_sel,a wfc_sel model) *)
		if (depends = [])
		then (wfc_sel,a wfc_sel model)
		else 
		    let
			val (wfs,pos) = process_depends depends model ([],[])
		    in
			if (List.all (fn a => a = true) wfs)
			then (wfc_sel,pos@(a wfc_sel model))
			else raise WFCPOG_RegistryError("A dependent wellformedness check of the po " ^ (name_of wfc_sel) ^ " is not true!\n")
		    end
	      | x  => raise WFCPOG_RegistryError ("A assumed po " ^ (name_of wfc_sel) ^ " is not a po!\n")
	val _ = trace function_ends ("WFCPOG_Registry.generate_po\n")
    in
	res
    end

fun generate_pos model wfcs = map (generate_po model) wfcs

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
