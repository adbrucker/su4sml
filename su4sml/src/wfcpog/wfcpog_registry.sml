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

    structure WFCPOG_LSK_Data     : WFCPOG_DATA
    structure WFCPOG_TAX_Data     : WFCPOG_DATA
    structure WFCPOG_RFM_Data     : WFCPOG_DATA


    val supported          : WFCPOG.wfpo list
    val wfpos              : WFCPOG.wfpo list ref
    val add_wfpo           : WFCPOG.wfpo -> unit
    val del_wfpo           : WFCPOG.wfpo_id -> unit
    
    val rename_wfpo        : string -> WFCPOG.wfpo -> WFCPOG.wfpo
    val get_wfpo           : WFCPOG.wfpo list -> WFCPOG.wfpo_id -> WFCPOG.wfpo
							    
    val check_wfc          : Rep.Model -> WFCPOG.wfpo -> bool
    val check_wfcs         : Rep.Model -> WFCPOG.wfpo list -> bool
    val generate_po        : Rep.Model -> WFCPOG.wfpo -> (WFCPOG.wfpo * Rep_OclTerm.OclTerm list) 
    val generate_pos       : Rep.Model -> WFCPOG.wfpo list -> (WFCPOG.wfpo * Rep_OclTerm.OclTerm list) list

    exception WFCPOG_RegistryError of string
end


structure WFCPOG_Registry :WFCPOG_REGISTRY  = 
struct

exception WFCPOG_RegistryError of string

structure WFCPOG_LSK_Data = WFCPOG_Liskov_Constraint.WFCPOG_LSK_Data
structure WFCPOG_TAX_Data = WFCPOG_Taxonomy_Constraint.WFCPOG_TAX_Data
structure WFCPOG_RFM_Data = WFCPOG_Refine_Constraint.WFCPOG_RFM_Data

open Rep_Logger
open WFCPOG

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
    

fun check_wfc model (wfc_sel)  = 
    let 
	val _ = trace 50 (name_of wfc_sel ^ "..............")
    in
	case WFCPOG.apply_of wfc_sel of
	    WFCPOG.WFC(a) => a wfc_sel model 
	  | x => raise WFCPOG_RegistryError ("A assumed wfc " ^ (name_of wfc_sel) ^ " is not a wfc!\n")
    end
    
fun check_wfcs model wfcs = List.all (fn v => (v = true)) (map (check_wfc model) wfcs) 


fun generate_po model (wfc_sel)  = 
    let
	val _ = trace 50 (name_of wfc_sel ^ "...............")
    in
	case (WFCPOG.apply_of wfc_sel) of
	    WFCPOG.POG (a) => (wfc_sel,a wfc_sel model)
	  | x  => raise WFCPOG_RegistryError ("A assumed po " ^ (name_of wfc_sel) ^ " is not a po!\n")
    end

fun generate_pos model wfcs = map (generate_po model) wfcs

val supported = [ 
    WFCPOG.WFPO{
     identifier      = "lsk_pre",
     name            = "Liskov weaken_precondition",
     description     = "Generate Proof Obligations for weaking precondition",
     recommended     = false,
     depends         = [],
     recommends      = [],
     apply           = WFCPOG.POG(WFCPOG_Liskov_Constraint.weaken_precondition),
     data            = Datatab.empty
    }, 
    WFCPOG.WFPO{
     identifier      = "lsk_post", 
     name            = "Liskov strengthen_postcondition",   (* short description (for output) *)
     description     = "Generate Proof Obligations following from strengthen_postcondition",
     recommended     = false,
     depends         = [],
     recommends      = [],
     apply           = WFCPOG.POG(WFCPOG_Liskov_Constraint.strengthen_postcondition),
     data            = Datatab.empty
    },
    WFCPOG.WFPO{
     identifier      = "lsk_inv", 
     name            = "Liskov conjugate_invariants", 
     description     = "Generate Proof Obligations following from conjugate_invariants",
     recommended     = false,
     depends         = [],
     recommends      = [],
     apply           = WFCPOG.POG(WFCPOG_Liskov_Constraint.conjugate_invariants),
     data            = Datatab.empty
    },
    WFCPOG.WFPO{
     identifier      = "lsk", 
     name            = "Liskov's subtitution principle", 
     description     = "Generate Proof Obligations following the idea from Barbara Liskov",
     recommended     = true,
     depends         = ["lsk_pre","lsk_post"],
     recommends      = [],
     apply           = WFCPOG.POG(WFCPOG_Liskov_Constraint.conjugate_invariants),
     data            = Datatab.empty
    },
    WFCPOG.WFPO{
     identifier      = "inf_ster",
     name            = "Interfas has_consistent_stereotypes",
     description     = "Checks if all operations of an interface do not have stereotypes create or destroy.",
     recommended     = false,
     depends         = [],
     recommends      = [],
     apply           = WFCPOG.WFC(WFCPOG_Interface_Constraint.has_consistent_stereotypes),
     data            = Datatab.empty
    },
    WFCPOG.WFPO{
     identifier      = "inf_name",
     name            = "Interface is_nameclash_free",
     description     = "Checks for classes inheriting from more than one interface that there are no nameclashes.",
     recommended     = false,
     depends         = [],
     recommends      = [],
     apply           = WFCPOG.WFC(WFCPOG_Interface_Constraint.is_nameclash_free),
     data            = Datatab.empty
    },
    WFCPOG.WFPO{
     identifier      = "inf",
     name            = "Interface complete",
     description     = "Checks has_consistent_stereotypes and is_nameclash_free",
     recommended     = true,
     depends         = ["inf_ster"],
     recommends      = [],
     apply           = WFCPOG.WFC(WFCPOG_Interface_Constraint.is_nameclash_free),
     data            = Datatab.empty
    },
    WFCPOG.WFPO{
     identifier      = "class_model", (* identifier                     *) 
     name            = "Data model consistency: class model",
     description     = "Data model consistency; a classes should be able to be instantiated from a state.",
     recommended     = false,
     depends         = [],
     recommends      = [],
     apply           = WFCPOG.POG(WFCPOG_Data_Model_Consistency_Constraint.class_model_consistency),
     data = Datatab.empty
    },
    WFCPOG.WFPO{
     identifier      = "strong_model", (* identifier                     *) 
     name            = "Data model consistency: strong model",
     description     = "Data model consistency; all classes should be able to be instantiated from a state.",
     recommended     = false,
     depends         = [],
     recommends      = [],
     apply           = WFCPOG.POG(WFCPOG_Data_Model_Consistency_Constraint.strong_model_consistency),
     data = Datatab.empty
    },
    WFCPOG.WFPO{
     identifier      = "oper_model", (* identifier                     *) 
     name            = "Operational model consistency",
     description     = "Inserts the additional proof obligations resulting from the SecureUML transformation",
     recommended     = false,
     depends         = [],
     recommends      = [],
     apply           = WFCPOG.POG(WFCPOG_Operational_Constraint.generate_pos),
     data = Datatab.empty
    },
    WFCPOG.WFPO{
     identifier      = "cmd", (* identifier                     *) 
     name            = "Query Command Constraint",
     description     = "Check if operations which are declared as command are really commands",
     recommended     = false,
     depends         = [],
     recommends      = [],
     apply           = WFCPOG.POG(WFCPOG_Command_Query_Constraint.ops_are_command),
     data = Datatab.empty
    },
    WFCPOG.WFPO{
     identifier      = "quy", (* identifier                     *) 
     name            = "Query Command Constraint",
     description     = "Check if operations which are declared as queries are really queries",
     recommended     = false,
     depends         = [],
     recommends      = [],
     apply           = WFCPOG.POG(WFCPOG_Command_Query_Constraint.ops_are_query),
     data = Datatab.empty
    },
    WFCPOG.WFPO{
     identifier      = "vis", (* identifier                     *) 
     name            = "Visibility Constraint",
     description     = "Check if the accessed operations in preconditions/postconditions/invariants are visible.",
     recommended     = false,
     depends         = [],
     recommends      = [],
     apply           = WFCPOG.WFC(WFCPOG_Visibility_Constraint.are_conditions_visible),
     data = Datatab.empty
    },
    WFCPOG.WFPO{
     identifier      = "tax", 
     name            = "Max Depth",
     description     = "Max Depth",
     recommended     = true,
     depends         = [],
     recommends      = [],
     apply           = WFCPOG.WFC(WFCPOG_Taxonomy_Constraint.has_maxDepth),
     data = Datatab.empty
    }, 
    WFCPOG.WFPO{ 
     identifier      = "rfm_syn", 
     name            = "OO Refinement",  
     description     = "Check if public classes of aboriginal are also public in new Package",
     recommended     = false,
     depends         = [],
     recommends      = [],
     apply           = WFCPOG.WFC(WFCPOG_Refine_Constraint.check_syntax),
     data            = Datatab.empty
    }(*, 
    WFCPOG.WFPO {
     identifier      = "ref_po", 
     name            = "OO Refinement",
     description     = "Generate Proof Obligations for OO data refinement",
     recommended     = true,
     depends         = [],
     recommends      = [],
     apply           = WFCPOG.POG(Refine_Constraint.refine_po),
     data            = Datatab.empty
    }*)]
end
