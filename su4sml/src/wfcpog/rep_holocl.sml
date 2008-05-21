(*****************************************************************************
 * HOL-OCL --- an interactive theorem-prover for for UML/OCL
 *             http://www.brucker.ch/projects/hol-ocl/
 *                                                                            
 * holocl_namespace.sml --- 
 * This file is part of HOL-OCL.
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
(* $Id: holocl_namespace.sml 7513 2008-03-27 06:27:50Z brucker $ *)

signature REP_HOLOCL_NAMESPACE =
sig
    datatype Level = univ    (* universe toolbox *)
		   | operation
		   | holocl  (* HOL-OCL toplevel *)
		   | l0      (* Level 0          *)
		   | l1      (* Level 1          *)
		   | l2      (* Level 2          *)

    (* useful stuff; move to su4sml? *)
    val rm_whitespace : string -> string
    val listin : ''a  -> ''a list -> bool

    val ext_name_of : Rep_OclType.Path -> string list


    val post_1_of : Rep_OclType.Path -> Rep_OclType.Path 
    val pre_1_of : Rep_OclType.Path -> Rep_OclType.Path 
    val local_name_of : Rep_OclType.Path -> string

    val hc_path : Level -> Rep_OclType.Path -> Rep_OclType.Path

    val get_class_of : Rep_OclType.Path -> Rep_OclType.Path 
    val mk_class_of :  Rep_OclType.Path -> Rep_OclType.Path
    val mk_def_of :  Rep_OclType.Path -> Rep_OclType.Path
					     
    val is_hc_pathstr : Level-> string -> bool
    val class2get_parent : Rep_OclType.Path -> string -> Rep_OclType.Path
    val parent2class_of : Rep_OclType.Path -> string -> Rep_OclType.Path

    val is_univ_class_of : Rep_OclType.Path -> Rep_OclType.Path
    val is_class_of : Rep_OclType.Path -> Rep_OclType.Path

    val key_name_of :  Rep_OclType.Path -> string list
    val base_type_name_of : Rep_OclType.Path -> Rep_OclType.Path
    val type_name_of : Rep_OclType.Path -> Rep_OclType.Path
    val attr_of : Rep_OclType.Path -> string -> Rep_OclType.Path
    val is_classType_of : Rep_OclType.Path -> Rep_OclType.Path

    val attr_pre_of : Rep_OclType.Path -> string -> Rep_OclType.Path 
    val type_charset_name_of : Rep_OclType.Path -> Rep_OclType.Path
    val invariant_1_of : Rep_OclType.Path -> Rep_OclType.Path
    val invariant_2_of : Rep_OclType.Path -> Rep_OclType.Path
    val strong_sve_2_of :Rep_OclType.Path -> Rep_OclType.Path
    val strict_sve_2_of :Rep_OclType.Path -> Rep_OclType.Path

    val name_of_inv : Rep_Core.Classifier -> Rep_OclType.Path
    val name_of_pre : Rep_Core.Classifier -> Rep_Core.operation -> Rep_OclType.Path
    val name_of_post : Rep_Core.Classifier -> Rep_Core.operation -> Rep_OclType.Path

end



structure Rep_HolOcl_Namespace:REP_HOLOCL_NAMESPACE = 
struct 
    datatype Level = univ    (* universe toolbox *)
		   | operation
		   | holocl  (* HOL-OCL toplevel *)
		   | l0      (* Level 0          *)
		   | l1      (* Level 1          *)
		   | l2      (* Level 2          *)



fun listin _ []  = false
  | listin e (x::xs) = if e=x then true else listin e xs


(*separate s [x1, x2, ..., xn]  ===>  [x1, s, x2, s, ..., s, xn]*)
fun separate s (x :: (xs as _ :: _)) = x :: s :: separate s xs
  | separate _ xs = xs;


fun space_implode a bs = SML90.implode (separate a bs);

fun rm_whitespace s = space_implode "_" (String.tokens (fn c => c = (#" ")
							orelse c = (#":")) s)


fun local_name_of path = case path of 
			     [] =>  "" 
			   | p => (hd o rev) p

fun hc_path level p =  if ((String.isSubstring "OclAny" (local_name_of p))
		       andalso not  (String.isSubstring "_2_OclAny" (local_name_of p))
		       andalso not  (String.isSubstring "OclAny_2" (local_name_of p)))
		       (* hack *)
		      then 			  
			   case level of
			      univ      => p  
			    | operation => p
			    | holocl    => p 
			    | l0        => ("UML_OCL"::["level0"])@p 	
			    | l1        => ("UML_OCL"::["level1"])@p 
			    | l2        => p 
		      else
			  case level of
			      univ      => ("UML_OCL"::["univ"])@p   (* universe toolbox *)
			    | operation => ("UML_OCL"::["ops"])@p 
			    | holocl    =>  "UML_OCL"::p             (* HOL-OCL toplevel *)
			    | l0        => ("UML_OCL"::["level0"])@p   
			    | l1        => ("UML_OCL"::["level1"])@p
			    | l2        => p 


fun class2get_parent path superclass = rev (((hd (rev path))^"_2_"^superclass)::(tl (rev path)))

fun parent2class_of path superclass = rev ((superclass^"_2_"^(hd (rev path)))::(tl (rev path)))

fun mk_def_of path = rev (((hd (rev path))^"_def")::(tl (rev path)))   
fun type_charset_name_of path = rev (((hd (rev path))^"_type")::(tl (rev path)))

fun attr_of path attr=  path @[attr]   

fun attr_pre_of path attr = path@[attr^"_pre"]         
fun invariant_1_of  path = path@["invariant1"]
fun invariant_2_of  path = path@["inv"]
fun strong_sve_2_of  path = path@["strong_sve"]
fun strict_sve_2_of  path = path@["strict_sve"]

fun rm_whitespace s = space_implode "_" (String.tokens (fn c => c = (#" ")
                                                               orelse c = (#":")) s)

fun op_tab_of   path = path@["op_tab"]

fun pre_1_of  path = path@["pre1"]
fun post_1_of  path = path@["post1"]
 
fun get_class_of path = rev (("get_"^(hd (rev path)))::(tl (rev path)))           (* mkC *)	


fun mk_class_of path = rev (("mk_"^(hd (rev path)))::(tl (rev path)))           (* mkC *)	
						  
fun key_name_of path = if (path = (Rep_Core.name_of Rep_Core.OclAnyC)) (* hack *)
		  then ["OCL_OclAny_type","OclAny_key"]
		  else rev (((hd (rev path))^"_key")::(tl (rev path))) (* keyC *)	
						

fun is_hc_pathstr level pstr = 	if ((String.isSubstring "OclAny" pstr)
		       andalso not  (String.isSubstring "_2_OclAny" pstr)
		       andalso not  (String.isSubstring "OclAny_2" pstr))
		       (* hack *)
		      then 			  
			   case level of
			      univ   => false
			    | operation => false
			    | holocl    => false
			    | l0        => String.isSubstring "UML_OCL.level0." pstr
			    | l1        => String.isSubstring "UML_OCL.level1." pstr 
			    | l2        => false 
		      else
			  case level of
			      univ   => String.isSubstring "UML_OCL.univ." pstr
			    | operation => String.isSubstring "UML_OCL.ops" pstr
			    | holocl    => String.isSubstring "UML_OCL." pstr
			    | l0        => String.isSubstring "UML_OCL.level0." pstr
			    | l1        => String.isSubstring "UML_OCL.level1." pstr
			    | l2        => not (String.isSubstring "UML_OCL." pstr)




fun base_type_name_of path = if (path = (Rep_Core.name_of Rep_Core.OclAnyC)) (* hack *) 
			     then  ["OCL_OclAny_type","OclAny_key"] (* ??? *)
			     else rev (((hd (rev path))^"_base_type")::(tl (rev path)))

fun type_name_of path = if (path = (Rep_Core.name_of Rep_Core.OclAnyC)) (* hack *)
			then ["OCL_OclAny_type","OclAny_key"]
			else rev (((hd (rev path))^"_type")::(tl (rev path)))           (* typeC *)	



fun is_class_of path =  rev (("is_"^(hd (rev path)))::(tl (rev path)))           (* mkC *)	
fun is_classType_of path =  rev (("isType_"^(hd (rev path)))::(tl (rev path)))           (* mkC *)	
fun is_univ_class_of path = rev (("is_"^(hd (rev path))^"_univ")::(tl (rev path)))           (* mkC *)	


fun ext_name_of path = if (path = (Rep_Core.name_of Rep_Core.OclAnyC)) (* hack *) 
		  then  ["OCL_OclAny_type","OclAny_ext"]
		  else rev (((hd (rev path))^"_ext")::(tl (rev path))) (* extC *)	


fun name_of_inv C = let
fun invariant_1_of path  = path@["model","inv"]

in
hc_path l2 (invariant_1_of  ((Rep_Core.name_of C)))
end
fun name_of_pre C Op = hc_path l2 (pre_1_of  ((Rep_Core.name_of C)@[Rep_Core.mangled_name_of_op Op]))
fun name_of_post C Op = hc_path l2 (post_1_of  ((Rep_Core.name_of C)@[Rep_Core.mangled_name_of_op Op]))

end
signature REP_HOLOCL_HELPER =
sig 
    val holocl_and        : Rep_OclTerm.OclTerm -> Rep_OclTerm.OclTerm -> Rep_OclTerm.OclTerm
    val holocl_and_all    : Rep_OclTerm.OclTerm list -> Rep_OclTerm.OclTerm 
    val holocl_or         : Rep_OclTerm.OclTerm -> Rep_OclTerm.OclTerm -> Rep_OclTerm.OclTerm
    val holocl_or_all     : Rep_OclTerm.OclTerm list -> Rep_OclTerm.OclTerm 
    val holocl_implies    : Rep_OclTerm.OclTerm -> Rep_OclTerm.OclTerm -> Rep_OclTerm.OclTerm
    val holocl_not        : Rep_OclTerm.OclTerm -> Rep_OclTerm.OclTerm 
    val holocl_xor        : Rep_OclTerm.OclTerm -> Rep_OclTerm.OclTerm -> Rep_OclTerm.OclTerm
    val holocl_localValid_state      : Rep_OclTerm.OclTerm -> string -> Rep_OclTerm.OclTerm
    val holocl_localValid_transition : Rep_OclTerm.OclTerm -> string -> string -> Rep_OclTerm.OclTerm
    val get_holocl_operation         : Rep_Core.operation -> Rep_Core.Classifier -> Rep_Core.transform_model -> Rep_OclTerm.OclTerm
    val get_holocl_abstraction_relation: Rep_Core.Classifier -> Rep_Core.Classifier -> Rep_Core.transform_model -> Rep_OclTerm.OclTerm
end
structure Rep_HolOcl_Helper:REP_HOLOCL_HELPER = 
struct 
open Rep_Core
open Rep_Logger
open Rep_OclType
open Rep_OclTerm
open WFCPOG_Library

exception HolOcl_Helper_InvalidArguments of string


fun type_of_t term = Rep_OclHelper.type_of term

fun ocl_opcall (source:OclTerm) f args t  = OperationCall (source, type_of_t source, f,
                                                  map (fn x => (x,type_of_t x)) args,
                                                  t)

fun holocl_and a b        = ocl_opcall a ["holOclLib","Boolean","and"]     [b] Boolean
fun holocl_and_all []     = raise (HolOcl_Helper_InvalidArguments ("holocl.holocl_and_all: empty argument list"))
  | holocl_and_all [a:OclTerm]    = a
  | holocl_and_all (h::t) = holocl_and h (holocl_and_all t)
fun holocl_or  a b        = ocl_opcall a ["holOclLib","Boolean","or"]      [b] Boolean
fun holocl_or_all []      = raise (HolOcl_Helper_InvalidArguments ("holocl.holocl_or_all: empty argument list"))
  | holocl_or_all [a:OclTerm]     = a
  | holocl_or_all (h::t)  = holocl_or h (holocl_or_all t)
fun holocl_implies a b    = ocl_opcall a ["holOclLib","Boolean","implies"] [b] Boolean
fun holocl_xor a b        = ocl_opcall a ["holOclLib","Boolean","xor"]     [b] Boolean
fun holocl_not a          = ocl_opcall a ["holOclLib","Boolean","not"]     []  Boolean

fun holocl_localValid_state term var_name = 
    OperationCall(term,Boolean,["holOclLib","Boolean","OclLocalValid"],[(Literal(var_name,OclState),DummyT)],Boolean)

fun holocl_localValid_transition term var_name1 var_name2 = 
    let
	val sigma = Literal(var_name1,OclState)
	val sigma_s = Literal(var_name2,OclState)
	val tuple_term = Tuple [(var_name1,sigma,OclState),(var_name2,sigma_s,OclState)]
    in
	OperationCall(term,Boolean,["holOclLib","Boolean","OclLocalValid"],[(tuple_term,OclState)],Boolean)
    end

fun get_holocl_operation oper class model = 
    let
	val _ = trace function_calls ("WFCPOG_Refine_Constraint.get_holocl_operation\n") 
	val hol_name = string_of_path ((name_of class)@[(name_of_op oper)])
	val styp = type_of class
	val src = Variable((string_of_path (name_of class))^(name_of_op oper),styp)
	val predicate = Predicate(src,Boolean,[hol_name],args2varargs (arguments_of_op oper))
	val _ = trace function_ends ("WFCPOG_Refine_Constraint.get_holocl_operation\n") 
    in
	predicate
    end

fun get_holocl_abstraction_relation  abs_class conc_class model =
    let
	val _ = trace function_calls ("WFCPOG_Refine_Constraint.get_holocl_abstraction_relation\n")
	val predicate_name = "R_"^(string_of_path (package_of abs_class))^"_"^(string_of_path (package_of conc_class))^"_"^(List.last (name_of abs_class))
	val abs_typ = type_of abs_class
	val conc_typ = type_of conc_class
	val abs_state = Variable("tau_concrete",OclState)
	val conc_state = Variable("tau_abstract",OclState)
	val abs_term = Variable(string_of_path (name_of abs_class),abs_typ)
	val conc_term = Variable(string_of_path (name_of conc_class),conc_typ)
	val predicate = Predicate(abs_state,OclState,[predicate_name],[(conc_state,OclState),(abs_term,abs_typ),(conc_term,conc_typ)])
	val _ = trace function_ends ("WFCPOG_Refine_Constraint.get_holocl_abstraction_relation\n")
    in
	predicate
    end
end
