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

signature HOLOCL_NAMESPACE =
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



structure HolOcl_Namespace:HOLOCL_NAMESPACE = 
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