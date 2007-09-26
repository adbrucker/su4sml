(*****************************************************************************
 * su4sml --- a SML repository for managing (Secure)UML/OCL models
 *             http://projects.brucker.ch/su4sml/
 *                                                                            
 * ocl2string --- "pretty printing" for OCL terms
 * This file is part of su4sml.
 *
 * Copyright (c) 2005-2007, ETH Zurich, Switzerland
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
(* $Id$ *)

(** "pretty printing" for OCL expressions. *)
structure Ocl2String = 
struct
open library
open Rep_OclType
open Rep_OclTerm
open Rep_Core


fun ocl2string show_types oclterm =     
    let 
	fun string_of_infix show_types src styp opname arg atyp rtyp 
	  = if show_types 
	    then "(("^(ocl2string show_types src)^"):"^(string_of_OclType styp)
		 ^" "^opname^" ("^(ocl2string show_types arg)^"):"^(string_of_OclType atyp)
		 ^"):"^(string_of_OclType rtyp)
	    else (ocl2string show_types src)
		 ^" "^opname^" "^(ocl2string show_types arg)
	fun string_of_prefix1 show_types src styp opname rtyp 
	  = if show_types 
	    then "(("^opname^" ("^(ocl2string show_types src)^"):"^(string_of_OclType styp)
		 ^"):"^(string_of_OclType rtyp)^")"
	    else "("^opname^" "^(ocl2string show_types src)^")"
	fun string_of_oo_infix show_types src styp opname arg atyp rtyp 
	  = if show_types 
	    then "(("^(ocl2string show_types src)^"):"^(string_of_OclType styp)
		 ^"->"^opname^"("^(ocl2string show_types arg)^"):"^(string_of_OclType atyp)
		 ^")):"^(string_of_OclType rtyp)
	    else (ocl2string show_types src)
		 ^"->"^opname^"("^(ocl2string show_types arg)^")"
	fun string_of_oo_postfix1 show_types src styp opname rtyp 
	  = if show_types 
	    then "((("^(ocl2string show_types src)^"):"^(string_of_OclType styp)
		 ^")->"^opname^"():"^(string_of_OclType rtyp)^")"
	    else "("^(ocl2string show_types src)^")->"^opname^"()"
	fun string_of_oo_typeinfix show_types src styp opname arg rtyp 
	  = if show_types 
	    then "(("^(ocl2string show_types src)^"):"^(string_of_OclType styp)
		 ^"->"^opname^"("^(string_of_OclType arg)^"):OclType"
		 ^")):"^(string_of_OclType rtyp)
	    else (ocl2string show_types src)
		 ^"->"^opname^"("^(string_of_OclType arg)^")"
	fun cs_list []  = ""
	  | cs_list [a] = a
	  | cs_list l   = foldl (fn (x,y) => (y^", "^x)) (hd l) (tl l)
	fun arglist show_types args = cs_list 
					  (map (fn (arg,atyp) 
						   => if show_types 
						      then "("^(ocl2string show_types arg)^"):"^(string_of_OclType atyp)
						      else (ocl2string show_types arg)) args)
	fun collection_part_list show_types args = cs_list (map (fn x => case x
                                                                      of CollectionItem (term,typ) => ocl2string show_types term
                                                                       | CollectionRange (t1,t2,typ) => (ocl2string show_types t1)^".."^(ocl2string show_types t2))
                                                            args)
    in
    case oclterm of 
	(**************************************)
	(* Literal                            *)
	(**************************************)
	(* OCL Boolean                        *)
	Literal (s, String)         => if show_types 
				       then "("^s^":"^(string_of_OclType String)^")" 
				       else ""^s^""
      | Literal (lit, typ)          => if show_types 
					then "("^lit^":"^(string_of_OclType typ)^")" 
					else lit
  | CollectionLiteral (parts, typ as Bag x) => "Bag{"^(collection_part_list show_types parts)^"}" 
  | CollectionLiteral (parts, typ as Set x) => "Set{"^(collection_part_list show_types parts)^"}" 
  | CollectionLiteral (parts, typ as OrderedSet x) => "OrderedSet{"^(collection_part_list show_types parts)^"}" 
  | CollectionLiteral (parts, typ as Sequence x) => "Sequence{"^(collection_part_list show_types parts)^"}" 

      | If (cterm,ctyp, tterm,ttyp,eterm,etyp,iftyp) =>  if show_types 
							     then "(if ("^(ocl2string show_types cterm)^":"^(string_of_OclType ctyp)
								  ^") then ("^(ocl2string show_types tterm)^":"^(string_of_OclType ttyp)
								  ^") else ("^(ocl2string show_types eterm)^":"^(string_of_OclType etyp)
								  ^") endif:"^(string_of_OclType iftyp)^")" 
							     else "if "^(ocl2string show_types cterm)
								  ^" then "^(ocl2string show_types tterm)
								  ^" else "^(ocl2string show_types eterm)^" endif"
      | AssociationEndCall(src,styp,path,ptyp)    => if show_types
						     then "(("^(ocl2string show_types src)^":"^(string_of_OclType styp)^")."
							  ^(hd (rev path))^":"^(string_of_OclType ptyp)^")"
						     else (ocl2string show_types src)^"."^(hd (rev path))
      | AttributeCall(src,styp,path,ptyp)         => if show_types
						     then "(("^(ocl2string show_types src)^":"^(string_of_OclType styp)^")."
							  ^(hd (rev path))^":"^(string_of_OclType ptyp)^")"
						     else (ocl2string show_types src)^"."^(hd (rev path))
      (**************************************)
      (* OperationCall                      *)
      (**************************************)
      (* OCL Boolean *)
      (* @pre                               *)
      | OperationCall (src,styp,["oclLib","OclAny","atPre"],[],rtyp) => if show_types 
									then "((("^(ocl2string show_types src)^"):"^(string_of_OclType styp)
									     ^")@pre:"^(string_of_OclType rtyp)^")"
									else "("^(ocl2string show_types src)^")@pre"
      | OperationCall (src,styp,["oclLib","Boolean",opname],[(arg,atyp)],rtyp) => string_of_infix show_types  src styp opname arg atyp rtyp 
      | OperationCall (src,styp,["oclLib","Boolean","not"],[],rtyp)            => string_of_prefix1 show_types src styp "not" rtyp
      | OperationCall (src,styp,[opname],[],rtyp)                              => string_of_oo_postfix1 show_types src styp opname rtyp
      | OperationCall (src,styp,["oclLib",classifier,"="],[(arg,atyp)],rtyp)   => string_of_infix show_types  src styp "="  arg atyp rtyp
      | OperationCall (src,styp,["oclLib",classifier,"<>"],[(arg,atyp)],rtyp)  => string_of_infix show_types  src styp "<>" arg atyp rtyp
      | OperationCall (src,styp,["oclLib",classifier,"=="],[(arg,atyp)],rtyp)  => string_of_infix show_types  src styp "==" arg atyp rtyp
      | OperationCall (src,styp,["oclLib",classifier,"~="],[(arg,atyp)],rtyp)  => string_of_infix show_types  src styp "~=" arg atyp rtyp
      (* OCL Numerals                       *)
      | OperationCall (src,styp,["oclLib",classifier,"<"],[(arg,atyp)],rtyp)   => string_of_infix show_types  src styp "<" arg atyp rtyp
      | OperationCall (src,styp,["oclLib",classifier,"<="],[(arg,atyp)],rtyp)  => string_of_infix show_types  src styp "<=" arg atyp rtyp
      | OperationCall (src,styp,["oclLib",classifier,">"],[(arg,atyp)],rtyp)   => string_of_infix show_types  src styp ">" arg atyp rtyp
      | OperationCall (src,styp,["oclLib",classifier,">="],[(arg,atyp)],rtyp)  => string_of_infix show_types  src styp ">=" arg atyp rtyp
      (* OCL Real                          *)
      | OperationCall (src,styp,["oclLib",classifier,"round"],[],rtyp)         => string_of_oo_postfix1 show_types src styp "round" rtyp   
      | OperationCall (src,styp,["oclLib",classifier,"floor"],[],rtyp)         => string_of_oo_postfix1 show_types src styp "floor" rtyp 
      | OperationCall (src,styp,["oclLib",classifier,"min"],[(arg,atyp)],rtyp) => string_of_oo_infix show_types  src styp "min" arg atyp rtyp
      | OperationCall (src,styp,["oclLib",classifier,"max"],[(arg,atyp)],rtyp) =>   string_of_oo_infix show_types  src styp "max" arg atyp rtyp
      | OperationCall (src,styp,["oclLib",classifier,"/"],[(arg,atyp)],rtyp)   =>  string_of_infix show_types  src styp "/" arg atyp rtyp
      | OperationCall (src,styp,["oclLib",classifier,"abs"],[],rtyp)           =>  string_of_oo_postfix1 show_types src styp "abs" rtyp    
      | OperationCall (src,styp,["oclLib",classifier,"-"],[(arg,atyp)],rtyp) => string_of_infix show_types  src styp "-" arg atyp rtyp
      | OperationCall (src,styp,["oclLib",classifier,"+"],[(arg,atyp)],rtyp) => string_of_infix show_types  src styp "+" arg atyp rtyp
      | OperationCall (src,styp,["oclLib",classifier,"*"],[(arg,atyp)],rtyp) => string_of_infix show_types  src styp "*" arg atyp rtyp  
      (* OCL Integer                        *)
      | OperationCall (src,styp,["oclLib",classifier,"mod"],[(arg,atyp)],rtyp) => string_of_infix show_types  src styp "mod" arg atyp rtyp   
      | OperationCall (src,styp,["oclLib",classifier,"div"],[(arg,atyp)],rtyp) => string_of_infix show_types  src styp "div" arg atyp rtyp
      | OperationCall (src,styp,["oclLib",classifier,"-"],[],rtyp)             => string_of_prefix1 show_types src styp "-" rtyp
      (* OCL String                          *)
(*      | OperationCall (src,styp,["oclLib",classifier,"subString"],[(b,Integer),(e,Integer)],String) 
	=> OclSubString u (ocl2string u  src) (ocl2string u  b) (ocl2string u  e)		 
*)

      | OperationCall (src,styp,["oclLib",classifier,"allInstances"],[],_)
        => (string_of_OclType (Rep_OclHelper.type_of src))^"::allInstances()"
      | OperationCall (src,styp,["oclLib",classifier,opname],[],rtyp) =>  string_of_oo_postfix1 show_types src styp opname rtyp
      | OperationCall (src,styp,["oclLib",classifier,opname],[(arg,atyp)],rtyp) => string_of_oo_infix show_types  src styp opname arg atyp rtyp
  
      (* OperationCalls to modell and Error *)
      (* TODO *)
      | OperationCall (src,styp,op_name,args,t) => if show_types 
						   then "("^(ocl2string show_types src)^"."^(hd (rev op_name))
							^"("^arglist show_types args^")"
							^"):"^(string_of_OclType t)
						   else (ocl2string show_types src)^"."^(hd (rev op_name))
							^"("^arglist show_types args^")"
							
      (**************************************)
      (* Variable                           *) 
      (**************************************)
      | Variable (vname,t) => if show_types
			      then "("^vname^":"^(string_of_OclType t)^")"
			      else vname
      (**************************************)
      (* Let                                *)
      (**************************************) 
      (* Error                              *)
      | Let (var,vart,rhs,rhst,i,it) => "let "^var^":"^(string_of_OclType vart)^
                                        " = "^(ocl2string show_types rhs)^
                                        "in\n"^(ocl2string show_types i)
                                        
      (**************************************)
      (* OperationWithType                  *)
      (**************************************)
      (* Error                              *)
      | OperationWithType (src,styp,opname,oclType,rtyp) =>  string_of_oo_typeinfix show_types  src styp opname oclType  rtyp
      (**************************************)
      (* Iterate                            *)
      (**************************************)
      (* Error                              *)
      | Iterate ([],acc_name,acc_type,acc_term,sterm,stype,bterm,btype,res_type) =>
	(ocl2string false sterm) ^ "->" ^ "iterate(" ^ acc_name ^ ":" ^ (Rep_OclType.string_of_OclType acc_type) ^ (ocl2string false acc_term) ^ (ocl2string false bterm)
      | Iterate (iter_vars,acc_name,acc_type,acc_term,sterm,stype,bterm,btype,res_type) =>  
	let
	    fun  string_of_vars [] = "" 
	       | string_of_vars ((string,typ)::tail) = (string ^ ":" ^ (Rep_OclType.string_of_OclType typ))^(string_of_vars tail)
	in
	    (ocl2string false sterm) ^ "->" ^ "iterate(" ^ (string_of_vars iter_vars)  ^ acc_name ^ ":" ^ (Rep_OclType.string_of_OclType acc_type) ^ (ocl2string false acc_term) ^ (ocl2string false bterm)
	end
      (**************************************)
      (* Iterator                           *)
      (**************************************)
      (* forAll                             *)
      | Iterator (iname,vars,src,styp,c,ctyp,rtyp) => if show_types 
						      then "("^(ocl2string show_types src)^":"
							   ^(string_of_OclType styp)^"->"^iname^"("
							   ^(cs_list (map (fn (a,t) => a^":"^(string_of_OclType t)) 
									  vars))
							   ^"|"^(ocl2string show_types c)^")"
						       else (ocl2string show_types src)^"->"^iname^"("
							    ^(cs_list (map #1 vars))
							    ^"|"^(ocl2string show_types c)^")"
(*
      (* OCL Collection                     *) 
      | Iterate (src,styp,["oclLib",classifier,"iterate"],args,Collection _) => OclIterate u C be e  
      | Iterate (src,styp,["oclLib",classifier,"isUnique"],args,Collection _) => OclIsUnique  u C be  
      | Iterate (src,styp,["oclLib",classifier,"one"],args,Collection _) => OclOne  u C be       
      | Iterate (src,styp,["oclLib",classifier,"any"],args,Collection _) => OclAny  u C be       
      (* OCL OrderedSet                     *) 
      | Iterate (src,styp,["oclLib",classifier,"count"],[(arg,_)],OrderedSet _) => OclOSetCount u S e   *)
      (* Error                              *)
      (*    | Iterator (s,_,_,_,_,_,_) => error ("error: unknown Iterator '"^(s)^"' in in ocl2string") *)   
      (**************************************)
      (* Catch out                          *)
      (**************************************)
      (* Error                              *)
      | _ => error "error: unknown OCL-term in in ocl2string"
    end
end

(** "pretty printing" of Repository models *)
structure Rep2String = 
struct

fun precond2string (SOME n,t) = "    pre "^n^":\n         "^ 
				(Ocl2String.ocl2string false t)^"\n"
  | precond2string (NONE,t) = "    pre: "^ (Ocl2String.ocl2string false t)^"\n"

fun postcond2string (SOME n,t) = "    post "^n^":\n         "^ 
				 (Ocl2String.ocl2string false t)^"\n" 
  | postcond2string (NONE,t) = "    post: "^ (Ocl2String.ocl2string false t)^"\n"

fun inv2string (SOME n,t) = "  inv "^n^":\n       "^(Ocl2String.ocl2string false t)^"\n"
  | inv2string (NONE,t) = "  inv: "^(Ocl2String.ocl2string false t)^"\n"

fun argument2string (n,t) = n^":"^(Rep_OclType.string_of_OclType t)

fun stereotype2string st = "<<"^st^">> "

fun operation2string ({name,arguments,result,precondition,postcondition,...}:Rep.operation) = 
    "  "^name^
    "("^String.concatWith ", " (map argument2string arguments)^
    ") : "^Rep_OclType.string_of_OclType result^"\n"^
    String.concat (map precond2string precondition)^
    String.concat (map postcond2string postcondition)

fun attribute2string ({name,attr_type,...}:Rep.attribute) = 
    "  "^name^" : "^(Rep_OclType.string_of_OclType attr_type)^"\n"

fun parent2string (SOME (p)) = " extends "^Rep_OclType.string_of_OclType p
  | parent2string _ = ""

fun classifier2string (C as Rep.Class x) =
    String.concat (map stereotype2string (#stereotypes x))^
    "class "^Rep.string_of_path (Rep.name_of C)^
    parent2string (#parent x)^
    " {\n"^
    String.concat (map inv2string (#invariant x))^
    String.concat (map attribute2string (#attributes x))^
    String.concat (map operation2string (#operations x))^
    "}\n"
  | classifier2string (C as Rep.AssociationClass x) =
    String.concat (map stereotype2string (#stereotypes x))^
    "associationclass "^Rep.string_of_path (Rep.name_of C)^
    parent2string (#parent x)^
    " {\n"^
    String.concat (map inv2string (#invariant x))^
    String.concat (map attribute2string (#attributes x))^
    String.concat (map operation2string (#operations x))^
    "}\n"
  | classifier2string (C as Rep.Interface x) =
    String.concat (map stereotype2string (#stereotypes x))^
    "interface "^Rep.string_of_path (Rep.name_of C)^"{\n"^
    String.concat (map operation2string (#operations x))^
    "}\n"
  | classifier2string (C as Rep.Primitive x) =
    String.concat (map stereotype2string (#stereotypes x))^
    "primitive "^Rep.string_of_path (Rep.name_of C)^"{\n"^
    String.concat (map operation2string (#operations x))^
    "}\n"
  | classifier2string (C as Rep.Enumeration x) =
    String.concat (map stereotype2string (#stereotypes x))^
    "enum "^Rep.string_of_path (Rep.name_of C)^"{\n"^
    String.concat (map operation2string (#operations x))^
    "}\n"
  | classifier2string (C as Rep.Template x) =
    "template of "^ (classifier2string (#classifier x))

fun printClass (x:Rep.Classifier) = print (classifier2string x)

fun printList (x:Rep.Classifier list) = 
    print (String.concatWith "\n\n" (map classifier2string x ))
end
