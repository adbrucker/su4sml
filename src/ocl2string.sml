(*****************************************************************************
 *          su4sml - a SecureUML repository for SML              
 *                                                                            
 * ocl2string - "pretty printing" for OCL terms
 * Copyright (C) 2005  Achim D. Brucker <brucker@inf.ethz.ch>   
 *                     Jürgen Doser <doserj@inf.ethz.ch>    
 *                                                                            
 * This file is part of su4sml.                                              
 *                                                                            
 * su4sml is free software; you can redistribute it and/or modify it under   
 * the terms of the GNU General Public License as published by the Free       
 * Software Foundation; either version 2 of the License, or (at your option)  
 * any later version.                                                         
 *                                                                            
 * su4sml is distributed in the hope that it will be useful, but WITHOUT ANY 
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS 
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more 
 * details.                                                              
 *                                                                            
 * You should have received a copy of the GNU General Public License along    
 * with this program; if not, write to the Free Software Foundation, Inc.,    
 * 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.                  
 ******************************************************************************)

structure ocl2string = 
struct
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
    in
    case oclterm of 
	(**************************************)
	(* Literal                            *)
	(**************************************)
	(* OCL Boolean                        *)
	Literal (lit, typ)          => if show_types 
					then "("^lit^":"^(string_of_OclType typ)^")" 
					else lit
      | If (cterm,ctyp, tterm,ttyp,eterm,etyp,iftyp) =>  if show_types 
							     then "(if ("^(ocl2string show_types cterm)^":"^(string_of_OclType ctyp)
								  ^") then ("^(ocl2string show_types tterm)^":"^(string_of_OclType ttyp)
								  ^") else ("^(ocl2string show_types eterm)^":"^(string_of_OclType etyp)
								  ^") endif:"^(string_of_OclType iftyp)^")" 
							     else "if "^(ocl2string show_types cterm)
								  ^" then "^(ocl2string show_types tterm)
								  ^" else "^(ocl2string show_types eterm)^" endif"
      | AssociationEndCall(src,ts,p,t)    => error "NOT YET SUPPORTED: AssociationEndCall"
      | AttributeCall(src,ts,p,t)         => error "NOT YET SUPPORTED: AtrributeCall"
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
	=> OclSubString u (ocl2holocl u  src) (ocl2holocl u  b) (ocl2holocl u  e)		 
*)
      | OperationCall (src,styp,["oclLib",classifier,opname],[(arg,atyp)],rtyp) => string_of_oo_infix show_types  src styp opname arg atyp rtyp
      | OperationCall (src,styp,["oclLib",classifier,opname],[],rtyp) =>  string_of_oo_postfix1 show_types src styp opname rtyp


(*      (* OperationCalls to modell and Error *)
      | OperationCall (src,styp,op_name,args,t) => let 
      	    val Op = case (getoperation cl op_name) of
			 SOME(Op) => Op
		       | NONE     => error ("error: unknown OperationCall '"
					    ^(string_of_path op_name)^":("^(string_of_OclType t)
					    ^")'  in ocl2holocl")
	    val m_name = mangled_name_of_op Op
	in 
	    (foldl (fn (t0,t1) => t1$t0) (Const((string_of_path (hc_path operation [m_name]))^".Op",dummyT))
			   (map (fn a => ocl2holocl u (fst a)) ((src,styp)::args)))
	end
 *)
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
      | Let (s,_,_,_,_,_) => error ("error: unknown Let '"^(s)^"'  in ocl2holocl") 
      (**************************************)
      (* OperationWithType                  *)
      (**************************************)
      (* Error                              *)
      | OperationWithType (src,styp,opname,oclType,rtyp) =>  string_of_oo_typeinfix show_types  src styp opname oclType  rtyp
      (**************************************)
      (* Iterate                            *)
      (**************************************)
      (* Error                              *)
      | Iterate (_,s,_,src,_,c,_,_) =>  error ("error: unknown Iterate '"^(s)^"' in in ocl2holocl") 
      (**************************************)
      (* Iterator                           *)
      (**************************************)
      (* forAll                              *)
      | Iterator ("forAll",vars,src,styp,c,_,_) =>  ("error: unknown Iterrator 'forall' in in ocl2holocl") 
      | Iterator ("exists",vars,src,styp,c,_,_) =>  ("error: unknown Iterrator 'exists' in in ocl2holocl")
      | Iterator ("select",vars,src,styp,c,_,_) => ("error: unknown Iterrator 'select' in in ocl2holocl")
      | Iterator ("collect",vars,src,styp,c,_,_) => ("error: unknown Iterrator 'collect' in in ocl2holocl")

      (* OCL Collection                     *) 
   (* 
      | Iterate (src,styp,["oclLib",classifier,"iterate"],args,Collection _) => OclIterate u C be e  
      | Iterate (src,styp,["oclLib",classifier,"isUnique"],args,Collection _) => OclIsUnique  u C be  
      | Iterate (src,styp,["oclLib",classifier,"one"],args,Collection _) => OclOne  u C be       
      | Iterate (src,styp,["oclLib",classifier,"any"],args,Collection _) => OclAny  u C be       
      (* OCL OrderedSet                     *) 
      | Iterate (src,styp,["oclLib",classifier,"count"],[(arg,_)],OrderedSet _) => OclOSetCount u S e       *)
      (* Error                              *)
      | Iterator (s,_,_,_,_,_,_) => error ("error: unknown Iterator '"^(s)^"' in in ocl2holocl") 
      (**************************************)
      (* Catch out                          *)
      (**************************************)
      (* Error                              *)
      | _ => error ("error: unknown OCL-term in in ocl2holocl") 
end
end
