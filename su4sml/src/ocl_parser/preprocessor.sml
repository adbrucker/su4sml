(*****************************************************************************
 * su4sml --- a SML repository for managing (Secure)UML/OCL models
 *             http://projects.brucker.ch/su4sml/
 *                                                                            
 * preprocessor.sml --- 
 * This file is part of su4sml.
 *
 * Copyright (c) 2005-2007, ETH Zurich, Switzerland
 *               2008 Achim D. Brucker, Germany
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

signature PREPROCESSOR = 
sig

    val preprocess_context_list         : Context.context list -> Rep_Core.Classifier list -> Context.context list
    val embed_bound_variables           : (string * Rep_OclType.OclType) list -> Rep_OclTerm.OclTerm -> Rep_OclTerm.OclTerm
end


structure Preprocessor:PREPROCESSOR  = 
 struct
open Rep_Helper
open Rep_Logger
open Rep_Core
open Rep_OclTerm
open Rep_OclType

open Context
open RepParser
open XMI_DataTypes
open OclLibrary


type operation = Rep_Core.operation
type attribute = Rep_Core.attribute





(* The problem here is, that when parsing the ocl file, there is something the parser cannot
   know. Therefore we have to solve the problem before to type check.

   PROBLEM 1:
   The parser can never know if it is an absolute call for an operation or an attribute, or 
   a relative call. 
   Example:    x.hallo >= 0
   Possiblity 1:                                  
	context A::f(x:Object):Integer
            pre: x > 0
        to get classifier x look up in the signature for Object x
   Possiblity 2:
        context A::f(y:Object):Integer
            pre: x > 0
        to get classifier x look up in the classifer list at self.x

   This problem is solve by going through all the contexts, looking whether the attribute calls
   refer to self or signature arguemnts.

   Used methods:
	- member
	- fetch
	- check_for_self

   PROBLEM 2:

   The parser parses a call to result following:
 
      ******************************************************
	  
      post: result = 0
                       
      AttributeCall (Variable("self",_),...,["result"], ...)
      
      ******************************************************

   So we have to substitute this by: Variable ("result",typ_of_operation_return_typ)

   
   PROBLEM 3:
   
   The parser parses the OperationWithType calls wrong.


   PROBLEM 4:
  
   package simple
   context A
   inv : self.i->forAll(a:B | ...)
   endpackage

   but the type is not (Classifier (["B"]) but (Classifier (["simple","B"])


   
   PROBLEM 5: 

   '@pre'-expressions needs to be treated separately 



*)

(*
TEMPLATE FOR TERM:
fun fun_name (Varible (str,type)) = 
  | fun_name (Literal (str,type)) =
  | fun_name (CollectionLiteral (collpart,typ)) = 
  | fun_name (If (cond,cond_type,then_e,then_type,else_e,else_type,res_type)) = 
  | fun_name (AttributeCall (sterm,styp,p,res_typ)) =
  | fun_name (OperationCall (sterm,styp,pa,para,res_typ)) = 
  | fun_name (OperationWithType (sterm,stype,para_name,para_type,res_type)) =
  | fun_name (Let (var_name,var_type,rhs,rhs_type,in_e,in_type)) =
  | fun_name (Iterator (name,iter_vars,sterm,stype,body_e,body_type,res_type)) =
  | fun_name (Iterate (iter_vars,result_var,sterm,stype,body_term,body_type,res)) = 
  | fun_name (AssociatonEndCall(sterm,stype,path,res)) = 
  | fun_name (Predicate (sterm,stype,path,args)) = 
  | fun_name (QualifiedAssociationEndCall(sterm,stype,qualifiers,path,res)) =

*)

(* RETURN: OclTerm *)
fun embed_atPre_expressions_collpart (CollectionItem (term,typ)) = 
    let
	val _ = trace function_calls ("Preprocessor.embed_atPre_expression_collpart CollectionItem(...)\n")
	val res = (CollectionItem (embed_atPre_expressions term,typ))
	val _ = trace function_ends ("Preprocessor.embed_atPre_expression_collpart\n")
    in
	res
    end
  | embed_atPre_expressions_collpart (CollectionRange (term1,term2,typ)) = 
    let
	val _ = trace function_calls ("Preprocessor.embed_atPre_expression_collpart CollectionRange(...)\n")
	val res = (CollectionRange (embed_atPre_expressions term1, embed_atPre_expressions term2, typ))
	val _ = trace function_ends ("Preprocessor.embed_atPre_expression_collpart\n")
    in
	res
    end

and embed_atPre_expressions (Variable (str,typ)) = 
    let
	val _ = trace function_calls ("Preprocessor.embed_atPre_expressions Variable(...)\n")
	val res = (Variable (str,typ))
	val _ = trace function_ends ("Preprocessor.embed_atPre_expressions\n")
    in
	res
    end
  | embed_atPre_expressions (Literal (str,typ)) = 
    let
	val _ = trace function_calls ("Preprocessor.embed_atPre_expressions Literal(...)\n")
	val res = (Literal (str,typ))
	val _ = trace function_ends ("Preprocessor.embed_atPre_expressions\n")
    in
	res
    end
  | embed_atPre_expressions (CollectionLiteral (collpart,typ)) = 
    let
	val _ = trace function_calls ("Preprocessor.embed_atPre_expressions CollectionLiteral(...)\n")
	val res = (CollectionLiteral (List.map (embed_atPre_expressions_collpart) collpart,typ))
	val _ = trace function_ends ("Preprocessor.embed_atPre_expressions\n")
    in
	res
    end
  | embed_atPre_expressions (If (cond,cond_type,then_e,then_type,else_e,else_type,res_type)) =
    let
	val _ = trace function_calls ("Preprocessor.embed_atPre_expressions Variable(...)\n")
	val res = (If (embed_atPre_expressions cond,cond_type,embed_atPre_expressions then_e,then_type,embed_atPre_expressions else_e,else_type,res_type))
	val _ = trace function_ends ("Preprocessor.embed_atPre_expressions\n")
    in
	res
    end
  | embed_atPre_expressions (AttributeCall (sterm,styp,p,res_typ)) =
    let
	val _ = trace function_calls ("Preprocessor.embed_atPre_expressions AttributeCall(...)\n")
	val res =
	    if (List.last (p) = "atPre")
	    then (* atPre Call *)
		(
		 if (List.length p = 1) 	 
		 then (* self *)
		     (OperationCall (sterm,styp,[OclLibPackage,"OclAny","atPre"],[],DummyT))
		 else (* contains at least one 'normal' attribute *)
		     (AttributeCall (OperationCall (embed_atPre_expressions sterm,styp,[OclLibPackage,"OclAny","atPre"],[],DummyT),styp,real_path p,res_typ))
	)
	    else (* normal Call *) 
		(AttributeCall (embed_atPre_expressions sterm,styp,p,res_typ))
	val _ = trace function_ends ("Preprocessor.embed_atPre_expressions\n")
    in
	res
    end
  | embed_atPre_expressions (OperationCall (sterm,styp,pa,para,res_typ)) =
    let
	val _ = trace function_calls ("Preprocessor.embed_atPre_expressions OperationCall(...)\n")
	val atpre_para = List.map (fn (a,b) => (embed_atPre_expressions a,b)) para
	val res = 
	    if (List.last (pa) = "atPre")
	    then (OperationCall (OperationCall (embed_atPre_expressions sterm,styp,real_path pa,atpre_para,res_typ),DummyT,[OclLibPackage,"OclAny","atPre"],[],DummyT))
	    else (OperationCall (embed_atPre_expressions sterm,styp,pa,atpre_para,res_typ))
	val _ = trace function_ends ("Preprocessor.embed_atPre_expressions\n")
    in
	res
    end
  | embed_atPre_expressions (OperationWithType (sterm,stype,para_name,para_type,res_type)) = 
    let
	val _ = trace function_calls ("Preprocessor.embed_atPre_expressions OperationWithType(...)\n")
	val res = (OperationWithType (embed_atPre_expressions sterm,stype,para_name,para_type,res_type))
	val _ = trace function_ends ("Preprocessor.embed_atPre_expressions\n")
    in
	res
    end
  | embed_atPre_expressions (Let (var_name,var_type,rhs,rhs_type,in_e,in_type)) =
    let
	val _ = trace function_calls ("Preprocessor.embed_atPre_expressions Let(...)\n")
	val res = (Let (var_name,var_type,embed_atPre_expressions rhs,rhs_type,embed_atPre_expressions in_e,in_type))
	val _ = trace function_ends ("Preprocessor.embed_atPre_expressions\n")
    in
	res
    end
  | embed_atPre_expressions (Iterator (name,iter_vars,sterm,stype,body_e,body_type,res_type)) = 
    let
	val _ = trace function_calls ("Preprocessor.embed_atPre_expressions Iterator(...)\n")
	val res = (Iterator (name,iter_vars,embed_atPre_expressions sterm,stype,embed_atPre_expressions body_e,body_type,res_type))
	val _ = trace function_ends ("Preprocessor.embed_atPre_expressions\n")
    in
	res
    end
  | embed_atPre_expressions (Iterate (iter_vars,acc_var_name,acc_var_type,acc_var_term,sterm,stype,bterm,btype,res_type)) = 
    let
	val _ = trace function_calls ("Preprocessor.embed_atPre_expressions Iterate(...)\n")
	val res = (Iterate (iter_vars,acc_var_name,acc_var_type,acc_var_term,embed_atPre_expressions sterm,stype,embed_atPre_expressions bterm,btype,res_type))
	val _ = trace function_ends ("Preprocessor.embed_atPre_expression\n")
    in
	res
    end
(* RETURN: OclTerm *)
fun embed_bound_variable (str,typ) (Variable(s,t)) = 
    let
	val _ = trace function_calls ("Preprocessor.embed_bound_variable Variable(...)\n")
	val _ = trace preprocessor ("1 Bound variable '" ^ s ^ "' in 'AttributeCall': " ^ Ocl2String.ocl2string false (Variable(s,t)) ^ "\n")
	val res = 
	    if (str = s ) then
		Variable(s,typ)
	    else
		Variable(s,t)
	val _ = trace function_ends ("Preprocessor.embed_bound_variable\n")
    in
	res
    end
  | embed_bound_variable (s,typ) (AttributeCall (sterm,styp,path,rtyp)) =
    let
        val _ = trace function_calls ("Preprocessor.embed_bound_variable AttributeCall(...)\n")
	val _ = trace preprocessor ("2 Bound variable '" ^ s ^ "' in 'AttributeCall': " ^ Ocl2String.ocl2string false (AttributeCall (sterm,styp,path,rtyp)) ^ "\n")
	val res = 
	    if (List.last path = s) then
		(* embed variable *)
		(Variable (s,typ))
	    else
		(AttributeCall (embed_bound_variable (s,typ) sterm,styp,path,rtyp))
	val _ = trace function_ends ("Preprocessor.embed_bound_variable\n")
    in
	res
    end
  | embed_bound_variable (s,typ) (OperationCall (sterm,styp,path,args,rtyp)) =
    let
	val _ = trace function_calls ("Preprocessor.embed_bound_variable AttributeCall(...)\n")
	val _ = trace preprocessor ("Bound variable '" ^ s ^ "' in 'OperationCall': " ^ Ocl2String.ocl2string false (OperationCall (sterm,styp,path,args,rtyp)) ^ "\n")
	val res = (OperationCall (embed_bound_variable (s,typ) sterm,styp,path,embed_bound_args (s,typ) args ,rtyp))
	val _ = trace function_ends ("Preprocessor.embed_bound_variable\n")
    in
	res
    end
  | embed_bound_variable (s,typ) (Iterator (name,iter_list,sterm,styp,expr,expr_typ,rtyp)) =
  let
      val _ = trace function_calls ("Preprocessor.embed_bound_variable AttributeCall(...)\n")
      val _ = trace preprocessor ("Bound variable '" ^ s ^ "' in 'Iterator': " ^ Ocl2String.ocl2string false (Iterator (name,iter_list,sterm,styp,expr,expr_typ,rtyp)) ^ "\n")
      val res = (Iterator (name,iter_list,embed_bound_variable (s,typ) sterm,styp,embed_bound_variables iter_list (embed_bound_variable (s,typ) expr),expr_typ,rtyp))
      val _ = trace function_ends ("Preprocessor.embed_bound_variable\n")
  in
      res
  end
| embed_bound_variable (s,typ) (Iterate (iter_vars,acc_name,acc_type,acc_term,sterm,stype,bterm,btype,res_type)) =
  let
      val _ = trace function_calls ("Preprocessor.embed_bound_variable AttributeCall(...)\n")
      val  _ = trace medium ("Bound variable '" ^ s ^ "' in 'Iterate': " ^ Ocl2String.ocl2string false (Iterate (iter_vars,acc_name,acc_type,acc_term,sterm,stype,bterm,btype,res_type)) ^ "\n")
      val res = (Iterate (iter_vars,acc_name,acc_type,acc_term,embed_bound_variable (s,typ) sterm,stype,embed_bound_variable (s,typ) bterm,btype,res_type))
      val _ = trace function_ends ("Preprocessor.embed_bound_variable\n")
  in
      res
  end
| embed_bound_variable (s,typ) (Let (var_name,var_type,rhs,rhs_type,in_e,in_type)) =
  let
      val _ = trace function_calls ("Preprocessor.embed_bound_variable AttributeCall(...)\n")
      val _ = trace preprocessor ("Bound variable '" ^ s ^ "' in 'Let': " ^ Ocl2String.ocl2string false (Let (var_name,var_type,rhs,rhs_type,in_e,in_type)) ^ "\n")
      val embed_in_e = embed_bound_variable (var_name,var_type) in_e
      val res = (Let (var_name,var_type,embed_bound_variable (s,typ) rhs,rhs_type,embed_bound_variable (s,typ) embed_in_e,in_type))
      val _ = trace function_ends ("Preprocessor.embed_bound_variable\n")
  in
      res
  end
| embed_bound_variable (s,typ) (If (cond,cond_type,then_e,then_type,else_e,else_type,res_type)) =
  let
      val _ = trace function_calls ("Preprocessor.embed_bound_variable AttributeCall(...)\n") 
      val _ = trace preprocessor ("Bound variable '" ^ s ^ "' in 'If'  ..." ^ "\n")
      val res = (If (embed_bound_variable (s,typ) cond,cond_type,embed_bound_variable (s,typ) then_e,then_type,embed_bound_variable (s,typ) else_e,else_type,res_type))
      val _ = trace function_ends ("Preprocessor.embed_bound_variable\n")
  in
      res
  end
| embed_bound_variable (s,typ) term = term

(* RETURN: IDENDITAET *)
and swap f a b = f b a   

(* RETURN: OclTerm *)
and embed_bound_variables [] term = term
  | embed_bound_variables (h::tail) term = 
    (embed_bound_variables tail (embed_bound_variable h term))


(* RETURN: (OclTerm * OclType) list *)
and embed_bound_args (str,typ) [] = []
  | embed_bound_args (str,typ) (h::arg_list) =
    (embed_bound_variable (str,typ) (#1 h),#2 h)::(embed_bound_args (str,typ) arg_list)

(* RETURN: OclTerm *)
(* Better readable source code *)
fun embed_method_arguments [] term = term
  | embed_method_arguments ((str,typ)::tail) term = 
    embed_method_arguments tail (embed_bound_variable (str,typ) term)

(* RETURN: OclTerm *)
fun embed_iterator_variables arg_list term = embed_method_arguments arg_list term



(* For the existing variables in Ocl.
   There is only one (I know, maybe some more).
   It can be easily extended here 
*)
(* RETURN: CollectionPart list  *)
fun generate_variables_coll_list  ((CollectionItem (term,typ))::tail) path meth_name model 
  = (CollectionItem(generate_variables term path meth_name model,typ))
    ::(generate_variables_coll_list tail path meth_name model)
  | generate_variables_coll_list ((CollectionRange (first_term,last_term,typ))::tail) 
				 path meth_name model 
    = (CollectionRange(generate_variables 
			   first_term path meth_name model, 
		       generate_variables last_term path meth_name model,typ))
      ::(generate_variables_coll_list tail path meth_name model)
  | generate_variables_coll_list [] path meth_name model =  []

(* RETURN: OclTerm *)
and generate_variables (Literal (paras)) path meth_name model = Literal (paras)
  | generate_variables (Variable (paras)) path meth_name model = Variable (paras)
  | generate_variables (CollectionLiteral ([],dummyT)) path meth_name model = 
    (CollectionLiteral  ([],Set OclAny)) (* HACK/ DefaultType for Empty List: OclAny *)
  | generate_variables (CollectionLiteral (collpart_list,typ)) path meth_name model = 
    (CollectionLiteral  (generate_variables_coll_list collpart_list path meth_name model,typ))

  | generate_variables (If (cond,cond_type,then_e,then_type,else_e,else_type,res_type)) path meth_name model= 
    (If (generate_variables cond path meth_name model,cond_type,generate_variables then_e path meth_name model,then_type,generate_variables else_e path meth_name model,else_type,res_type))
  | generate_variables (AttributeCall (src,src_type,["result"],_)) path meth_name model =
    let
	val _ = trace function_calls ("Preprocessor.generate_variables: AttributeCall\n")
	val new_src = generate_variables src path meth_name model	
	val _ = List.app (print o (fn x => x^"\n") o string_of_path o name_of ) model
	val classifier = class_of path (model,[])
	val _ = trace low "classifier found\n"
	val meth = get_operation meth_name classifier (model,[])
	val res = (Variable ("result",(#result (meth))))
	val _ = trace function_ends ("Preprocessor.generate_variables\n")
    in
	res
    end
  | generate_variables (AttributeCall (sterm,styp,p,res_typ)) path meth_name model =
    (AttributeCall (generate_variables sterm path meth_name model,styp,p,res_typ))
  | generate_variables (OperationCall (sterm,styp,pa,paras,res_typ)) path meth_name model = 
    let 
	val _ = trace function_calls ("Preprocessor.generate_variables \n")
	val new_para_terms = List.map (fn (a,b) => generate_variables (a) path meth_name model) paras
	val new_paras = List.map (fn a => (a, type_of_term a)) new_para_terms
	val res =  
	    (OperationCall (generate_variables sterm path meth_name model,styp,pa,new_paras,res_typ))
	val _ = trace function_ends ("Preprocessor.generate_variables\n")
    in
	res
    end
  | generate_variables (OperationWithType (sterm,stype,para_name,para_type,res_typ)) path meth_name model =
    let
	val _ = trace function_calls ("Preprocessor.generate_variables \n")
	val res =  
	    (OperationWithType (generate_variables sterm path meth_name model,stype,para_name,para_type,res_typ))
	val _ = trace function_ends ("Preprocessor.generate_variables\n")
    in
	res
    end
  | generate_variables (Let (var_name,var_type,rhs,rhs_type,in_e,in_type)) path meth_name model =
    (Let (var_name,var_type,generate_variables rhs path meth_name model,rhs_type,generate_variables in_e path meth_name model,in_type))
  | generate_variables (Iterator (name,iter_vars,sterm,stype,body_e,body_type,res_type)) path meth_name model = 
    (Iterator (name,iter_vars,generate_variables sterm path meth_name model,stype,generate_variables body_e path meth_name model,body_type,res_type))

(* RETURN: (string*OclType) *)
fun fetch (x,((y1,y2)::tail)) =
    if (x=y1) then
	(y1,y2)
    else
	fetch (x,tail)

(* RETURN: OclTerm list *)
fun check_for_self_paras arg_list typ [] model = []
| check_for_self_paras arg_list typ ((term,t)::tail) model = 
  let
      val _ = trace function_calls ("Preprocessor.check_for_self_paras\n")
      val res = ((check_for_self arg_list typ term model),t)::(check_for_self_paras arg_list typ tail model)
      val _ = trace function_ends ("Preprocessor.check_for_self_paras\n")
  in
      res
  end

and check_for_self_collpart  arg_list typ model (CollectionItem (term,ctyp)) = 
    let
	val _ = trace function_calls ("Preprocessor.check_for_self_collpart CollectionItem(...)\n")
	val res = (CollectionItem (check_for_self arg_list typ term model,ctyp))
	val _ = trace function_ends ("Preprocessor.check_for_self_collpart\n")
    in
	res
    end
  | check_for_self_collpart arg_list typ model (CollectionRange (term1,term2,ctyp)) = 
    let
	val _ = trace function_calls ("Preprocessor.check_for_self_collpart CollectionRange(...)\n")
	val res = (CollectionRange (check_for_self arg_list typ term1 model, 
				    check_for_self arg_list typ term2 model, ctyp))
	val _ = trace function_ends ("Preprocessor.check_for_self_collpart\n")
    in
	res
    end

(* RETURN: OclTerm *)
and check_for_self arg_list typ (AttributeCall (Variable("dummy_source",_),_,path,_))  model=
    let
	val _ = trace function_calls ("Preprocessor.check_for_self: dummy_source AttributeCall\n")
	val test = (member (List.last path) (List.map (#1) arg_list))
        val _ = trace preprocessor ("member? "^ Bool.toString (test) ^ "\n")
	val res = 
	    if (List.last path = "self") then
		(* 'self' is writen in the ocl file *)
		(Variable ("self",typ))
	    else
		(AttributeCall (Variable ("self",typ),DummyT,path,DummyT))
	val _ = trace function_ends ("Preprocessor.check_for_self\n")
    in
	res
    end

  | check_for_self arg_list typ (CollectionLiteral (collpart,ctyp)) model =
    let
 	val _ = trace function_calls ("Preprocessor.check_for_self: dummy_source CollectionLiteral\n")

	val res = (CollectionLiteral (List.map (check_for_self_collpart arg_list typ model) collpart,ctyp))
 	val _ = trace function_ends ("Preprocessor.check_for_self\n")
    in
      res
    end

| check_for_self arg_list typ (AttributeCall (source_term,source_typ,path,ret_typ)) model = 
  let
      val _ = trace function_calls ("Preprocessor.check_for_self: complex AttributeCall\n")
      val res = (AttributeCall (check_for_self arg_list typ source_term model,source_typ,path,ret_typ))
      val _ = trace function_ends ("Preprocessor.check_for_self\n")
  in
      res
  end
(* OperationCall *)
| check_for_self arg_list typ (OperationCall (Variable ("dummy_source",_),source_type,path,paras,ret_typ)) model = 
  let
      val test = (member (List.last path) (List.map (#1) arg_list))
      val _ = trace preprocessor ("member2? "^ Bool.toString (test) ^ "\n")
  in
      if (member (List.last path) (List.map (#1) arg_list)) 
      then
	  (* Call of a method parameter *)
	  (Variable ((#1 (fetch ((List.last path), arg_list))), (#2 (fetch ((List.last path),arg_list)))))
      else
	  (* Call of a member of the class *)
	  (OperationCall (Variable ("self",typ),source_type,path,check_for_self_paras arg_list typ paras model,ret_typ))
  end
| check_for_self arg_list typ (OperationCall (source_term,source_typ,path,paras,ret_typ))  model = 
  let
      val _ = trace function_calls ("Preprocessor.check_for_self complex OperationCall\n")
      val res = (OperationCall (check_for_self arg_list typ source_term model ,source_typ,path,check_for_self_paras arg_list typ paras model,ret_typ))
      val _ = trace function_ends ("Preprocessor.check_for_self\n")
  in
      res
  end
| check_for_self arg_list typ (Iterator (name,iter_var,sterm,styp,expr,expr_typ,res_typ)) model = 
  let
      val _ = trace function_calls ("Preprocessor.check_for_self: Iterator(...)\n")
      val res = (Iterator (name,iter_var,(check_for_self arg_list typ sterm model),styp,(check_for_self arg_list typ expr model),expr_typ,res_typ))
      val _ = trace function_ends ("Preprocessor.check_for_self\n")
  in
      res
  end
| check_for_self arg_list typ (Iterate (iter_vars,acc_name,acc_type,acc_term,sterm,stype,bterm,btype,res_type)) model = 
  let
      val _ = trace function_calls ("Preprocessor.check_for_self  Iterate \n")
      val res = (Iterate (iter_vars,acc_name,acc_type,acc_term,(check_for_self arg_list typ sterm model),stype,(check_for_self arg_list typ bterm model),btype,res_type))
      val _ = trace function_ends("Preprocessor.check_for_self\n")
  in
      res    
  end
| check_for_self arg_list typ (Let (str,ttyp,rhs_term,rhs_typ,in_term,in_typ)) model =
  let
      val _ = trace function_calls ("Preprocessor.check_for_self Let (...)\n")
      val self_rhs_term = check_for_self arg_list typ rhs_term model
      val self_in_term = check_for_self arg_list typ in_term model
      val res = (Let (str,ttyp,self_rhs_term,rhs_typ,self_in_term,in_typ))
      val _ = trace function_ends ("Preprocessor.check_for_self\n")
  in
      res
  end
| check_for_self arg_list typ (If (cond,cond_typ,expr1,typ1,expr2,typ2,res_typ)) model =
  let
      val _ = trace function_calls ("Preprocessor.check_for_self If (...)\n")
      val self_cond = check_for_self arg_list typ cond model
      val self_expr1 = check_for_self arg_list typ expr1 model
      val self_expr2 = check_for_self arg_list typ expr2 model
      val res = (If (self_cond,cond_typ,self_expr1,typ1,self_expr2,typ2,res_typ))
      val _ = trace function_ends ("Preprocessor.check_for_self\n")
  in 
      res
  end
| check_for_self arg_list typ term model = term




(* 
fun prefix_OperationWithType_CollectionPart prefix (CollectionItem(term,typ)) = 
    let
	val new_term = prefix_OperationWithType prefix term
    in
	(CollectionItem(new_term,type_of_term new_term))
    end
  | prefix_OperationWithType_CollectionPart prefix (CollectionRange(term1,term2,typ)) = 
    let
	val new_term1 = prefix_OperationWithType prefix term1
	val new_term2 = prefix_OperationWithType prefix term2
    in
	(CollectionRange(new_term1,new_term2,type_of_term new_term1))
    end

and prefix_OperationWithType prefix (Variable (str,typ)) = (Variable (str,typ))
  | prefix_OperationWithType prefix (Literal (str,typ)) = (Literal (str,typ))
  | prefix_OperationWithType prefix (CollectionLiteral (collparts,typ)) =
    let
	val new_collparts = List.map (fn a => prefix_OperationWithType_CollectionPart prefix a) collparts
	val new_type = type_of_CollPart (List.hd(new_collparts))
    in
	CollectionLiteral(new_collparts,new_type)
    end
  | prefix_OperationWithType prefix (If (cond,cond_type,then_e,then_type,else_e,else_type,res_type)) = 
    let
	val new_cond = prefix_OperationWithType prefix cond
	val new_cond_type = type_of_term new_cond
	val new_then_e = prefix_OperationWithType prefix then_e
	val new_then_type = type_of_term new_then_e
	val new_else_e = prefix_OperationWithType prefix else_e
	val new_else_type = type_of_term new_else_e
	val new_res_type = new_then_type
    in
	(If (new_cond,new_cond_type,new_then_e,new_then_type,new_else_e,new_else_type,new_res_type))
    end
  | prefix_OperationWithType prefix (QualifiedAssociationEndCall(sterm,stype,qualifiers,path,res)) =
    let
	val new_term = prefix_OperationWithType prefix sterm
	val new_type = type_of_term new_term
	val new_qualifiers = List.map (fn (a,b) => 
					  let
					      val new_a = prefix_OperationWithType prefix a
					      val new_a_type = type_of_term a
					  in
					      (new_a,new_a_type)
					  end
				      ) qualifiers
    in
	QualifiedAssociationEndCall(new_term,new_type,new_qualifiers,path,res)
    end
  | prefix_OperationWithType prefix (AttributeCall (sterm,styp,p,res_typ)) =
    let
	val new_term = prefix_OperationWithType prefix sterm
	val new_type = type_of_term new_term
    in
	AttributeCall(new_term,new_type,p,res_typ)
    end
  | prefix_OperationWithType prefix (AssociationEndCall(sterm,stype,path,res)) = 
    let
	val new_term = prefix_OperationWithType prefix sterm
	val new_type = type_of_term new_term
    in
	AssociationEndCall(new_term,new_type,path,res)
    end

  | prefix_OperationWithType prefix (OperationCall (sterm,styp,pa,args,res_typ)) = 
    (* if it is an OperationWithType,
     * then it gets parsed like:
     * 
     *
     *     OperationCall 
     *	         (source_term,
     *		  DummyT,
     *		  ["oclIsTypeOf"], 
     *		  [(AttributeCall(Variable ("dummy_source",DummyT),DummyT,["Chair"],DummyT),DummyT)],
     *		  DummyT)
     *)
    (
     case args of 
	 [(AttributeCall(Variable("dummy_source",DummyT),DummyT,path,DummyT),DummyT)] => 
         (** OperationWithType **)
	 let
	     val new_term = prefix_OperationWithType prefix sterm
	     val new_type = type_of_term new_term
	     val new_path = prefix_path prefix path 
	     val new_args = [(AttributeCall(Variable("dummy_source",DummyT),DummyT,new_path,DummyT),DummyT)]
	 in
	     (OperationCall(new_term,new_type,pa,new_args,res_typ))
	 end
       | x =>
	 (** OperationCall **)
	 let
	     val new_term = prefix_OperationWithType prefix sterm
	     val new_type = type_of_term new_term
	     val new_args = List.map (fn (a,b) => 
					 let
					     val new_a = prefix_OperationWithType prefix a
					     val new_a_type = type_of_term new_a
					 in
					     (new_a,new_a_type)
					 end ) args
	in
	     OperationCall(new_term,new_type,pa,new_args,res_typ)
	 end
    )
  | prefix_OperationWithType prefix (Predicate (sterm,stype,path,args)) = 
    let
	val new_term = prefix_OperationWithType prefix sterm
	val new_type = type_of_term new_term
	val new_args = List.map (fn (a,b) => 
				    let
					val new_a = prefix_OperationWithType prefix a
					val new_a_type = type_of_term new_a
				    in
					(new_a,new_a_type)
				    end ) args
    in
	Predicate (new_term,new_type,path,new_args)
    end
  | prefix_OperationWithType prefix (Let (var_name,var_type,rhs,rhs_type,in_e,in_type)) =
    let
	val new_rhs = prefix_OperationWithType prefix rhs
	val new_rhs_type = type_of_term new_rhs
	val new_in_e = prefix_OperationWithType prefix in_e
	val new_in_type = type_of_term new_in_e
    in
	(Let(var_name,var_type,new_rhs,new_rhs_type,new_in_e,new_in_type))
    end
  | prefix_OperationWithType prefix (Iterator (name,iter_vars,sterm,stype,body_e,body_type,res_type)) =
    let
	val new_term = prefix_OperationWithType prefix sterm
	val new_type = type_of_term new_term
	val new_body = prefix_OperationWithType prefix body_e
	val new_body_type = type_of_term new_body
	val new_res_type = res_type
    in
	Iterator(name,iter_vars,new_term,new_type,new_body,new_body_type,res_type)
    end
  | prefix_OperationWithType prefix (Iterate (iter_vars,res_string,res_type,res_term,sterm,stype,body_term,body_type,res)) = 
    let
	val new_term = prefix_OperationWithType prefix sterm
	val new_type = type_of_term new_term
	val new_res_term = prefix_OperationWithType prefix res_term
	val new_body = prefix_OperationWithType prefix body_term
	val new_body_type = type_of_term new_body
    in
	Iterate(iter_vars,res_string,res_type,new_res_term,new_term,new_type,new_body,new_body_type,res)
    end
*)

(* RETURN: Context *)
fun preprocess_context (Cond (path,op_name,op_sign,result_type,cond,pre_name,expr)) model = 
    let
	(* embed 'result' variable *)
	val _ = trace function_calls ("Preprocessor.preprocess_context Cond(...)\n")
	val _ = trace preprocessor ("Embed result variable \n")
	val vexpr = generate_variables expr path op_name model
	val _ = trace preprocessor ("Variable 'result' embeded ... \n")
	(* embed method arguments *)
	val class = class_of_type  (Classifier (path)) (model,[])
	val prfx  = package_of class
	val prefixed_op_sign = List.map (fn (a,b) => (a,prefix_type prfx  b)) op_sign
	val prefixed_result_type = prefix_type prfx result_type
	val eexpr = embed_method_arguments prefixed_op_sign vexpr
        (* embed '@pre'-expressions *)
	val pexpr = embed_atPre_expressions eexpr
	val res = 
	    (Cond (path,op_name,prefixed_op_sign,prefixed_result_type,cond,pre_name,(check_for_self prefixed_op_sign (Classifier (path)) pexpr model)))
	val _ = trace function_ends ("Preprocessor.preprocess_context\n")
    in
	res
    end
| preprocess_context (Inv (path,string,term)) model =
    let
	val _ = trace function_calls ("Preprocessor.preprocess_context Inv (...)\n")
        (* embed '@pre'-expressions *)
	val pexpr = embed_atPre_expressions term
	val res = (Inv (path,string,(check_for_self [] (Classifier (path)) pexpr model)))
	val _ = trace function_ends ("Preprocessor.preprocess_context\n")
    in
	res
    end
| preprocess_context (Attr (path,typ,aoa,expr)) model = 
    let
	val _ = trace function_calls ("Preprocessor.preprocess_context Attr(...)\n")
        (* embed '@pre'-expressions *)
	val pexpr = embed_atPre_expressions expr
	val res = (Attr (path,typ,aoa,check_for_self [] (Classifier (path)) pexpr model))
	val _ = trace function_ends ("Preprocessor.preprocess_context\n")
    in
	res
    end
| preprocess_context c  model = 
    let
	val _ = trace function_calls ("Preprocessor.preprocess_context: others" ^ "\n")
	val res = c
	val _ = trace function_ends ("Preprocessor.preprocess_context\n")
    in
	res
    end

(* RETURN: Context list *)
fun preprocess_context_list [] model = [] 
  | preprocess_context_list (h::context_list_tail) model = 
    let
	val _ = trace function_calls ("Preprocessor.preprocess_context_list\n")
	val res = (preprocess_context h model)::(preprocess_context_list context_list_tail model)
	val _ = trace function_ends ("Preprocessor.preprocess_context_list\n")
    in
	res
    end
 end
