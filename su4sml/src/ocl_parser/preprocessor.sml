(*****************************************************************************
 * su4sml --- a SML repository for managing (Secure)UML/OCL models
 *             http://projects.brucker.ch/su4sml/
 *                                                                            
 * preprocessor.sml --- 
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

signature PREPROCESSOR = 
sig

    val preprocess_context_list         : Context.context list -> Rep_Core.Classifier list -> Context.context list
    val embed_bound_variables           : (string * Rep_OclType.OclType) list -> Rep_OclTerm.OclTerm -> Rep_OclTerm.OclTerm
end


structure Preprocessor:PREPROCESSOR  = 
 struct
open Context
open Rep_OclTerm
open Rep_OclType
open Rep_Core
open RepParser
open XMI_DataTypes
open OclLibrary
open Ext_Library

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
fun embed_atPre_expressions (Varible (str,type)) = 
  | embed_atPre_expressions (Literal (str,type)) =
  | embed_atPre_expressions (CollectionLiteral (collpart,typ)) = 
  | embed_atPre_expressions (If (cond,cond_type,then_e,then_type,else_e,else_type,res_type)) path meth_name model= 
  | embed_atPre_expressions (AttributeCall (sterm,styp,p,res_typ)) path meth_name model =
  | embed_atPre_expressions (OperationCall (sterm,styp,pa,para,res_typ)) path meth_name model = 
  | embed_atPre_expressions (OperationWithType (sterm,stype,para_name,para_type,res_type)) path meth_name model = 
  | embed_atPre_expressions (Let (var_name,var_type,rhs,rhs_type,in_e,in_type)) path meth_name model =
  | embed_atPre_expressions (Iterator (name,iter_vars,sterm,stype,body_e,body_type,res_type)) path meth_name model = 
*)

(* RETURN: bool *)
fun member x [] = false
| member x (h::tail) = 
    if (x = h) then
	true
    else 
	member x tail

(* RETURN: OclTerm *)
fun embed_atPre_expressions_collpart (CollectionItem (term,typ)) = 
    (CollectionItem (embed_atPre_expressions term,typ))
  | embed_atPre_expressions_collpart (CollectionRange (term1,term2,typ)) = 
    (CollectionRange (embed_atPre_expressions term1, embed_atPre_expressions term2, typ))

and embed_atPre_expressions (Variable (str,typ)) = (Variable (str,typ))
  | embed_atPre_expressions (Literal (str,typ)) = (Literal (str,typ))
  | embed_atPre_expressions (CollectionLiteral (collpart,typ)) = 
    (CollectionLiteral (List.map (embed_atPre_expressions_collpart) collpart,typ))
  | embed_atPre_expressions (If (cond,cond_type,then_e,then_type,else_e,else_type,res_type)) =
    (If (embed_atPre_expressions cond,cond_type,embed_atPre_expressions then_e,then_type,embed_atPre_expressions else_e,else_type,res_type))
  | embed_atPre_expressions (AttributeCall (sterm,styp,p,res_typ)) =
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
  | embed_atPre_expressions (OperationCall (sterm,styp,pa,para,res_typ)) =
    let
	val atpre_para = List.map (fn (a,b) => (embed_atPre_expressions a,b)) para
    in
	if (List.last (pa) = "atPre")
	then (OperationCall (OperationCall (embed_atPre_expressions sterm,styp,real_path pa,atpre_para,res_typ),DummyT,[OclLibPackage,"OclAny","atPre"],[],DummyT))
	else (OperationCall (embed_atPre_expressions sterm,styp,pa,atpre_para,res_typ))
    end
  | embed_atPre_expressions (OperationWithType (sterm,stype,para_name,para_type,res_type)) = 
    (OperationWithType (embed_atPre_expressions sterm,stype,para_name,para_type,res_type))
  | embed_atPre_expressions (Let (var_name,var_type,rhs,rhs_type,in_e,in_type)) =
    (Let (var_name,var_type,embed_atPre_expressions rhs,rhs_type,embed_atPre_expressions in_e,in_type))
  | embed_atPre_expressions (Iterator (name,iter_vars,sterm,stype,body_e,body_type,res_type)) = 
    (Iterator (name,iter_vars,embed_atPre_expressions sterm,stype,embed_atPre_expressions body_e,body_type,res_type))
  | embed_atPre_expressions (Iterate (iter_vars,acc_var_name,acc_var_type,acc_var_term,sterm,stype,bterm,btype,res_type)) = 
    (Iterate (iter_vars,acc_var_name,acc_var_type,acc_var_term,embed_atPre_expressions sterm,stype,embed_atPre_expressions bterm,btype,res_type))
(* RETURN: OclTerm *)
fun embed_bound_variable (str,typ) (Variable(s,t)) = 
    let
	val _ = trace zero ("1 Bound variable '" ^ s ^ "' in 'AttributeCall': " ^ Ocl2String.ocl2string false (Variable(s,t)) ^ "\n")
    in
	if (str = s ) then
	    Variable(s,typ)
	else
	    Variable(s,t)
    end
| embed_bound_variable (s,typ) (AttributeCall (sterm,styp,path,rtyp)) =
  let
	val _ = trace zero ("2 Bound variable '" ^ s ^ "' in 'AttributeCall': " ^ Ocl2String.ocl2string false (AttributeCall (sterm,styp,path,rtyp)) ^ "\n")
  in
      if (List.last path = s) then
	  (* embed variable *)
	  (Variable (s,typ))
      else
	  (AttributeCall (embed_bound_variable (s,typ) sterm,styp,path,rtyp))
  end
| embed_bound_variable (s,typ) (OperationCall (sterm,styp,path,args,rtyp)) =
  let
      	val _ = trace zero ("Bound variable '" ^ s ^ "' in 'OperationCall': " ^ Ocl2String.ocl2string false (OperationCall (sterm,styp,path,args,rtyp)) ^ "\n")
  in
      (OperationCall (embed_bound_variable (s,typ) sterm,styp,path,embed_bound_args (s,typ) args ,rtyp))
  end
| embed_bound_variable (s,typ) (Iterator (name,iter_list,sterm,styp,expr,expr_typ,rtyp)) =
  let
      	val _ = trace zero ("Bound variable '" ^ s ^ "' in 'Iterator': " ^ Ocl2String.ocl2string false (Iterator (name,iter_list,sterm,styp,expr,expr_typ,rtyp)) ^ "\n")
  in
      (Iterator (name,iter_list,embed_bound_variable (s,typ) sterm,styp,embed_bound_variables iter_list (embed_bound_variable (s,typ) expr),expr_typ,rtyp))
  end
| embed_bound_variable (s,typ) (Iterate (iter_vars,acc_name,acc_type,acc_term,sterm,stype,bterm,btype,res_type)) =
  let
      val  _ = trace medium ("Bound variable '" ^ s ^ "' in 'Iterate': " ^ Ocl2String.ocl2string false (Iterate (iter_vars,acc_name,acc_type,acc_term,sterm,stype,bterm,btype,res_type)) ^ "\n")
  in
     (Iterate (iter_vars,acc_name,acc_type,acc_term,embed_bound_variable (s,typ) sterm,stype,embed_bound_variable (s,typ) bterm,btype,res_type))
  end
| embed_bound_variable (s,typ) (Let (var_name,var_type,rhs,rhs_type,in_e,in_type)) =
  let
      val _ = trace zero ("Bound variable '" ^ s ^ "' in 'Let': " ^ Ocl2String.ocl2string false (Let (var_name,var_type,rhs,rhs_type,in_e,in_type)) ^ "\n")
      val embed_in_e = embed_bound_variable (var_name,var_type) in_e
  in
      (Let (var_name,var_type,embed_bound_variable (s,typ) rhs,rhs_type,embed_bound_variable (s,typ) embed_in_e,in_type))
  end
| embed_bound_variable (s,typ) (If (cond,cond_type,then_e,then_type,else_e,else_type,res_type)) =
  let 
      val _ = trace zero ("Bound variable '" ^ s ^ "' in 'If'  ..." ^ "\n")
  in
      (If (embed_bound_variable (s,typ) cond,cond_type,embed_bound_variable (s,typ) then_e,then_type,embed_bound_variable (s,typ) else_e,else_type,res_type))
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
fun generate_variables_coll_list  ((CollectionItem (term,typ))::tail)  path meth_name model = 
    (CollectionItem(generate_variables term path meth_name model,typ))::(generate_variables_coll_list tail path meth_name model)
  | generate_variables_coll_list ((CollectionRange (first_term,last_term,typ))::tail) path meth_name model =
    (CollectionRange(generate_variables first_term path meth_name model,generate_variables last_term path meth_name model,typ))::(generate_variables_coll_list tail path meth_name model)

(* RETURN: OclTerm *)
and generate_variables (Literal (paras)) path meth_name model = Literal (paras)
  | generate_variables (Variable (paras)) path meth_name model = Variable (paras)
  | generate_variables (CollectionLiteral (collpart_list,typ)) path meth_name model = 
    (CollectionLiteral  (generate_variables_coll_list collpart_list path meth_name model,typ))
  | generate_variables (If (cond,cond_type,then_e,then_type,else_e,else_type,res_type)) path meth_name model= 
    (If (generate_variables cond path meth_name model,cond_type,generate_variables then_e path meth_name model,then_type,generate_variables else_e path meth_name model,else_type,res_type))
  | generate_variables (AttributeCall (_,_,["result"],_)) path meth_name model =
    let
	val _ = trace low ("generate_variable: AttributeCall\n")
	val _ = List.app (print o (fn x => x^"\n") o string_of_path o name_of ) model
	val classifier = class_of path model
	val _ = trace low "classifier found\n"
	val meth_list = operations_of  classifier
	val meth = find_operation meth_name meth_list
	val _ = trace zero ("a result call resolved ..." ^ "\n")
    in
	(Variable ("result",(#result meth)))
    end
  | generate_variables (AttributeCall (sterm,styp,p,res_typ)) path meth_name model =
    (AttributeCall (generate_variables sterm path meth_name model,styp,p,res_typ))
  | generate_variables (OperationCall (sterm,styp,pa,para,res_typ)) path meth_name model = 
    let 
	val _ = print ("recursive embed 'result' ... \n")
    in 
	(OperationCall (generate_variables sterm path meth_name model,styp,pa,para,res_typ))
    end
  | generate_variables (OperationWithType (sterm,stype,para_name,para_term,res_type)) path meth_name model = 
    (OperationWithType (generate_variables sterm path meth_name model,stype,para_name,para_term,res_type))
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
| check_for_self_paras arg_list typ ((term,t)::tail) model = ((check_for_self arg_list typ term model),t)::(check_for_self_paras arg_list typ tail model)
and

(* RETURN: OclTerm *)
check_for_self arg_list typ (AttributeCall (Variable("dummy_source",_),_,path,_))  model=
    let
	val test = (member (List.last path) (List.map (#1) arg_list))
        val _ = trace zero ("member? "^ Bool.toString (test) ^ "\n")
    in
	if (List.last path = "self") then
	    (* 'self' is writen in the ocl file *)
	    (Variable ("self",typ))
	else
	    (AttributeCall (Variable ("self",typ),DummyT,path,DummyT))
    end
| check_for_self arg_list typ (AttributeCall (source_term,source_typ,path,ret_typ)) model = 
  let
      val _ = trace zero ("check_for_self: complex AttributeCall "^ "\n")
  in
      (AttributeCall (check_for_self arg_list typ source_term model,source_typ,path,ret_typ))
  end
(* OperationCall *)
| check_for_self arg_list typ (OperationCall (Variable ("dummy_source",_),source_type,path,paras,ret_typ)) model = 
  let
      val test = (member (List.last path) (List.map (#1) arg_list))
      val _ = trace zero ("member2? "^ Bool.toString (test) ^ "\n")
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
      val _ = trace zero ("check_for_self: complex OperationCall "^ "\n")
  in
      (OperationCall (check_for_self arg_list typ source_term model ,source_typ,path,check_for_self_paras arg_list typ paras model,ret_typ))
  end
| check_for_self arg_list typ (Iterator (name,iter_var,sterm,styp,expr,expr_typ,res_typ)) model = 
  let
      val _ = trace zero ("check_for_self: Iterator "^ "\n")
  in
      (Iterator (name,iter_var,(check_for_self arg_list typ sterm model),styp,(check_for_self arg_list typ expr model),expr_typ,res_typ))
  end
| check_for_self arg_list typ (Iterate (iter_vars,acc_name,acc_type,acc_term,sterm,stype,bterm,btype,res_type)) model = 
  let
      val _ = trace zero ("check_for_self: Iterate "^ "\n")
  in
      (Iterate (iter_vars,acc_name,acc_type,acc_term,(check_for_self arg_list typ sterm model),stype,(check_for_self arg_list typ bterm model),btype,res_type))
  end
| check_for_self arg_list typ (Let (str,ttyp,rhs_term,rhs_typ,in_term,in_typ)) model =
  let
      val self_rhs_term = check_for_self arg_list typ rhs_term model
      val self_in_term = check_for_self arg_list typ in_term model
  in
      (Let (str,ttyp,self_rhs_term,rhs_typ,self_in_term,in_typ))
  end
| check_for_self arg_list typ (If (cond,cond_typ,expr1,typ1,expr2,typ2,res_typ)) model =
  let
      val self_cond = check_for_self arg_list typ cond model
      val self_expr1 = check_for_self arg_list typ expr1 model
      val self_expr2 = check_for_self arg_list typ expr2 model
  in
      (If (self_cond,cond_typ,self_expr1,typ1,self_expr2,typ2,res_typ))
  end
| check_for_self arg_list typ term model = term



(* RETURN: Context *)
fun preprocess_context (Cond (path,op_name,op_sign,result_type,cond,pre_name,expr)) model = 
    let
	(* embed 'result' variable *)
	val _ = trace zero ("Embed result variable \n")
	val vexpr = generate_variables expr path op_name model
	val _ = trace zero ("Variable 'result' embeded ... \n")
	(* embed method arguments *)
	val class = class_of_type  (Classifier (path)) model
	val prfx  = package_of class
	val prefixed_op_sign = List.map (fn (a,b) => (a,prefix_type prfx  b)) op_sign
	val prefixed_result_type = prefix_type prfx result_type
	val eexpr = embed_method_arguments prefixed_op_sign vexpr
        (* embed '@pre'-expressions *)
	val pexpr = embed_atPre_expressions eexpr
    in
	(Cond (path,op_name,prefixed_op_sign,prefixed_result_type,cond,pre_name,(check_for_self prefixed_op_sign (Classifier (path)) pexpr model)))
    end
| preprocess_context (Inv (path,string,term)) model =
    let
	val _ = trace zero ("Preprocess context: Inv (...)" ^ "\n")
        (* embed '@pre'-expressions *)
	val pexpr = embed_atPre_expressions term
    in
	(Inv (path,string,(check_for_self [] (Classifier (path)) pexpr model)))
    end
| preprocess_context (Attr (path,typ,aoa,expr)) model = 
    let
	val _ = trace zero ("Preprocess context: Attr"^ "\n")
        (* embed '@pre'-expressions *)
	val pexpr = embed_atPre_expressions expr
    in
	(Attr (path,typ,aoa,check_for_self [] (Classifier (path)) pexpr model))
  end
| preprocess_context c  model = 
    let
	val _ = trace zero ("Preprocess context: others" ^ "\n")
    in
	c
    end

(* RETURN: Context list *)
fun preprocess_context_list [] model = [] 
| preprocess_context_list (h::context_list_tail) model = (preprocess_context h model)::(preprocess_context_list context_list_tail model)

end
