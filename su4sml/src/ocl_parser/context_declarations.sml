(*****************************************************************************
 * su4sml --- a SML repository for managing (Secure)UML/OCL models
 *             http://projects.brucker.ch/su4sml/
 *                                                                            
 * context_declarations.sml --- 
 * This file is part of su4sml.
 *
 * Copyright (c) 2005-2007 ETH Zurich, Switzerland
 *           (c) 2008       Achim D. Brucker, Germany
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

signature CONTEXT = 
sig

    (* datatypes *)
    datatype ConditionType = pre | post | body (* | def *)
    datatype AttrOrAssoc = derive | init | def
    datatype context =
	 Empty_context of string *
			Rep_OclTerm.OclTerm				(* expression *)
       | Inv of 	string list *		                                (* context *)
			string option *					(* name of invariant *)
			Rep_OclTerm.OclTerm				(* invariant expression *)
       | Attr of	string list *	     				        (* context *)
			Rep_OclType.OclType *				(* type *)
			AttrOrAssoc *					(* {Init|Derive} *)
			Rep_OclTerm.OclTerm 				(* init_or_der_value *)		
       | Cond of	string list   *					(* context *)
			string *					(* name of operation *)
			(string * Rep_OclType.OclType) list * 		(* signature of operation *)
			Rep_OclType.OclType *				(* result *)
			ConditionType *                			(* {Pre | Post | Body} *)
			string option *					(* name of precondition *)
			Rep_OclTerm.OclTerm		 		(* condition expression *)
       | Guard of	string list *		              			(* context *) (* not yet supported *)
			string option *					(* name *)
			Rep_OclTerm.OclTerm				(* expression *)

    (* exceptions *)			
    exception Error of string
    exception wrongOperation of string

    (* operations *)				 
    val add_source                      : Rep_OclTerm.OclTerm * Rep_OclTerm.OclTerm -> Rep_OclTerm.OclTerm
    val nest_source                     : Rep_OclTerm.OclTerm list -> Rep_OclTerm.OclTerm
    val cond_type_to_string             : ConditionType -> string 
    val package_of_context              : context -> string list
    val real_path                       : string list -> string list
    val gen_let_term                    : (string * Rep_OclType.OclType * Rep_OclTerm.OclTerm) list -> Rep_OclTerm.OclTerm -> Rep_OclTerm.OclTerm
    val gen_literal_term                : string * Rep_OclType.OclType -> Rep_OclTerm.OclTerm * Rep_OclType.OclType
    val extend_path                     : context -> string list -> context
    val list_extend_path                : string list -> context list -> context list
    val real_signature                  : ('a * 'b) list -> ('a * 'b) list
    val cxt_list2string                 : context list -> string
    val guard_list                      : string list * (string option * Rep_OclTerm.OclTerm) list -> context list
    val inv_list                        : string list * (string option * Rep_OclTerm.OclTerm) list -> context list
    val cond_list                       : string list * (string * Rep_OclType.OclType) list * (ConditionType * string option * Rep_OclTerm.OclTerm) list -> context list
    val attr_list                       : string list * Rep_OclType.OclType * (AttrOrAssoc * Rep_OclTerm.OclTerm) list -> context list

    (* values *)														  
    val OclLibPackage                   : string
    
    (* OclConsoleParser *)
    val rename_classifier               : Rep_OclType.Path -> Rep_Core.Classifier -> Rep_Core.Classifier
    val merge_classifiers               : Rep_Core.Classifier list -> Rep_Core.Classifier

    val operations_to_classifier        : Rep_Core.operation list -> Rep_Core.Classifier 
    val attributes_to_classifier        : Rep_Core.attribute list -> Rep_Core.Classifier
    val constraints_to_classifier       : ((string option * Rep_OclTerm.OclTerm) list) -> Rep_Core.Classifier

    val dispatch_pre_or_post            : ConditionType -> ((ConditionType * string option * Rep_OclTerm.OclTerm) list) -> (string option * Rep_OclTerm.OclTerm) list

								     
end
structure Context:CONTEXT =
struct


open Rep_Logger
open Rep_Core
open Rep_OclType
open Rep_OclTerm
open XMI_DataTypes
open OclLibrary

     
type operation = Rep_Core.operation

type Visibility = Rep_Core.Visibility
type Scope      = XMI_DataTypes.ScopeKind
		 
datatype ConditionType = pre | post | body
				      
datatype AttrOrAssoc = derive | init | def
				       
datatype context =
	 Empty_context of string *
			  Rep_OclTerm.OclTerm				(* expression *)
       | Inv of 	string list *		                                (* context *)
			string option *					(* name of invariant *)
			Rep_OclTerm.OclTerm				(* invariant expression *)
       | Attr of	string list *	     				        (* context *)
			Rep_OclType.OclType *				(* type *)
			AttrOrAssoc *					(* {Init|Derive} *)
			Rep_OclTerm.OclTerm 				(* init_or_der_value *)		
       | Cond of	string list   *					(* context *)
			string *					(* name of operation *)
			(string * Rep_OclType.OclType) list * 		(* signature of operation *)
			Rep_OclType.OclType *				(* result *)
			ConditionType *                			(* {Pre | Post | Body} *)
			string option *					(* name of precondition *)
			Rep_OclTerm.OclTerm		 		(* preondition expression *)
       | Guard of	string list *		              			(* context *) (* not yet supported *)
			string option *					(* name *)
			Rep_OclTerm.OclTerm				(* expression *)
			
			
exception Error of string
exception wrongOperation of string
exception NestSourceError of string
	  

val OclLibPackage = OclLibrary.OclLibPackage


(* RETURN: string *)
fun cond_type_to_string pre = "pre"
  | cond_type_to_string post = "post"
  | cond_type_to_string body = "body"

(* RETURN: string list *)
fun package_of_context (Empty_context (_,_))  = raise Error "Empty Context in Context.package_of"
  | package_of_context (Inv (p,_,_))          = rev (tl (rev p))
  | package_of_context (Attr (p,_,_,_))       = rev ((tl o tl) (rev p))
  | package_of_context (Cond (p,_,_,_,_,_,_)) =  rev ((tl o tl) (rev p))
  | package_of_context (Guard (_,_,_))        = raise Error "Guard not supported in in Context.package_of" 


(* switch arguments *)
fun switch f (a,b) = f (b,a)

(* RETURN: Path *)
fun real_path ([]) = []
  | real_path ([x]) = []
  | real_path (x::tail) = x::real_path tail

(* RETURN: OclTerm  /* a let term */ *)
fun gen_let_term [] expr = expr
  | gen_let_term ((str,typ,exp)::init_var_list_tail) expr =
    (Let (str,typ,exp,DummyT,gen_let_term init_var_list_tail expr,DummyT))

(* RETURN: OclTerm *)
fun gen_literal_term (name,typ) = (Literal (name,typ),typ)

(* prefix the path of an OclTerm with 'ext_path' *)			
fun extend_path (Attr (path,typ,selector,expr)) ext_path = Attr (ext_path@path,prefix_type ext_path typ,selector,prefix_expression ext_path expr)
  | extend_path (Cond (path,name,sign,res,selector,name_sel,expr)) ext_path = 
    Cond (ext_path@path,name,sign,res,selector,name_sel,prefix_expression ext_path expr)
  | extend_path (Inv (path, name, expr)) ext_path = Inv (ext_path@path,name,prefix_expression ext_path expr)
  | extend_path (Guard (path, name, expr)) ext_path = Guard (ext_path@path, name, prefix_expression ext_path expr)


(* RETURN: context list *)
(* prefixes the path of every list member with 'ext_path' *) 
fun list_extend_path s [] = []
  | list_extend_path ext_path ((Empty_context (s,t))::(context_list_tail)) = 
    [(Empty_context(s,t))]@(list_extend_path ext_path context_list_tail)

  | list_extend_path ext_path ((Inv(path,st,t))::(context_list_tail)) = 
    [(extend_path (Inv(path,st,t)) ext_path)]@(list_extend_path ext_path context_list_tail)

  | list_extend_path ext_path ((Attr(path,ty,aoa,t))::(context_list_tail)) = 
    [(extend_path (Attr(path,ty,aoa,t)) ext_path)]@(list_extend_path ext_path context_list_tail)

  | list_extend_path ext_path ((Cond(path,s,sig_list,res,con,so,t))::(context_list_tail)) = 
    [(extend_path (Cond(path,s,sig_list,res,con,so,t)) ext_path)]@(list_extend_path ext_path context_list_tail)
  | list_extend_path ext_path ((Guard(path,so,t))::(context_list_tail)) = 
    [(extend_path (Guard(path,so,t)) ext_path)]@(list_extend_path ext_path context_list_tail)

(* deletes last element of signature which is return type of operation *)
fun real_signature ([]) = []
  | real_signature [(name,typ)] = []
  | real_signature ((name,typ)::tail) = 
    (name,typ)::real_signature tail

(* RETURN: OclTerm *)
(* Add to an OclTerm the correct source term 'source' *)
fun add_source (source,(AttributeCall (_, _, path, res_typ ))) = 
    let
	val test = (AttributeCall (source,DummyT,path,res_typ))
	val _ = trace low ("source added for AttributeCall..." ^ Ocl2String.ocl2string true test ^ "\n"); 
    in
	(AttributeCall (source, DummyT, path, res_typ))
    end
  | add_source (source,(OperationCall (_,_,path,paras,res_typ))) = 
    let
	val _ = trace low ("source added for OperationCall..." ^ "\n"); 
    in
	(OperationCall (source,DummyT,path,paras,res_typ))
    end
  | add_source (source, Literal(s,t)) = Literal (s,t)
  | add_source (source, CollectionLiteral (part_list,typ)) = 
    let 
	val _ = trace low ("source added for AttributeCall..." ^ "\n"); 
    in
	(CollectionLiteral (part_list,typ))
    end
  | add_source (source, Iterator(name,iter_vars_list,_,_,body_term,body_typ,res_typ)) =
    let
	val _ = trace low ("source added for Iterator..." ^ "\n"); 
    in
	(Iterator (name,iter_vars_list,source,DummyT,body_term,body_typ,res_typ))
    end
  | add_source (source, Let(paras)) = 
    (* let has no source *)
    Let(paras) 
  | add_source (source, If (paras)) = 
    (* If has no source *)
    If (paras)
  | add_source (source, Iterate([],acc_var_name,acc_var_type,acc_var_term,sterm,stype,bterm,btype,res_type)) =
    let
	val _ = trace low ("source added for Iterate ..." ^ "\n");
    in
	(Iterate ([],acc_var_name,acc_var_type,acc_var_term,source,DummyT,bterm,btype,res_type))
    end
  | add_source (source, Iterate(iter_vars,acc_var_name,acc_var_type,acc_var_term,sterm,stype,bterm,btype,res_type)) =
    let
	val _ = trace high ("source added for Iterate ..." ^ "\n");
    in
	(Iterate (iter_vars,acc_var_name,acc_var_type,acc_var_term,source,DummyT,bterm,btype,res_type))
    end

(* RETURN: OclTerm list *)
fun add_source_to_list source (h::tail) = (add_source (source,h))::tail

(* RETURN: OclTerm *)					
(* add sources of a list, when every list element is the
   source of the following element.
   The last element is initalized with "self", because its 
   always so in an object oriented language if its no an argument
   of an operation, which is checked later.
 *)

(* RETURN: OclTerm *)
fun nest_source (OperationCall (sterm,styp,[OclLibPackage,rtyp,"-"],[],res_typ)::tail) =
    let
	val _ = trace low ("unary_exp_cs Call: '-' ... \n")
    in
	foldl (switch add_source) (OperationCall (sterm,styp,[OclLibPackage,rtyp,"-"],[],res_typ)) tail
    end
  | nest_source (OperationCall (sterm,styp,[OclLibPackage,rtyp,"not"],[],res_typ)::tail) =
    let
	val _ = trace low ("unary_exp_cs Call: 'not' ... \n")
    in
	foldl (switch add_source) (OperationCall (sterm,styp,[OclLibPackage,rtyp,"not"],[],res_typ)) tail
    end
  | nest_source term_list = 
    let 
	val _ = trace low ("source nested for AttributeCall..." ^ "\n"); 
        val _ = trace low ((Ocl2String.ocl2string true (List.last term_list)) ^ "bla\n"); 
    in
	foldl (switch add_source) (Variable ("dummy_source",DummyT)) term_list
    end

(* RETURN: context list *)
fun attr_list (context,Typ,[]) = 
    let 
	val _ = trace low ("Contextes created form list of Attributes ..." ^ "\n") 
    in 
	[] 
    end
  | attr_list (context,Typ,((asser,expr)::tail)) = 
    let 
	val _ = trace low ("Contextes created form list of Attributes ..." ^ "\n") 
    in 
	(Attr (context,Typ,asser,expr))::(attr_list (context,Typ,tail)) 
    end

(* RETURN: context list *)    
fun inv_list (context,[]) = 
    let 
	val _ = trace low ("Contextes created form list of invs ..." ^ "\n") 
    in 
	[] 
    end	
  | inv_list (context,((name,expr)::tail)) = 
    let 
	val _ = trace low ("Contextes created form list of invs ..." ^ "\n") 
    in 
	(Inv(context,name,expr))::(inv_list (context,tail)) 
    end
    
(* RETURN: context list *)
fun cond_list (path,sign,[]) = 
    let 
	val _ = trace low ("Contextes created form list of conds ..." ^ "\n")
    in 
	[] 
    end
  | cond_list (path,sign,((asser,name_cond,expr)::tail))  = 
    let 
	val _ = trace low ("Contextes created form list of conds ..." ^ "\n") 
    in 
	Cond(real_path path,List.last path,real_signature sign, #2(List.last sign),asser,name_cond,expr)::cond_list (path,sign,tail)
    end

(* RETURN: context list *)    
fun guard_list (context,[]) = []
  | guard_list (context,(name,expr)::tail) = Guard (context,name,expr)::guard_list (context,tail) 					     

(* RETURN: string *)
fun cxt_list2string ([]) = ""
  | cxt_list2string ((Empty_context(s,t))::tail) = 
    "empty: "^(Ocl2String.ocl2string false t)^"\n"^(cxt_list2string tail)
  | cxt_list2string ((Inv(p,s,t))::tail) = 
    "inv: "^(Ocl2String.ocl2string false t)^"\n"^(cxt_list2string tail)
  | cxt_list2string ((Attr(p,ty,a,t))::tail) = 
    "attr_or_assoc "^(Ocl2String.ocl2string false t)^"\n"^(cxt_list2string tail)
  | cxt_list2string ((Cond(p,s,l,ty,c,so,t))::tail) = 
    "condition: "^(Ocl2String.ocl2string false t)^"\n"^(cxt_list2string tail)
  | cxt_list2string ((Guard(p,so,t))::tail) = 
    "guard: "^(Ocl2String.ocl2string false t)^"\n"^(cxt_list2string tail)		

fun rename_classifier path (Class{name=name,parent=parent,attributes=attributes,operations=operations,associations=associations,invariant=invariant,stereotypes=stereotypes,interfaces=interfaces,thyname=thyname,visibility=visibility,activity_graphs=activity_graphs}) =
    let
	val _ = trace function_calls ("Context.rename_classifier\n")
	val res = Class {
		  name = Classifier (path),
		  parent=parent,
		  attributes=attributes,
		  operations=operations,
		  associations=associations,
		  invariant=invariant,
		  stereotypes=stereotypes,
		  interfaces=interfaces,
		  thyname=thyname,
		  visibility=visibility,
		  activity_graphs=activity_graphs
		  }
	val _ = trace function_ends ("Context.rename_classifier\n")
    in
	res
    end

fun merge_classifier ((a as Class{attributes=a_atts,operations=a_ops,invariant=a_invs,associations=a_assocs,...}),(b as Class{attributes=b_atts,operations=b_ops,invariant=b_invs,associations=b_assocs,...})) = 
    let
	val _ = trace function_calls ("Context.merge_classifier\n")
	val res = Class {
		  name = OclVoid,
		  parent=NONE,
		  attributes=a_atts@b_atts,
		  operations=a_ops@b_ops,
		  associations=a_assocs@b_assocs,
		  invariant=a_invs@b_invs,
		  stereotypes=[],
		  interfaces=[],
		  thyname=NONE,
		  visibility=public:Rep_Core.Visibility,
		  activity_graphs=[]
		  }
	val _ = trace function_ends ("Context.merge_classifier\n")
    in
	res
    end

fun merge_classifiers list =
    let
	val _ = trace function_calls ("Context.merge_classifiers\n")
	val Empty_Class = Class{
			  name = OclVoid,
			  parent=NONE,
			  attributes=[],
			  operations=[],
			  associations=[],
			  invariant=[],
			  stereotypes=[],
			  interfaces=[],
			  thyname=NONE,
			  visibility=public:Rep_Core.Visibility,
			  activity_graphs=[]
			  }
	val res = List.foldr (merge_classifier) Empty_Class list 
	val _ = trace function_ends ("Context.merge_classifier\n")
    in
	res
    end


fun operations_to_classifier ops = 
    let
	val _ = trace function_calls ("Context.operation_to_classifier\n")
	val res = Class{
			  name = OclVoid,
			  parent=NONE,
			  attributes=[],
			  operations=ops,
			  associations=[],
			  invariant=[],
			  stereotypes=[],
			  interfaces=[],
			  thyname=NONE,
			  visibility=public:Rep_Core.Visibility,
			  activity_graphs=[]
			  }
	val _ = trace function_ends ("Context.operation_to_classifier\n")
    in
	res
    end

fun attributes_to_classifier atts = 
    let
	val _ = trace function_calls ("Context.attributes_to_classifier\n")
	val res = Class{
			  name = OclVoid,
			  parent=NONE,
			  attributes=atts,
			  operations=[],
			  associations=[],
			  invariant=[],
			  stereotypes=[],
			  interfaces=[],
			  thyname=NONE,
			  visibility=public:Rep_Core.Visibility,
			  activity_graphs=[]
			  }
	val _ = trace function_ends ("Context.attributes_to_classifier\n")
    in
	res
    end

fun constraints_to_classifier invs = 
    let
	val _ = trace function_calls ("Context.constraints_to_classifier\n")
	val res = Class{
			  name = OclVoid,
			  parent=NONE,
			  attributes=[],
			  operations=[],
			  associations=[],
			  invariant=invs,
			  stereotypes=[],
			  interfaces=[],
			  thyname=NONE,
			  visibility=public:Rep_Core.Visibility,
			  activity_graphs=[]
			  }
	val _ = trace function_ends ("Context.constraints_to_classifier\n")
    in
	res
    end

fun dispatch_pre_or_post (cond_type:ConditionType) (list:(ConditionType * string option * OclTerm) list) = 
    let
	val _ = trace function_calls ("Context.dispatch_pre_or_post")
	val filter = List.filter (fn (a,b,c) => if cond_type = a
						then true
						else false) list
	val res = List.map (fn (a,b,c) => (b,c)) filter
	val _ = trace function_ends ("Context.dispatch_pre_or_post")
    in
	res
    end
end;
