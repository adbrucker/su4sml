(*****************************************************************************
 * su4sml --- a SML repository for managing (Secure)UML/OCL models
 *             http://projects.brucker.ch/su4sml/
 *                                                                            
 * type_checker.sml --- 
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

signature TYPECHECKER = 
sig


    exception wrongCollectionLiteral of Rep_OclTerm.OclTerm * string
    exception CollectionRangeError of Rep_OclTerm.CollectionPart * string
    exception IteratorTypeMissMatch of Rep_OclTerm.OclTerm * string 
    exception NoSuchIteratorNameError of Rep_OclTerm.OclTerm * string 
    exception TypeCheckerResolveIfError of Rep_OclTerm.OclTerm * string
    exception NotYetSupportedError of string
    exception WrongContextChecked of Context.context
    exception IterateError of string
    exception IterateAccumulatorTypeError of string
    exception IterateTypeMissMatch of string

    val check_context_list            : Context.context list -> Rep_Core.Classifier list -> Context.context option list
    val check_context                 : Context.context -> Rep_Core.Classifier list -> Context.context option
    val resolve_OclTerm               : Rep_OclTerm.OclTerm -> Rep_Core.Classifier list -> Rep_OclTerm.OclTerm
    val resolve_CollectionPart        : Rep_Core.Classifier list -> Rep_OclTerm.CollectionPart -> Rep_OclTerm.CollectionPart 
    val resolve_arguments             : (Rep_OclTerm.OclTerm * Rep_OclType.OclType) list -> Rep_Core.Classifier list -> (Rep_OclTerm.OclTerm * Rep_OclType.OclType) list
end

structure TypeChecker:TYPECHECKER  = 
 struct

open Context
open Rep_OclTerm
open Rep_OclType
open Rep_Core
open RepParser
open XMI_DataTypes
open Preprocessor
open OclLibrary
open Ext_Library

type operation = Rep_Core.operation
type attribute = Rep_Core.attribute

exception wrongCollectionLiteral of Rep_OclTerm.OclTerm * string
exception CollectionRangeError of Rep_OclTerm.CollectionPart * string
exception IteratorTypeMissMatch of Rep_OclTerm.OclTerm * string 
exception NoSuchIteratorNameError of Rep_OclTerm.OclTerm * string 
exception TypeCheckerResolveIfError of Rep_OclTerm.OclTerm * string
exception NotYetSupportedError of string
exception WrongContextChecked of context
exception AsSetError of (OclTerm * string list * int * (OclTerm * OclType) list *  Classifier list)
exception DesugaratorCall of (OclTerm * string list * int * (OclTerm * OclType) list * Classifier list)
exception IterateError of string			     
exception IterateAccumulatorTypeError of string
exception IterateTypeMissMatch of string			  

(* RETURN: bool *)
fun check_argument_type [] [] = true
  | check_argument_type [x] [] = false
  | check_argument_type [] [x] = false
  | check_argument_type list [] = false
  | check_argument_type [] list = false
  | check_argument_type [(term,typ1:OclType)] [(string,typ2:OclType)] =
    if (typ1 = typ2) then
	true
    else
	false
  | check_argument_type ((term,typ1)::sig_tail1) ((string,typ2)::sig_tail2) = 
    if (typ1 = typ2) then
	(check_argument_type sig_tail1 sig_tail2)
    else	false



(* RETURN: OclTerm (OperationCall/AttributeCall) *)
fun FromSet_desugarator rterm path attr_or_meth rargs model =
    if (attr_or_meth = 0)
    then (* OperationCall *)
	let
	    (* check 'fromSet' *)
	    val _ = trace low ("\n==> FromSet-desugarator: operation ... \n")
	    val new_type = template_parameter (type_of_term rterm)
	    val iterVar = (("anonIterVar_" ^ (varcounter.nextStr())),new_type)
	    val class = get_classifier (Variable (iterVar)) model 
	    val ops = get_overloaded_methods class (List.last path) model
	in
	    if (List.length ops = 0)
	    then raise InterferenceError ("FromSet no operation/attribute found. \n")
	    else
		let
		    val insert_term = interfere_methods ops (Variable iterVar) rargs model
		    val it_type = type_of_term insert_term
		in
		    Iterator ("collect",[iterVar],rterm,type_of_term rterm,insert_term,it_type,it_type)
		end
	end
    else (* AttributeCall *)
	let
	    (* check 'fromSet' *)
	    val _ = trace low ("\n==> FromSet-desugarator: attribute/assocend ... \n")
	    val new_type = template_parameter (type_of_term rterm)
	    val iterVar = (("anonIterVar_" ^ (varcounter.nextStr())),new_type)
	    val class = get_classifier (Variable (iterVar)) model 
	    val attrs_or_assocs = get_overloaded_attrs_or_assocends class (List.last path) model
	in
	    if (List.length attrs_or_assocs = 0)
	    then raise InterferenceError ("Attriubte '" ^ (List.last path) ^ "' does not exist ... \n") 
	    else 
		let
		    val insert_term = interfere_attrs_or_assocends attrs_or_assocs (Variable iterVar) model
		    val it_type = type_of_term insert_term
		    val _ = trace development ("association type " ^ string_of_OclType it_type ^ "\n")
        	    (* special case *)
        	   (* if it is an attribute, there needs to be added a collection type constructor *)
		
		in
		    case (List.hd attrs_or_assocs) of 
			(* AttributeCall *)
			(x,SOME(shit),NONE) => 
			let
			    val ret_type = replace_templ_para (type_of_term rterm) it_type
			in
			    Iterator ("collect",[iterVar],rterm,type_of_term rterm,insert_term,it_type,ret_type)
			end
			(* AssociatonCall *)
		      | (x,NONE,SOME(shit)) => 
			if (isColl_Type it_type) 
			then
			    let
				val ret_type = replace_templ_para (type_of_term rterm) (template_parameter it_type)
			    in
				Iterator("collect",[iterVar],rterm,type_of_term rterm,insert_term,it_type,ret_type)
			    end
			else
			    let
				val ret_type = replace_templ_para (type_of_term rterm) it_type
			    in
				Iterator("collect",[iterVar],rterm,type_of_term rterm,insert_term,it_type,ret_type)
			    end
		end
	end	
	
(* RETURN: OclTerm (OperationCall/AttributeCall) *)
fun AsSet_desugarator rterm path attr_or_meth rargs model =
    if (attr_or_meth = 0) 
    then (* OperationCall *)
	let
	    val _ = trace low ("\n==> AsSet-desugarator: operation ... \n")
	    val rtyp = Set(type_of_term rterm)
	    val _ = trace low ("Type of source term " ^ string_of_OclType rtyp ^ " ---> try Set(" ^ string_of_OclType rtyp ^ ")\n")
	    val class = get_classifier (Variable ("anonIterVar_" ^ (varcounter.nextStr()),rtyp)) model
	    val ops = get_overloaded_methods class (List.last path) model
	    val new_rterm = CollectionLiteral([CollectionItem(rterm,type_of_term rterm)],rtyp)
	in
	    if (List.length ops = 0)
	    then
		raise NoSuchOperationError ("interefere_methods: No operation signature matches given types (source: "^(Ocl2String.ocl2string false rterm)^").")
	    else
		interfere_methods ops new_rterm rargs model
	end
    else (* AttributeCall *)
	let
	    val _ = trace low ("\n==> AsSet-desugarator: attribute/assocend\n")
	    val rtyp = Set(type_of_term rterm)
	    val _ = trace low (string_of_OclType rtyp ^ "\n")
	    val class = get_classifier (Variable ("anonIterVar_" ^ (varcounter.nextStr()),Set(rtyp))) model
	    val attrs = get_overloaded_attrs_or_assocends class (List.last path) model
	    (* source term is a dummy-Term *) 
	    val new_rterm = CollectionLiteral([CollectionItem(rterm,type_of_term rterm)],rtyp)
	    val _ = trace low ("'AsSetError' ... \n")
	in
	    if (List.length attrs = 0) 
	    then 
		raise NoSuchAttributeError ("Attriubte '" ^ (List.last path) ^ "' does not exist ... \n") 
	    else 
		interfere_attrs_or_assocends attrs new_rterm model
	end

(* RETURN: OclTerm (OperationCall/AttributeCall) *)
fun desugarator rterm path attr_or_meth rargs model = 
    FromSet_desugarator rterm path attr_or_meth rargs model  
    handle InterferenceError s => AsSet_desugarator rterm path attr_or_meth rargs model

(* RETURN: CollectionPart *)
fun resolve_CollectionPart model (CollectionItem (term,typ))  =
    let
	val rterm = resolve_OclTerm term model
	val rtyp = type_of_term rterm
    in
	(CollectionItem (rterm,rtyp))
    end
  | resolve_CollectionPart model (CollectionRange (term1,term2,typ))  = 
    let
	val rterm1 = resolve_OclTerm term1 model
	val rtyp1 = type_of_term rterm1
	val rterm2 = resolve_OclTerm term2 model
	val rtyp2 = type_of_term rterm2
    in
	if (rtyp2 = rtyp1) then
	    (CollectionRange (rterm1,rterm2,rtyp1))
	else
	    raise (CollectionRangeError ((CollectionRange (term1,term2,typ)),("Begin and end of Range not of same type.\n")))
    end

and resolve_CollectionLiteral (CollectionLiteral (part_list,typ)) model = 
    let
	val rpart_list = List.map (resolve_CollectionPart model) part_list
	val tlist = List.map (type_of_CollPart) rpart_list
    in
	if (List.all (type_equals (List.hd (tlist))) tlist)
	then
	    rpart_list
	else
	    raise wrongCollectionLiteral ((CollectionLiteral (part_list,typ)),"Not all Literals have the same type.\n")
    end
    
(* RETURN: OclTerm * OclType *)
and resolve_arguments [] model = []
| resolve_arguments ((term,typ)::list_tail) model =
    let 
	val rterm = resolve_OclTerm term model 
	val rtyp = type_of_term rterm
    in
	(rterm,rtyp)::(resolve_arguments list_tail model)
    end

(* RETURN: OclTerm *)
and resolve_OclTerm (Literal (s,typ)) model = 
  let
      val _ = trace medium ("RESOLVE Literal: " ^ s ^ "\n")
	      
  in
      (Literal (s,typ))
  end
(* Variable *)
| resolve_OclTerm (Variable ("self",typ)) model = (Variable ("self",typ)) 
| resolve_OclTerm (Variable (name,typ)) model = 
  let
      val _ = trace medium ("RESOLVE Variable: " ^ name ^ "\n")
  in
      Variable (name,typ)
  end
(* AssociationEndCall *)
| resolve_OclTerm (AssociationEndCall (term,_,["self"],_)) model =  (resolve_OclTerm (AttributeCall (term,OclAny,["self"],OclAny)) model) 
| resolve_OclTerm (AssociationEndCall (term,_,attr_path,_)) model = (resolve_OclTerm (AttributeCall (term,OclAny,attr_path,OclAny)) model)
(* AttributeCall *)
(* self.self -> self *)
| resolve_OclTerm (AttributeCall (term,_,["self"],_)) model = 
  let
      val _ = trace medium ("RESOLVE AttributeCall:  a self call ... " ^ "\n")
  in
      (resolve_OclTerm term model)
  end
| resolve_OclTerm (AttributeCall (term,_,attr_path,_)) model =
  let 
      val _ = trace medium ("RESOLVE AttributeCall: attribute name:  " ^ (List.last attr_path) ^ "\n")
      (* resolve source term *)
      val rterm = resolve_OclTerm term model
      val _ = trace low ("res AttCall (" ^ (List.last attr_path) ^ ") : rterm = " ^ Ocl2String.ocl2string false rterm ^ "\n")
      val _ = trace low ("res AttCall (" ^ (List.last attr_path) ^ ") : rtype = " ^ string_of_OclType (type_of_term rterm) ^ "\n")
  in
      let
      in
	  if (List.hd attr_path = "arrow")
	  then get_attr_or_assoc rterm (List.last attr_path) model
	       handle InterferenceError s => AsSet_desugarator rterm (List.tl attr_path) 1 [] model
	  else
	       get_attr_or_assoc rterm (List.last attr_path) model
	      
	      (*  2-dimensional inheritance of Collection types *)
	      handle InterferenceError s =>
		     (
		      (
		       let
			   val _ = trace low ("\n==> 2-dim Inheritance check: attribute/assocend\n")
			   val rtyp = type_of_term rterm
			   val _ = trace low (string_of_OclType rtyp ^ "\n")
			   val templ_type = template_parameter rtyp
			   val pclass = get_classifier (Variable ("x",templ_type)) model
			   val ntempl_type = type_of_parent pclass 
			   val new_type = replace_templ_para rtyp ntempl_type
			   val new_class = get_classifier (Variable ("x",new_type)) model
			   val attrs = get_overloaded_attrs_or_assocends new_class (List.last attr_path) model
			   val _ = trace low ("parent type of term:" ^ string_of_OclType new_type ^ "\n")
		       in
			   if (List.length attrs = 0) 
			   then raise DesugaratorCall (rterm,attr_path,1,[],model) 
			   else interfere_attrs_or_assocends attrs rterm model
		       end    
		      ) 
		      handle DesugaratorCall arg => desugarator (#1 arg) (#2 arg) (#3 arg) (#4 arg) (#5 arg)
			   | NoCollectionTypeError t => AsSet_desugarator  rterm attr_path 1 [] model
		     )
      end
  end
(* built in Operations not include in Library: oclIsKindOf, oclIsTypOf, oclAsType *) 
(* OperationWithType Calls *)
(* OCLISTYPEOF *)
| resolve_OclTerm (OperationCall (term,_,["oclIsTypeOf"],[(AttributeCall (Variable ("self",vtyp),_,[real_typ], _),argt)],_)) model =
let
      (* prefix type of iterator variable *)
    
      val _ = trace medium ("\n\nRESOLVE OperationCallWithType: oclIsTypeOf\n")
      val rterm = resolve_OclTerm term model
      val _ = trace low ("res OpCall: oclIsTypeOf 2:  " ^ "\n")
      (* prefix types *)
      val rtyp = type_of_term rterm
      val _ = trace low ("res OpCall: oclIsTypeOf 3: "  ^ "\n")
      (* need to prefix the package *)
      (* because parameter is written relativly *)	
      val class = get_classifier rterm model
      val prfx = package_of class
      val _ = trace low ("type of classifier: " ^ string_of_path prfx ^ "\n")
      val ctyp = prefix_type prfx (string_to_type [real_typ])       
      val _ = trace low ("res OpCall: oclTypeOf 4:" ^ "... " ^ "\n")
  in
      OperationWithType (rterm,rtyp,"oclIsTypeOf",ctyp,Boolean)
  end  
(* OCLISKINDOF *)
| resolve_OclTerm (OperationCall (term,_,["oclIsKindOf"],[(AttributeCall (Variable ("self",_),_,[real_typ], _),argt)],_)) model =
  let
      val _ = trace medium ("RESOLVE OperationCallWithType: oclIsKindOf\n")
      val rterm = resolve_OclTerm term model
      val _ = trace low ("res OpCall: oclIsKindOf 2:" ^ "... " ^ "\n")
      val rtyp = type_of_term rterm
      val _ = trace low ("res OpCall: oclIsKindOf 3:" ^ "... " ^ "\n")
      (* need to prefix the package *)
      (* because parameter is written relativly *)
      val class = get_classifier rterm model
      val prfx = package_of class
      val _ = trace low ("type of classifier: " ^ string_of_path prfx ^ "\n")
      val ctyp = prefix_type prfx (string_to_type [real_typ])
      val _ = trace low ("res OpCall: oclIsKindOf 4:" ^ "... " ^ "\n")
  in
      OperationWithType (rterm,rtyp,"oclIsKindOf",ctyp,Boolean)
  end
(* OCLASTYPE *)
| resolve_OclTerm (OperationCall (term,_,["oclAsType"],[(AttributeCall (Variable ("self",_),_,[real_typ], _),argt)],_)) model =
  let
      val _ = trace medium ("RESOLVE OperationCallWithType: oclIsKindOf\n")
      val rterm = resolve_OclTerm term model
      val _ = trace low ("res OpCall: oclAsType 2:" ^ "... " ^ "\n")
      val rtyp = type_of_term rterm
      val _ = trace low ("res OpCall: oclAsType 3:" ^ "... " ^ "\n")
      (* need to prefix the package *)
      (* because parameter is written relativly *)
      val class = get_classifier rterm model
      val prfx = package_of class   
      val _ = trace low ("type of classifier: " ^ string_of_path prfx ^ "\n")
      val ctyp = prefix_type prfx (string_to_type [real_typ])
      val _ = trace low ("res OpCall: oclAsType 4:" ^ "... " ^ "\n")
  in
      OperationWithType (rterm,rtyp,"oclAsType",ctyp,ctyp)
  end
(* HARD CODED STUFF *)
| resolve_OclTerm (OperationCall (term,typ,[OclLibPackage,"OclAny","atPre"],[],_)) model = 
  let
      val _ = trace medium  ("RESOLVE OperationCall: atPre \n")
      (* resolve source term *)
      val rterm = resolve_OclTerm term model
      val rtyp = type_of_term rterm
      val _ = trace low  ("res OpCall: Type of source : " ^ string_of_OclType rtyp ^ "\n")
  in
      OperationCall (rterm,rtyp,[OclLibPackage,"OclAny","atPre"],[],rtyp)
  end
| resolve_OclTerm (OperationCall (term,typ,meth_path,args,res_typ)) model = 
  let
      val _ = trace medium  ("RESOLVE OperatioCall: name = " ^ (List.last (meth_path)) ^ "\n")
      (* resolve source term *)
      val rterm = resolve_OclTerm term model
      val _ = trace low  ("res OpCall: Type of source : " ^ string_of_OclType (type_of_term rterm) ^ "\n")
      (* resolve arguments *)
      val rargs = resolve_arguments args model
      val _ = trace low  ("res OpCall: args resolved ...\n")
  in
      let
      in
	  if (List.hd meth_path = "arrow")
	  then get_meth rterm (List.last meth_path) rargs model
	       handle InterferenceError s => AsSet_desugarator rterm (List.tl meth_path) 0 rargs model
	  else 
	      get_meth rterm (List.last meth_path) rargs model
	      (*  2-dimensional inheritance of Collection types *)
	      handle InterferenceError s =>	
		     (     
		      (
		       let
			   val _ = trace low ("\n==> 2-dim Inheritance check: attribute/assocend\n")
			   val rtyp = type_of_term rterm
			   val _ = trace low (string_of_OclType rtyp ^ "\n")
			   val templ_type = template_parameter rtyp
			   val pclass = get_classifier (Variable ("x",templ_type)) model
			   val ntempl_type = type_of_parent pclass 
			   val _ = trace low (string_of_OclType ntempl_type ^ "\n")
			   val new_type = replace_templ_para rtyp ntempl_type
			   val new_class = get_classifier (Variable ("x",new_type)) model
			   val ops = get_overloaded_methods new_class (List.last meth_path) model
			   val _ = trace low ("parent type of term: " ^ string_of_OclType new_type ^ "\n")
		       in
			   if (List.length ops = 0) 
			   then raise DesugaratorCall (rterm, meth_path, 0, rargs, model) 
			   else interfere_methods ops rterm rargs model
		       end
		      )
		      handle DesugaratorCall arg => desugarator (#1 arg) (#2 arg) (#3 arg) (#4 arg) (#5 arg)
			   | NoCollectionTypeError typ => AsSet_desugarator rterm meth_path 0 rargs model 
		     )
      end
end
(* Iterator *)
| resolve_OclTerm (Iterator (name,iter_vars,source_term,_,expr,expr_typ,res_typ)) model =
  let
      (* resolve source term, type *)      
      val _ = trace low ("RESOLVE Itertor: name = " ^ name ^ "\n")
      val rterm = resolve_OclTerm source_term model
      val rtyp = type_of_term rterm
      val _ = trace low ("res Iter (" ^ name ^ "): source type " ^ string_of_OclType (type_of_term  rterm) ^ "\n\n")
      (* get source classifier *)
      val source_class = get_classifier rterm model
      val _ = trace low ("res Iter (" ^ name ^ "): type of classifier: " ^ string_of_OclType (type_of source_class) ^ "\n")
      (* prefix types *)
      val prfx = (package_of_template_parameter (type_of source_class))
      val _ = trace low ("res Iter (" ^ name ^ "): Type prefixed ... \n")
      val piter_vars = List.map (fn (a,b) => (a,prefix_type prfx b)) iter_vars
      val piter_types = List.map (fn (a,b) => b) piter_vars 
      val _ = trace low ("res Iter (" ^ name ^ "): first iter types: " ^ string_of_OclType (List.hd piter_types) ^ "\n") 
      (* check if iterator types correspond to source type *)
      val static_iter_type = template_parameter (type_of (source_class))
      val _ = trace low ("Length of iter_types: " ^ Int.toString (List.length piter_types) ^ "\n")
      val _ = trace low ("parent of classifier: " ^ string_of_OclType (type_of_parent source_class) ^ "\n")
      val _ = trace low ("\nstatic iter type : " ^ string_of_OclType static_iter_type ^ "  \n")
      val _ = trace low ("iter types: " ^ string_of_OclType (List.hd piter_types) ^ "\n")
      val h2 = List.map (fn a => conforms_to a static_iter_type model) (piter_types)
      val check = List.all (fn a => a=true) h2
  in
      if (check) then
	  let
	      val _ = trace low  ("res Iter: types conforms \n")
	      val bound_expr = embed_bound_variables piter_vars expr
	      val _ = trace low ("res Iter: term : " ^ Ocl2String.ocl2string false bound_expr ^ "\n")
	      val rexpr = resolve_OclTerm bound_expr model
	      val _ = trace low (" manuel " ^ string_of_OclType (type_of_term rexpr) ^ "\n")
	      val _ = trace low (" ma     " ^ string_of_OclType (Set(static_iter_type)) ^ "\n")
	      val _ = trace low  ("res Iter: Iterator name = " ^ name ^ " \n\n\n")
	  in
	      (
	       case name of
		   "select" => 
		   Iterator (name,piter_vars,rterm,rtyp,rexpr,type_of_term rexpr,rtyp)
		 | "reject" => 
		   Iterator (name,piter_vars,rterm,rtyp,rexpr,type_of_term rexpr,rtyp)
		 | "forAll" =>  
		   Iterator (name,piter_vars,rterm,rtyp,rexpr,type_of_term rexpr,Boolean)
		 | "one" => 
		   Iterator (name,piter_vars,rterm,rtyp,rexpr,type_of_term rexpr,Boolean)
		 | "any" => 
		   Iterator (name,piter_vars,rterm,rtyp,rexpr,type_of_term rexpr,Boolean)
		 | "exists" =>
		   Iterator (name,piter_vars,rterm,rtyp,rexpr,type_of_term rexpr,Boolean)
		 | "collect" => 
		   Iterator (name,piter_vars,rterm,rtyp,rexpr,type_of_term rexpr,flatten_type (replace_templ_para (rtyp) (type_of_term rexpr)))
		 | _ => raise NoSuchIteratorNameError (Iterator (name,iter_vars,source_term,DummyT,expr,expr_typ,res_typ),("No such Iterator ..."))
	      )
	  end
      else      
	  raise IteratorTypeMissMatch (Iterator (name,iter_vars,source_term,DummyT,expr,expr_typ,res_typ),("Iterator variable doesn't conform to choosen set \n"))
  end     
| resolve_OclTerm (Iterate (iter_vars,acc_var_name,acc_var_type,acc_var_term,sterm,stype,bterm,btype,res_type)) model = 
let
      (* resolve source term, type *)
      val _ = trace medium ("RESOLVE Iterate: accumulator " ^ acc_var_name ^ "\n")
      val rterm = resolve_OclTerm sterm model
      val rtyp = type_of_term rterm
      val _ = trace medium ("res Iterate: source type " ^ string_of_OclType (type_of_term  rterm) ^ "\n\n")
      (* get source classifier *)
      val source_class = get_classifier rterm model
      val _ = trace medium ("res Iterate: type of classifier: " ^ string_of_OclType (type_of source_class) ^ "\n")
      (* prefix types *)
      val prfx = (package_of_template_parameter (type_of source_class))
      val _ = trace medium ("res Iterate: Type prefixed ... \n")
      val piter_vars = List.map (fn (a,b) => (a,prefix_type prfx b)) iter_vars
      val piter_types = List.map (fn (a,b) => b) piter_vars 
      val _ = trace medium ("res Iterate: first iter types: " ^ string_of_OclType (List.hd piter_types) ^ "\n") 
      (* check if iterator types correspond to source type *)
      val static_iter_type = template_parameter (type_of (source_class))
      val _ = trace medium ("Length of iter_types: " ^ Int.toString (List.length piter_types) ^ "\n")
      val _ = trace medium ("parent of classifier: " ^ string_of_OclType (type_of_parent source_class) ^ "\n")
      val _ = trace medium ("\nstatic iter type : " ^ string_of_OclType static_iter_type ^ "  \n")
      val _ = trace medium ("iter types: " ^ string_of_OclType (List.hd piter_types) ^ "\n")
      val h2 = List.map (fn a => conforms_to a static_iter_type model) (piter_types)
      val check = List.all (fn a => a=true) h2
      (* check if initial value of accumulator has correct type *)
      val racc_var_term = resolve_OclTerm acc_var_term model
      val racc_var_type = type_of_term racc_var_term
  in
      if (check) then
	  if (racc_var_type = acc_var_type) then
	      let
		  val _ = trace medium  ("res Iterate: types conforms \n")
		  val bound_expr = embed_bound_variables piter_vars bterm
		  val bound_expr2 = embed_bound_variables [(acc_var_name,acc_var_type)] bound_expr
		  val _ = trace medium ("myres Iterate: term : " ^ Ocl2String.ocl2string false bound_expr2 ^ "\n")
		  val rexpr = resolve_OclTerm bound_expr2 model
		  val _ = trace medium (" manuel " ^ string_of_OclType (type_of_term rexpr) ^ "\n")
		  val _ = trace medium (" ma     " ^ string_of_OclType (Set(static_iter_type)) ^ "\n")
		  val _ = trace medium  ("res Iterate: \n\n\n")
	      in
		  Iterate(piter_vars,acc_var_name,racc_var_type,racc_var_term,rterm,rtyp,rexpr,type_of_term rexpr,racc_var_type)
	      end
	  else
	      raise IterateAccumulatorTypeError ("Type of accumulator does not conform to type of expression of accumulator")
      else
	  raise IterateTypeMissMatch ("Iterate variables doesn't conform to choosen set \n")
  end     
| resolve_OclTerm (CollectionLiteral ([],typ)) model = 
  let
      val _ = trace medium ("RESOLVE CollectionLiteral\n")
  in 
      CollectionLiteral ([],typ)
  end
| resolve_OclTerm (CollectionLiteral (coll_parts,temp_typ)) model = 
  let
      val _ = trace medium ("RESOLVE CollectionLiteral\n")
      val r_coll_parts = List.map (resolve_CollectionPart model) coll_parts
      val typ = type_of_CollPart (List.hd r_coll_parts)
  in
      if (List.all (correct_type_for_CollLiteral typ) r_coll_parts) then
      (CollectionLiteral (r_coll_parts,replace_templ_para temp_typ typ))
  else
      raise (wrongCollectionLiteral ((CollectionLiteral (coll_parts,temp_typ)), ("not all Literals have type of Collection")))
  end
| resolve_OclTerm (Let (str,typ,rhs_term,_,in_term,_)) model = 
  let
      val _ = trace medium ("RESOLVE a Let-Expression \n")
      val rrhs_term = resolve_OclTerm rhs_term model
      val rrhs_typ = type_of_term rrhs_term
      val rin_term = resolve_OclTerm in_term model
      val rin_typ = type_of_term rin_term
  in
      (Let (str,typ,rrhs_term,rrhs_typ,rin_term,rin_typ))
  end
| resolve_OclTerm (If (cond_term,cond_typ,if_expr,if_typ,else_expr,else_typ,ret_typ)) model = 
  let
      val _ = trace medium ("RESOLVE a If-Expression \n")
      val rterm = resolve_OclTerm cond_term model
      val rtyp = type_of_term rterm

      val rif_expr = resolve_OclTerm if_expr model
      val rif_typ = type_of_term rif_expr

      val relse_expr = resolve_OclTerm else_expr model
      val relse_typ = type_of_term relse_expr
  in 
      if (rtyp = Boolean) then
	  if (conforms_to rif_typ relse_typ model) then 
	      If(rterm,rtyp,rif_expr,rif_typ,relse_expr,relse_typ,relse_typ)
	  else if (conforms_to relse_typ rif_typ model) then
	      If(rterm,rtyp,rif_expr,rif_typ,relse_expr,relse_typ,rif_typ)
	  else
	      raise TypeCheckerResolveIfError (If (cond_term,cond_typ,if_expr,if_typ,else_expr,else_typ,ret_typ),("Types of if-expression and else-expression don't conform each other \n"))
      else
	  raise TypeCheckerResolveIfError (If (cond_term,cond_typ,if_expr,if_typ,else_expr,else_typ,ret_typ),("Type of condition is not Boolean. \n"))
  end


(* RETURN: context option *)
fun check_context (Cond (path,op_name,op_sign,result_type,cond,pre_name,expr)) model =
let
    val _ = trace high ("Starts typechecking: ")
    val _ = trace high ("pre/post/body         : "  ^  Ocl2String.ocl2string false expr ^ "\n")
    val classifier = class_of_type  (Classifier (path)) model
    val oper_list = operations_of classifier
    val oper = find_operation op_name oper_list
    val check1 = (op_sign = (#arguments oper))
    val check2 = (result_type = (#result oper))
    val _ = trace low ("check1 = " ^ Bool.toString check1 ^ ", check2 = " ^ Bool.toString check2 ^ "\n")
    val _ = List.map (fn (a,b) => (trace low (a ^ ":" ^ (string_of_OclType b) ^ "   "))) op_sign
in
    if check1  andalso check2 
    then
	(SOME((Cond (path,op_name,op_sign,(#result oper),cond,pre_name,resolve_OclTerm expr model)))) 
  else
	NONE
end

| check_context (Attr (path,typ,attrorassoc,expr)) model =
  let
      val _ = trace high ("Starts typechecking: ")
      val _ = trace high ("init/derive           : " ^ Ocl2String.ocl2string false expr ^ "\n")
      val classifier = class_of_type (Classifier (real_path path)) model
      val _ = trace low ( "classifier found ... " ^ "\n")
      val attr_list = attributes_of classifier
      val _ = trace low ( "attr_list found ... " ^ "\n")
      val attr = find_attribute (List.last path) attr_list
      val _ = trace low ( "attribute found ... " ^ "\n")
  in
      if (typ = #attr_type attr)
      then
	  let
	      val _ = trace low (" ... " ^ "\n")
	  in
	      (SOME ((Attr (path,(#attr_type attr),attrorassoc,resolve_OclTerm expr model))))
	  end
      else
	  NONE
  end
| check_context (Inv (path,name,expr)) model = 
  let
      val _ = trace high ("Starts typechecking: ")
      val _ = trace high ("inv                   : " ^ Ocl2String.ocl2string false expr ^ "\n")
  in
      (SOME (Inv (path,name, resolve_OclTerm expr model)))
  end
| check_context (Empty_context (s,t)) _ = raise NotYetSupportedError ("Empty_context not supported.\n")
(* SOME (Empty_context (s,t)) *)
| check_context (Guard (path,name,expr)) _ = raise NotYetSupportedError ("Guard not supported.\n") 
(* SOME (Guard (path,name,expr)) *)

(* RETURN: (context option) list *)
fun check_context_list [] model = [] 
  | check_context_list (h::context_list_tail) model  = 
    ((check_context h model
      handle wrongCollectionLiteral (term,mes) =>
	   let
	       val _ = trace high ("\n\n#################################################\n")
	       val _ = trace high ("wrongCollectionLiteral:\n")
	       val _ = trace high ("Error Message: " ^ mes ^ "\n")
	       val _ = trace high ("In Term: " ^ Ocl2String.ocl2string false term ^ "\n")
	   in
	       raise WrongContextChecked h
	   end
	 | CollectionRangeError (part,mes) =>
	   let
	       val _ = trace high ("\n\n#################################################\n")
	       val _ = trace high ("CollectionRangeError:\n")
	       val _ = trace high ("Error Message: " ^ mes ^ "\n")
	   in
	       raise WrongContextChecked h
	   end
	 | IteratorTypeMissMatch (term,mes) =>
	   let
	       val _ = trace high ("\n\n#################################################\n")
	       val _ = trace high ("IteratorTypeMissMatch:\n")
	       val _ = trace high ("Error Message: " ^ mes ^ "\n")
	       val _ = trace high ("In Term: " ^ Ocl2String.ocl2string false term ^ "\n")
	   in
	       raise WrongContextChecked h
	   end
	 | NoSuchIteratorNameError (term,mes) => 
	   let
	       val _ = trace high ("\n\n#################################################\n")
	       val _ = trace high ("NoSuchIteratorNameError:\n")
	       val _ = trace high ("Error Message: " ^ mes ^ "\n")
	       val _ = trace high ("In Term: " ^ Ocl2String.ocl2string false term ^ "\n")
	   in
	       raise WrongContextChecked h
	   end
	 | TypeCheckerResolveIfError (term,mes) => 
	   let
	       val _ = trace high ("\n\n#################################################\n")
	       val _ = trace high ("TypeCheckerResolveIfError:\n")
	       val _ = trace high ("Error Message: " ^ mes ^ "\n")
	       val _ = trace high ("In Term: " ^ Ocl2String.ocl2string false term ^ "\n")
	   in
	       raise WrongContextChecked h
	   end
	 | NotYetSupportedError mes => 
	   let
	       val _ = trace high mes
	   in
	       raise WrongContextChecked h
	   end
     )::(check_context_list context_list_tail model))
    handle WrongContextChecked h => 
	   let
	       val _ = trace high ("\n\n#################################################\n")
	       val _ = trace high ("WrongContextChecked:\n")
	       val _ = trace high ("In Context: " ^ (cxt_list2string [h]) ^ "\n")
	   in
	       []
	   end
 end
 
