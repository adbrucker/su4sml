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


    exception TC_wrongCollectionLiteral of Rep_OclTerm.OclTerm * string
    exception TC_CollectionRangeError of Rep_OclTerm.CollectionPart * string
    exception TC_IteratorTypeMissMatch of Rep_OclTerm.OclTerm * string 
    exception TC_NoSuchIteratorNameError of Rep_OclTerm.OclTerm * string 
    exception TC_TypeCheckerResolveIfError of Rep_OclTerm.OclTerm * string
    exception TC_NotYetSupportedError of string
    exception TC_WrongContextChecked of Context.context
    exception TC_RootError of string 
(*    exception TC_AsSetError of (Rep_OclTerm.OclTerm * string list * int * 
 * 			     (Rep_OclTerm.OclTerm * Rep_OclType.OclType) list *  Rep_Core.transform_model)
 *    exception TC_DesugaratorCall of (Rep_OclTerm.OclTerm * string list * int * 
 *			  (Rep_OclTerm.OclTerm * Rep_OclType.OclType) list * Rep_Core.transform_model) *)
    exception TC_IterateError of string
    exception TC_IterateAccumulatorTypeError of string
    exception TC_IterateTypeMissMatch of string
    exception TC_NoSuchAttributeError of string 
    exception TC_NoSuchOperationError of string
    exception TC_OperationWithTypeError of string

    val check_context_list            : Context.context list -> Rep_Core.transform_model -> Context.context option list
    val check_context                 : Context.context -> Rep_Core.transform_model -> Context.context option
    val resolve_OclTerm               : Rep_OclTerm.OclTerm -> Rep_Core.transform_model -> Rep_OclTerm.OclTerm
    val resolve_CollectionPart        : Rep_Core.transform_model -> Rep_OclTerm.CollectionPart -> Rep_OclTerm.CollectionPart 
    val resolve_arguments             : (Rep_OclTerm.OclTerm * Rep_OclType.OclType) list -> Rep_Core.transform_model -> (Rep_OclTerm.OclTerm * Rep_OclType.OclType) list
end

structure TypeChecker:TYPECHECKER  = 
 struct
open library
open Rep_Core
open Rep_OclTerm
open Rep_OclType
open Context
open RepParser
open XMI_DataTypes
open Preprocessor
open OclLibrary
open Ocl2String

type operation = Rep_Core.operation
type attribute = Rep_Core.attribute

exception TC_RootError of string 
exception TC_wrongCollectionLiteral of Rep_OclTerm.OclTerm * string
exception TC_CollectionRangeError of Rep_OclTerm.CollectionPart * string
exception TC_IteratorTypeMissMatch of Rep_OclTerm.OclTerm * string 
exception TC_NoSuchIteratorNameError of Rep_OclTerm.OclTerm * string 
exception TC_TypeCheckerResolveIfError of Rep_OclTerm.OclTerm * string
exception TC_NotYetSupportedError of string
exception TC_WrongContextChecked of context
exception TC_AsSetError of (OclTerm * string list * int * (OclTerm * OclType) list *  Rep_Core.transform_model)
exception TC_DesugaratorCall of (OclTerm * string list * int * (OclTerm * OclType) list * Rep_Core.transform_model)
exception TC_IterateError of string			     
exception TC_IterateAccumulatorTypeError of string
exception TC_IterateTypeMissMatch of string			  
exception TC_NoSuchAttributeError of string 
exception TC_NoSuchOperationError of string
exception TC_OperationWithTypeError of string
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
fun FromSet_desugarator rterm path attr_or_meth rargs (model as (cls,assocs):Rep_Core.transform_model) =
    if (attr_or_meth = 0)
    then (* OperationCall *)
	let
	    (* check 'fromSet' *)
	    val _ = trace low ("\n==> FromSet-desugarator: operation ... \n")
	    val new_type = type_of_template_parameter (type_of_term rterm)
	    val iterVar = (("anonIterVar_" ^ (varcounter.nextStr())),new_type)
	    val class = class_of_term (Variable (iterVar)) model
	    val ops = get_overloaded_methods class (List.last path) model
	in
	    if (List.length ops = 0)
	    then raise UpcastingError ("FromSet no operation/attribute found. \n")
	    else
		let
		    val insert_term = upcast_op ops (Variable iterVar) rargs model
		    val it_type = type_of_term insert_term
		in
		    Iterator ("collect",[iterVar],rterm,type_of_term rterm,insert_term,it_type,it_type)
		end
	end
    else (* AttributeCall *)
	let
	    (* check 'fromSet' *)
	    val _ = trace low ("\n==> FromSet-desugarator: attribute/assocend ... \n")
	    val new_type = type_of_template_parameter (type_of_term rterm)
	    val iterVar = (("anonIterVar_" ^ (varcounter.nextStr())),new_type)
	    val class = class_of_term (Variable (iterVar)) model
	    val attrs_or_assocs = get_overloaded_attrs_or_assocends class (List.last path) model
	in
	    if (List.length attrs_or_assocs = 0)
	    then raise UpcastingError ("Attriubte '" ^ (List.last path) ^ "' does not exist ... \n") 
	    else 
		let
		    val insert_term = upcast_att_aend attrs_or_assocs (Variable iterVar) model
		    val it_type = type_of_term insert_term
		    val _ = trace development ("association type " ^ string_of_OclType it_type ^ "\n")
        	    (* special case *)
        	   (* if it is an attribute, there needs to be added a collection type constructor *)
		
		in
		    case (List.hd attrs_or_assocs) of 
			(* AttributeCall *)
			(x,SOME(shit),NONE) => 
			let
			    val ret_type = substitute_templ_para (type_of_term rterm) it_type
			in
			    Iterator ("collect",[iterVar],rterm,type_of_term rterm,insert_term,it_type,ret_type)
			end
			(* AssociatonCall *)
		      | (x,NONE,SOME(shit)) => 
			if (isColl_Type it_type) 
			then
			    let
				val ret_type = substitute_templ_para (type_of_term rterm) (type_of_template_parameter it_type)
			    in
				Iterator("collect",[iterVar],rterm,type_of_term rterm,insert_term,it_type,ret_type)
			    end
			else
			    let
				val ret_type = substitute_templ_para (type_of_term rterm) it_type
			    in
				Iterator("collect",[iterVar],rterm,type_of_term rterm,insert_term,it_type,ret_type)
			    end
		end
	end	
	
(* RETURN: OclTerm (OperationCall/AttributeCall) *)
fun AsSet_desugarator rterm path attr_or_meth rargs (model as (cls,assocs)) =
    let
	val _ = (trace function_calls ("TypeChecker.AsSet_desugarator class= " ^ (string_of_OclType (type_of_term rterm)) ^ " , attr\n"))
	val res = if (attr_or_meth = 0) 
		  then (* OperationCall *)
		      let
			  val _ = trace low ("\n==> AsSet-desugarator: operation ... \n")
			  val rtyp = Set(type_of_term rterm)
			  val _ = trace low ("Type of source term " ^ string_of_OclType rtyp ^ " ---> try Set(" ^ string_of_OclType rtyp ^ ")\n")
			  val class = class_of_term (Variable ("anonIterVar_" ^ (varcounter.nextStr()),rtyp)) model
			  val ops = get_overloaded_methods class (List.last path) model
			  val new_rterm = CollectionLiteral([CollectionItem(rterm,type_of_term rterm)],rtyp)
		      in
			  if (List.length ops = 0)
			  then
			      raise TC_NoSuchOperationError ("interefere_methods: No operation signature matches given types (source: "^(Ocl2String.ocl2string false rterm)^").")
			  else
			      upcast_op ops new_rterm rargs model
		      end
		  else (* AttributeCall *)
		      let
			  val _ = trace low ("\n==> AsSet-desugarator: attribute/assocend\n")
			  val rtyp = Set(type_of_term rterm)
			  val _ = trace low (string_of_OclType rtyp ^ "\n")
			  val class = class_of_term (Variable ("anonIterVar_" ^ (varcounter.nextStr()),Set(rtyp))) model
			  val attrs = get_overloaded_attrs_or_assocends class (List.last path) model
			  (* source term is a dummy-Term *) 
			  val new_rterm = CollectionLiteral([CollectionItem(rterm,type_of_term rterm)],rtyp)
			  val _ = trace low ("'AsSetError' ... \n")
		      in
			  if (List.length attrs = 0) 
			  then 
			      raise TC_NoSuchAttributeError ("Attriubte '" ^ (List.last path) ^ "' does not exist ... \n") 
			  else 
			      upcast_att_aend attrs new_rterm model
		      end
	val _ = trace function_ends ("TypeChecker.AsSet_desugarator class= " ^ (string_of_OclType (type_of_term rterm)) ^ " , attr\n")
    in
	res
    end
    
(* RETURN: OclTerm (OperationCall/AttributeCall) *)
	fun desugarator rterm path attr_or_meth rargs model = 
    FromSet_desugarator rterm path attr_or_meth rargs model  
    handle UpcastingError s => AsSet_desugarator rterm path attr_or_meth rargs model

(* RETURN: CollectionPart *)
fun resolve_CollectionPart model (CollectionItem (term,typ))  =
    let
	val _ = trace function_calls ("TypeChecker.resolve_CollectionPart " ^ (ocl2string false term) ^ "\n")
	val rterm = resolve_OclTerm term model
	val rtyp = type_of_term rterm
	val res = (CollectionItem (rterm,rtyp))
	val _ = trace function_ends ("TypeChecker.resolve_CollectionPart " ^ (ocl2string false term) ^ "\n")
    in
	res
    end
  | resolve_CollectionPart model (CollectionRange (term1,term2,typ))  = 
    let
	val _ = trace function_calls ("TypeChecker.resolve_CollectionPart " ^ (ocl2string false term1) ^ "\n")
	val rterm1 = resolve_OclTerm term1 model
	val rtyp1 = type_of_term rterm1
	val rterm2 = resolve_OclTerm term2 model
	val rtyp2 = type_of_term rterm2
	val res = 
	    if (rtyp2 = rtyp1) then
		(CollectionRange (rterm1,rterm2,rtyp1))
	    else
		raise (TC_CollectionRangeError ((CollectionRange (term1,term2,typ)),("Begin and end of Range not of same type.\n")))
    	val _ = trace function_ends ("TypeChecker.resolve_CollectionPart " ^ (ocl2string false term1) ^ "\n")
    in
	res
    end

and resolve_CollectionLiteral (CollectionLiteral (part_list,typ)) model = 
    let
	val _ = trace function_calls ("TypeChecker.resolve_CollectionLiteral\n ")
	val rpart_list = List.map (resolve_CollectionPart model) part_list
	val tlist = List.map (type_of_CollPart) rpart_list
	val res = 
	    if (List.all (type_equals (List.hd (tlist))) tlist)
	    then
		rpart_list
	    else
		raise TC_wrongCollectionLiteral ((CollectionLiteral (part_list,typ)),"Not all Literals have the same type.\n")
	val _ = trace function_ends ("TypeChecker.resolve_CollectionLiteral\n ")
    in
	res
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
      val _ = trace function_calls ("TypeChecker.resolve_OclTerm Literal " ^ ocl2string false (Literal(s,typ)) ^ "\n")
      val _ = trace medium ("RESOLVE Literal: " ^ s ^ "\n")
      val res = (Literal (s,typ))
      val _ = trace function_ends ("TypeChecker.resolve_OclTerm Literal " ^ ocl2string false (Literal(s,typ)) ^ "\n")
  in
      res
  end
(* Variable *)
| resolve_OclTerm (Variable ("self",typ)) model = 
  let
      val _ = trace function_calls ("TypeChecker.resolve_OclTerm Variable " ^ ocl2string false (Variable("self",typ)) ^ "\n")
      val res = (Variable ("self",typ)) 
      val _ = trace function_ends ("TypeChecker.resolve_OclTerm Variable " ^ ocl2string false (Variable("self",typ)) ^ "\n")
  in
      res
  end
| resolve_OclTerm (Variable (name,typ)) model = 
  let
      val _ = trace function_calls ("TypeChecker.resolve_OclTerm Variable " ^ ocl2string false (Variable(name,typ)) ^ "\n")
      val res = Variable (name,typ)
      val _ = trace function_ends ("TypeChecker.resolve_OclTerm Variable " ^ ocl2string false (Variable(name,typ)) ^ "\n")
  in
      res 
  end
(* AssociationEndCall *)
| resolve_OclTerm (AssociationEndCall (term,_,["self"],_)) model =  (resolve_OclTerm (AttributeCall (term,OclAny,["self"],OclAny)) model) 
| resolve_OclTerm (AssociationEndCall (term,_,attr_path,_)) model = (resolve_OclTerm (AttributeCall (term,OclAny,attr_path,OclAny)) model)
(* AttributeCall *)
(* self.self -> self *)
| resolve_OclTerm (AttributeCall (term,_,["self"],_)) model = 
  let
      val _ = trace function_calls ("TypeChecker.resolve_OclTerm, AttributeCall, self, " ^ ocl2string false term ^ "\n")
      val res = (resolve_OclTerm term model)
      val _ = trace function_ends ("TypeChecker.resolve_OclTerm, AttributeCall, self " ^ ocl2string false term ^ "\n")
  in
      res
  end
| resolve_OclTerm (AttributeCall (term,_,attr_path,_)) (model as (cls,assocs)) =
  let 
      val _ = trace function_calls ("TypeChecker.resolve_OclTerm, AttributeCall, attribute name =  " ^ (List.last attr_path) ^ ", " ^ ocl2string true term ^ "\n")
      (* resolve source term *)
      val rterm = resolve_OclTerm term model
      val _ = trace wgen ("res AttCall : arrow or not " ^ List.hd (attr_path) ^ "\n")
      val _ = trace wgen ("res AttCall (" ^ (List.last attr_path) ^ ") : rterm = " ^ Ocl2String.ocl2string false rterm ^ "\n")
      val _ = trace wgen ("res AttCall (" ^ (List.last attr_path) ^ ") : rtype = " ^ string_of_OclType (type_of_term rterm) ^ "\n")
      val res = 
	  let
	  in
	      if (List.hd attr_path = "arrow")
	      then get_attr_or_assoc rterm (List.last attr_path) model
		   handle UpcastingError s => AsSet_desugarator rterm (List.tl attr_path) 1 [] model
	      else
		  get_attr_or_assoc rterm (List.last attr_path) model
		  
		  (*  2-dimensional inheritance of Collection types *)
		  handle UpcastingError s =>
			 (
			  (
			   let
			       val _ = trace low ("\n==> 2-dim Inheritance check: ma attribute/assocend\n")
			       val rtyp = type_of_term rterm
			       val _ = trace low (string_of_OclType rtyp ^ "manu \n")
			       val templ_type = type_of_template_parameter rtyp
			       val pclass = class_of_term (Variable ("x",templ_type)) model
			       val _ = trace low ("manu 2")
			       val ntempl_type = type_of_parent pclass 
			       val _ = trace low ("manu 3")
			       val new_type = substitute_templ_para rtyp ntempl_type
			       val new_class = class_of_term (Variable ("x",new_type)) model
			       val attrs = get_overloaded_attrs_or_assocends new_class (List.last attr_path) model
			       val _ = trace low ("parent type of term:" ^ string_of_OclType new_type ^ "\n")
			   in
			       if (List.length attrs = 0) 
			       then raise TC_DesugaratorCall (rterm,attr_path,1,[],model) 
			       else upcast_att_aend attrs rterm model
			   end    
			  ) 
			  handle TC_DesugaratorCall arg => desugarator (#1 arg) (#2 arg) (#3 arg) (#4 arg) (#5 arg)
			       | NoCollectionTypeError t => AsSet_desugarator  rterm attr_path 1 [] model
			       | Empty => AsSet_desugarator rterm attr_path 1 [] model
			 )
	  end
      val _ = trace function_ends ("TypeChecker.resolve_OclTerm \n")
  in
      res
  end
(* built in Operations not include in Library: oclIsKindOf, oclIsTypOf, oclAsType *) 
(* OperationWithType Calls *)
(* OCLISTYPEOF *)
| resolve_OclTerm (opcall as OperationCall (term,_,["oclIsTypeOf"],[(AttributeCall (source,_,[string_path], _),arg_type)],_)) (model as (cls,assocs)) =
  let
      fun attributes_to_path (Variable (x,y)) = (* end of package *) []
	| attributes_to_path (AttributeCall(source,_,[package_part],res_typ)) = 
	  (package_part)::(attributes_to_path source)
      (* prefix type of iterator variable *)
      val _ = trace function_calls ("TypeChecker.resolve_OclTerm, OperationCallWithType = oclIsTypeOf, " ^ ocl2string true term ^"\n")
      val rterm = resolve_OclTerm term model
      val _ = trace low ("res OpCall: oclIsTypeOf 2:  " ^ "\n")
      val rtyp = type_of_term rterm
      val _ = trace low ("res OpCall: oclIsTypeOf 3: "  ^ "\n")
      val path = attributes_to_path source
      val typ  = type_of_path path model
		handle GetClassifierError s => raise TC_OperationWithTypeError ("Wrong or ommited package in a OperationWithType call. Please ajust the the package of the type.\n" ^ "OclTerm is: " ^ ocl2string true opcall ^ "\n")
      val _ = trace low ("res OpCall: oclTypeOf 4:" ^ "... " ^ "\n")
      val res = OperationWithType (rterm,rtyp,"oclIsTypeOf",typ,Boolean)
      val _ = trace function_ends ("TypeChecker.resolve_OclTerm\n")
  in
      res
  end  
(* OCLISKINDOF *)
| resolve_OclTerm (opcall as OperationCall (term,_,["oclIsKindOf"],[(AttributeCall (Variable ("self",_),_,[string_path], _),argt)],_)) (model as (cls,assocs)) =
let
      (* prefix type of iterator variable *)
      val _ = trace function_calls ("TypeChecker.resolve_OclTerm, OperationCallWithType = oclIsTypeOf, " ^ ocl2string true term ^"\n")
      val rterm = resolve_OclTerm term model
      val _ = trace low ("res OpCall: oclIsTypeOf 2:  " ^ "\n")
      val rtyp = type_of_term rterm
      val _ = trace low ("res OpCall: oclIsTypeOf 3: "  ^ "\n")

      val class = class_of_term rterm model
      (* Path is maybe wrong, because: 
       *  i.)  the package of the context was ommited
       *  ii.) the package of another context was ommited
       * here we handle this cases.
       *)
      val correct_type = string_to_type string_path
      val ctyp = type_of (class_of_type correct_type model)
	  handle Empty => raise TC_OperationWithTypeError ("Wrong or ommited package in a OperationWithType call. Please ajust the the package of the type.\n" ^ "OclTerm is: " ^ ocl2string true opcall ^ "\n")
      val _ = trace low ("res OpCall: oclTypeOf 4:" ^ "... " ^ "\n")
      val res = OperationWithType (rterm,rtyp,"oclIsTypeOf",ctyp,Boolean)
      val _ = trace function_ends ("TypeChecker.resolve_OclTerm\n")
  in
      res
  end
(* OCLASTYPE *)
| resolve_OclTerm (opcall as OperationCall (term,_,["oclAsType"],[(AttributeCall (Variable ("self",_),_,[string_path], _),argt)],_)) (model as (cls,assocs)) =
let
      (* prefix type of iterator variable *)
      val _ = trace function_calls ("TypeChecker.resolve_OclTerm, OperationCallWithType = oclIsTypeOf, " ^ ocl2string true term ^"\n")
      val rterm = resolve_OclTerm term model
      val _ = trace low ("res OpCall: oclIsTypeOf 2:  " ^ "\n")
      val rtyp = type_of_term rterm
      val _ = trace low ("res OpCall: oclIsTypeOf 3: "  ^ "\n")

      val class = class_of_term rterm model
      (* Path is maybe wrong, because: 
       *  i.)  the package of the context was ommited
       *  ii.) the package of another context was ommited
       * here we handle this cases.
       *)
      val correct_type = string_to_type string_path
      val ctyp = type_of (class_of_type correct_type model)
	  handle Empty => raise TC_OperationWithTypeError ("Wrong or ommited package in a OperationWithType call. Please ajust the the package of the type.\n" ^ "OclTerm is: " ^ ocl2string true opcall ^ "\n")
      val _ = trace low ("res OpCall: oclTypeOf 4:" ^ "... " ^ "\n")
      val res = OperationWithType (rterm,rtyp,"oclIsTypeOf",ctyp,Boolean)
      val _ = trace function_ends ("TypeChecker.resolve_OclTerm\n")
  in
      res
  end
(* HARD CODED STUFF *)
| resolve_OclTerm (OperationCall (term,typ,[OclLibPackage,"OclAny","atPre"],[],_)) model = 
  let
      val _ = trace function_calls  ("TypeChecker.resolve_OclTerm, OperationCall atPre, " ^ ocl2string true term ^ "\n")
      (* resolve source term *)
      val rterm = resolve_OclTerm term model
      val rtyp = type_of_term rterm
      val _ = trace low  ("res OpCall: Type of source : " ^ string_of_OclType rtyp ^ "\n")
      val res = OperationCall (rterm,rtyp,[OclLibPackage,"OclAny","atPre"],[],rtyp)
      val _ = trace function_ends ("TypeChecker.resovle_OclTerm\n")
  in
      res
  end
| resolve_OclTerm (OperationCall (term,typ,meth_path,args,res_typ)) (model as (cls,assocs)) =
  let
      val _ = trace function_calls ("TypeChecker.resolve_OclTerm, OperatioCall: name = " ^ (List.last (meth_path)) ^ ", " ^ ocl2string true term ^ "\n")
      (* resolve source term *)
      val rterm = resolve_OclTerm term model
      val _ = trace low  ("res OpCall: Type of source : " ^ string_of_OclType (type_of_term rterm) ^ "\n")
      (* resolve arguments *)
      val rargs = resolve_arguments args model
      val _ = trace low  ("res OpCall: args resolved ...\n")
      val res = 
	  let
	  in
	      if (List.hd meth_path = "arrow")
	      then get_meth rterm (List.last meth_path) rargs model
		   handle UpcastingError s => AsSet_desugarator rterm (List.tl meth_path) 0 rargs model
	      else 
		  get_meth rterm (List.last meth_path) rargs model
		  (*  2-dimensional inheritance of Collection types *)
		  handle UpcastingError s =>	
			 (     
			  (
			   let
			       val _ = trace low ("\n==> 2-dim Inheritance check: attribute/assocend\n")
			       val rtyp = type_of_term rterm
			       val _ = trace low (string_of_OclType rtyp ^ "\n")
			       val templ_type = type_of_template_parameter rtyp
			       val pclass = class_of_term (Variable ("x",templ_type)) model
			       val ntempl_type = type_of_parent pclass 
			       val _ = trace low (string_of_OclType ntempl_type ^ "\n")
			       val new_type = substitute_templ_para rtyp ntempl_type
			       val new_class = class_of_term (Variable ("x",new_type)) model
			       val ops = get_overloaded_methods new_class (List.last meth_path) model
			       val _ = trace low ("parent type of term: " ^ string_of_OclType new_type ^ "\n")
			   in
			       if (List.length ops = 0) 
			       then raise TC_DesugaratorCall (rterm, meth_path, 0, rargs, model) 
			       else upcast_op ops rterm rargs model
			   end
			  )
			  handle TC_DesugaratorCall arg => desugarator (#1 arg) (#2 arg) (#3 arg) (#4 arg) (#5 arg)
			       | NoCollectionTypeError typ => AsSet_desugarator rterm meth_path 0 rargs model 
			       | Empty => AsSet_desugarator rterm meth_path 0 rargs model
			 )
	  end
      val _ = trace function_ends ("TypeChecker.resolve_OclTerm\n")
  in
      res
  end
(* Iterator *)
| resolve_OclTerm (Iterator (name,iter_vars,source_term,_,expr,expr_typ,res_typ)) (model as (cls,assocs)) =
  let
      (* resolve source term, type *)      
      val _ = trace function_calls ("TypeChecker.resolve_OclTerm,  Itertor: name = " ^ name ^ "\n")
      val rterm = resolve_OclTerm source_term model
      val rtyp = type_of_term rterm
      val _ = trace low ("res Iter (" ^ name ^ "): source type " ^ string_of_OclType (type_of_term  rterm) ^ "\n\n")
      (* get source classifier *)
      val source_class = class_of_term rterm model
      val _ = trace low ("res Iter (" ^ name ^ "): type of classifier: " ^ string_of_OclType (type_of source_class) ^ "\n")
      (* prefix types *)
      val prfx = (package_of_template_parameter (type_of source_class))
      val _ = trace low ("res Iter (" ^ name ^ "): Type prefixed ... \n")
      val piter_vars = List.map (fn (a,b) => (a,prefix_type prfx b)) iter_vars
      val piter_types = List.map (fn (a,b) => b) piter_vars 
      val _ = trace low ("res Iter (" ^ name ^ "): first iter types: " ^ string_of_OclType (List.hd piter_types) ^ "\n") 
      (* check if iterator types correspond to source type *)
      val static_iter_type = type_of_template_parameter (type_of (source_class))
      val _ = trace low ("Length of iter_types: " ^ Int.toString (List.length piter_types) ^ "\n")
      val _ = trace low ("parent of classifier: " ^ string_of_OclType (type_of_parent source_class) ^ "\n")
      val _ = trace low ("\nstatic iter type : " ^ string_of_OclType static_iter_type ^ "  \n")
      val _ = trace low ("iter types: " ^ string_of_OclType (List.hd piter_types) ^ "\n")
      val h2 = List.map (fn a => conforms_to a static_iter_type model) (piter_types)
      val check = List.all (fn a => a=true) h2
      val res = 
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
		       Iterator (name,piter_vars,rterm,rtyp,rexpr,type_of_term rexpr,flatten_type (substitute_templ_para (rtyp) (type_of_term rexpr)))
		     | _ => raise TC_NoSuchIteratorNameError (Iterator (name,iter_vars,source_term,DummyT,expr,expr_typ,res_typ),("No such Iterator ..."))
		  )
	      end
	  else      
	      raise TC_IteratorTypeMissMatch (Iterator (name,iter_vars,source_term,DummyT,expr,expr_typ,res_typ),("Iterator variable doesn't conform to choosen set \n"))
      val _ = trace function_ends ("TypeChecker.resolve_OclTerm\n")
  in
      res
  end
     
| resolve_OclTerm (Iterate (iter_vars,acc_var_name,acc_var_type,acc_var_term,sterm,stype,bterm,btype,res_type)) (model as (cls,assocs)) = 
  let
      (* resolve source term, type *)
      val _ = trace function_calls ("TypeChecker.resolve_OclTerm, Iterate: accumulator " ^ acc_var_name ^ "\n")
      val rterm = resolve_OclTerm sterm model
      val rtyp = type_of_term rterm
      val _ = trace medium ("res Iterate: source type " ^ string_of_OclType (type_of_term  rterm) ^ "\n\n")
      (* get source classifier *)
      val source_class = class_of_term rterm model
      val _ = trace medium ("res Iterate: type of classifier: " ^ string_of_OclType (type_of source_class) ^ "\n")
      (* prefix types *)
      val prfx = (package_of_template_parameter (type_of source_class))
      val _ = trace medium ("res Iterate: Type prefixed ... \n")
      val piter_vars = List.map (fn (a,b) => (a,prefix_type prfx b)) iter_vars
      val piter_types = List.map (fn (a,b) => b) piter_vars 
      val _ = trace medium ("res Iterate: first iter types: " ^ string_of_OclType (List.hd piter_types) ^ "\n") 
      (* check if iterator types correspond to source type *)
      val static_iter_type = type_of_template_parameter (type_of (source_class))
      val _ = trace medium ("Length of iter_types: " ^ Int.toString (List.length piter_types) ^ "\n")
      val _ = trace medium ("parent of classifier: " ^ string_of_OclType (type_of_parent source_class) ^ "\n")
      val _ = trace medium ("\nstatic iter type : " ^ string_of_OclType static_iter_type ^ "  \n")
      val _ = trace medium ("iter types: " ^ string_of_OclType (List.hd piter_types) ^ "\n")
      val h2 = List.map (fn a => conforms_to a static_iter_type model) (piter_types)
      val check = List.all (fn a => a=true) h2
      (* check if initial value of accumulator has correct type *)
      val racc_var_term = resolve_OclTerm acc_var_term model
      val racc_var_type = type_of_term racc_var_term
      val res = 
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
		  raise TC_IterateAccumulatorTypeError ("Type of accumulator does not conform to type of expression of accumulator")
	  else
	      raise TC_IterateTypeMissMatch ("Iterate variables doesn't conform to choosen set \n")
      val _ = trace function_ends ("TypeChecker.resolve_OclTerm\n")
  in
      res
  end     
| resolve_OclTerm (CollectionLiteral ([],typ)) model = 
  let
      val _ = trace function_calls ("TypeChecker.resolve_OclTerm CollectionLiteral\n")
      val res = CollectionLiteral ([],typ)
      val _ = trace function_ends ("TypeChecker.resolve_OclTerm\n")
  in
      res
  end
| resolve_OclTerm (CollectionLiteral (coll_parts,temp_typ)) model = 
  let
      val _ = trace function_calls ("TypeChecker.resolve_OclTerm CollectionLiteral\n")
      val r_coll_parts = List.map (resolve_CollectionPart model) coll_parts
      val typ = type_of_CollPart (List.hd r_coll_parts)
      val res =
	  if (List.all (correct_type_for_CollLiteral typ) r_coll_parts) then
	      (CollectionLiteral (r_coll_parts,substitute_templ_para temp_typ typ))
	  else
	      raise (TC_wrongCollectionLiteral ((CollectionLiteral (coll_parts,temp_typ)), ("not all Literals have type of Collection")))
      val _ = trace function_ends ("TypeChecker.resolve_OclTerm\n")
  in
      res
  end
| resolve_OclTerm (Let (str,typ,rhs_term,_,in_term,_)) model = 
  let
      val _ = trace function_calls ("TypeChecker.resolve_OclTerm a Let-Expression \n")
      val rrhs_term = resolve_OclTerm rhs_term model
      val rrhs_typ = type_of_term rrhs_term
      val rin_term = resolve_OclTerm in_term model
      val rin_typ = type_of_term rin_term
      val res = (Let (str,typ,rrhs_term,rrhs_typ,rin_term,rin_typ))
      val _ = trace function_ends ("TypeChecker.resolve_OclTerm")
  in
      res
  end
| resolve_OclTerm (If (cond_term,cond_typ,if_expr,if_typ,else_expr,else_typ,ret_typ)) model = 
  let
      val _ = trace function_calls ("TypeChecker.resolve_OclTerm a If-Expression \n")
      val rterm = resolve_OclTerm cond_term model
      val rtyp = type_of_term rterm

      val rif_expr = resolve_OclTerm if_expr model
      val rif_typ = type_of_term rif_expr

      val relse_expr = resolve_OclTerm else_expr model
      val relse_typ = type_of_term relse_expr
      val res = 
	  if (rtyp = Boolean) then
	      if (conforms_to rif_typ relse_typ model) then 
		  If(rterm,rtyp,rif_expr,rif_typ,relse_expr,relse_typ,relse_typ)
	      else if (conforms_to relse_typ rif_typ model) then
		  If(rterm,rtyp,rif_expr,rif_typ,relse_expr,relse_typ,rif_typ)
	      else
		  raise TC_TypeCheckerResolveIfError (If (cond_term,cond_typ,if_expr,if_typ,else_expr,else_typ,ret_typ),("Types of if-expression and else-expression don't conform each other \n"))
	  else
	      raise TC_TypeCheckerResolveIfError (If (cond_term,cond_typ,if_expr,if_typ,else_expr,else_typ,ret_typ),("Type of condition is not Boolean. \n"))
      val _ = trace function_ends ("TypeChecker.resolve_OclTerm\n")
  in
      res
  end


(* RETURN: context option *)
fun check_context (Cond (path,op_name,op_sign,result_type,cond,pre_name,expr)) (model as (cls,assocs)) =
let
    val _ = trace high ("Starts typechecking: ")
    val _ = trace type_checker ("pre/post/body         : "  ^  Ocl2String.ocl2string false expr ^ "\n")
    val classifier = class_of_type  (Classifier (path)) model
    val oper = get_operation op_name classifier model
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

| check_context (Attr (path,typ,attrorassoc,expr)) (model as (cls,assocs)) =
  let
      val _ = trace function_calls ("TypeChecker.check_context STARTS TYPECHECKING ...\n")
      val _ = trace type_checker ("init/derive           : " ^ Ocl2String.ocl2string false expr ^ "\n")
      val classifier = class_of_type (Classifier (real_path path)) model
      val _ = trace low ( "classifier found ... " ^ "\n")
      val attr_list = attributes_of classifier
      val _ = trace low ( "attr_list found ... " ^ "\n")
      val attr = valOf (get_attribute (List.last path) attr_list)
      val _ = trace low ( "attribute found ... " ^ "\n")
      val res = 
	  if (typ = #attr_type attr)
	  then
	      let
		  val _ = trace low (" ... " ^ "\n")
	      in
		  (SOME ((Attr (path,(#attr_type attr),attrorassoc,resolve_OclTerm expr model))))
	      end
	  else
	      NONE
      val _ = trace function_ends ("TypeChecker.check_context\n\n\n")
  in
      res
  end
| check_context (Inv (path,name,expr)) model = 
  let
      val _ = trace function_calls ("TypeChecker.check_context STARTS TYPECHECKING ...\n")
      val _ = trace type_checker ("inv                   : " ^ Ocl2String.ocl2string false expr ^ "\n")
      val res = (SOME (Inv (path,name, resolve_OclTerm expr model)))
      val _ = trace function_ends ("TypeChecker.check_context\n\n\n")
  in 
      res
  end
| check_context (Empty_context (s,t)) _ = raise TC_NotYetSupportedError ("Empty_context not supported.\n")
(* SOME (Empty_context (s,t)) *)
| check_context (Guard (path,name,expr)) _ = raise TC_NotYetSupportedError ("Guard not supported.\n") 
(* SOME (Guard (path,name,expr)) *)

(* RETURN: (context option) list *)
fun check_context_list [] model = []
  | check_context_list (h::context_list_tail) model  = 
    ((check_context h model
      handle TC_wrongCollectionLiteral (term,mes) =>
	   let
	       val s1 =  ("wrongCollectionLiteral:\n")
	       val s2 =  ("Error Message: " ^ mes ^ "\n")
	       val s3 =  ("In Term: " ^ Ocl2String.ocl2string false term ^ "\n")
	       val _ = trace exce (s1^s2^s3)
	   in
	       raise TC_WrongContextChecked h
	   end
	 | TC_CollectionRangeError (part,mes) =>
	   let
    	       val s1 =  ("CollectionRangeError:\n")
	       val s2 =  ("Error Message: " ^ mes ^ "\n")
	       val _ = trace exce (s1^s2)
	   in
	       raise TC_WrongContextChecked h
	   end
	 | TC_IteratorTypeMissMatch (term,mes) =>
	   let
	       val s1 =  ("IteratorTypeMissMatch:\n")
	       val s2 =  ("Error Message: " ^ mes ^ "\n")
	       val s3 =  ("In Term: " ^ Ocl2String.ocl2string false term ^ "\n")
	       val _ = trace exce (s1^s2^s3)
	   in
	       raise TC_WrongContextChecked h
	   end
	 | TC_NoSuchIteratorNameError (term,mes) => 
	   let
	       val s1 =  ("NoSuchIteratorNameError:\n")
	       val s2 =  ("Error Message: " ^ mes ^ "\n")
	       val s3 =  ("In Term: " ^ Ocl2String.ocl2string false term ^ "\n")
	       val _ = trace exce (s1^s2^s3)
	   in
	       raise TC_WrongContextChecked h
	   end
	 | TC_TypeCheckerResolveIfError (term,mes) => 
	   let
	       val s1 =  ("TypeCheckerResolveIfError:\n")
	       val s2 =  ("Error Message: " ^ mes ^ "\n")
	       val s3 =  ("In Term: " ^ Ocl2String.ocl2string false term ^ "\n")
	       val _ = trace exce (s1^s2^s3)
	   in
	       raise TC_WrongContextChecked h
	   end
	 | TC_NotYetSupportedError mes => 
	   let
	       val s1 =  ("TC_NotYetSupportedError:\n")
	       val s2 =  ("Error Message: " ^ mes ^ "\n")
	       val _ = trace exce (s1^s2)
	   in
	       raise TC_WrongContextChecked h
	   end
	 | TC_OperationWithTypeError mes => 
	   let	       
	       val s1 =  ("TC_OperationWithType:\n")
	       val s2 =  ("Error Message: " ^ mes ^ "\n")
	       val _ = trace exce (s1^s2)
	   in
	       raise TC_OperationWithTypeError mes
	   end
     )::(check_context_list context_list_tail model))
    handle TC_WrongContextChecked h => 
	   let
	       val s1 =  ("\n\n#################################################\n")
	       val s2 =  ("WrongContextChecked:\n")
	       val s3 =  ("In Context: " ^ (cxt_list2string [h]) ^ "\n")
	       val _ = trace exce (s1^s2^s3)
	   in
	       raise TC_RootError ("Something went wrong!\n")
	   end
 end
 
