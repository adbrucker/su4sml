(*****************************************************************************
 * su4sml --- a SML repository for managing (Secure)UML/OCL models
 *             http://projects.brucker.ch/su4sml/
 *                                                                            
 * library.sml --- 
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

signature EXT_LIBRARY = 
sig

    (* operations with parents *)
    val class_of_parent         : Rep_Core.Classifier -> Rep_Core.Classifier list -> Rep_Core.Classifier
    val type_of_parent          : Rep_Core.Classifier -> Rep_OclType.OclType
    
    (* classifiers and packages *) 
    val class_of_type           : Rep_OclType.OclType -> Rep_Core.Classifier list -> Rep_Core.Classifier
    val get_classifier          : Rep_OclTerm.OclTerm -> Rep_Core.Classifier list -> Rep_Core.Classifier
    val package_of_template_parameter : Rep_OclType.OclType -> string list

    (* operations for types *)
    val type_equals             : Rep_OclType.OclType -> Rep_OclType.OclType -> bool
    val prefix_type             : string list -> Rep_OclType.OclType -> Rep_OclType.OclType
    val flatten_type            : Rep_OclType.OclType -> Rep_OclType.OclType
    val template_parameter      : Rep_OclType.OclType -> Rep_OclType.OclType
    val isColl_Type             : Rep_OclType.OclType -> bool
    val replace_templ_para      : Rep_OclType.OclType -> Rep_OclType.OclType -> Rep_OclType.OclType
    val string_to_type          : string list -> Rep_OclType.OclType
    val correct_type_for_CollLiteral : Rep_OclType.OclType -> Rep_OclTerm.CollectionPart -> bool
    val type_of_CollPart        : Rep_OclTerm.CollectionPart -> Rep_OclType.OclType
    val dispatch_collection     : (string * Rep_OclType.OclType) -> Rep_OclType.OclType
    
    (* operations for terms *)
    val prefix_expression       : string list -> Rep_OclTerm.OclTerm -> Rep_OclTerm.OclTerm
    val type_of_term            : Rep_OclTerm.OclTerm -> Rep_OclType.OclType
    val find_operation          : string -> Rep_Core.operation list -> Rep_Core.operation    
    val find_attribute          : string -> Rep_Core.attribute list -> Rep_Core.attribute
    
    (* operations for inheritance *)
    val conforms_to             : Rep_OclType.OclType -> Rep_OclType.OclType -> Rep_Core.Classifier list -> bool
    val upcast                  : (Rep_OclTerm.OclTerm * Rep_OclType.OclType) -> Rep_OclTerm.OclTerm
    val args_interfereable      : (string * Rep_OclType.OclType) list -> (Rep_OclTerm.OclTerm * Rep_OclType.OclType) list -> Rep_Core.Classifier list -> bool
    val interfere_args          : (string * Rep_OclType.OclType) list -> (Rep_OclTerm.OclTerm * Rep_OclType.OclType) list -> Rep_Core.Classifier list -> (Rep_OclTerm.OclTerm * Rep_OclType.OclType) list
    val interfere_methods       : (Rep_Core.Classifier * Rep_Core.operation) list -> Rep_OclTerm.OclTerm -> (Rep_OclTerm.OclTerm * Rep_OclType.OclType) list -> Rep_Core.Classifier list -> Rep_OclTerm.OclTerm
    val interfere_attrs_or_assocends: (Rep_Core.Classifier * Rep_Core.attribute option * Rep_Core.associationend option) list -> Rep_OclTerm.OclTerm -> Rep_Core.Classifier list -> Rep_OclTerm.OclTerm
    val get_overloaded_methods  : Rep_Core.Classifier -> string -> Rep_Core.Classifier list -> (Rep_Core.Classifier * Rep_Core.operation) list
    val get_overloaded_attrs_or_assocends    : Rep_Core.Classifier -> string -> Rep_Core.Classifier list -> (Rep_Core.Classifier * Rep_Core.attribute option * Rep_Core.associationend option) list
    val get_meth                : Rep_OclTerm.OclTerm -> string -> (Rep_OclTerm.OclTerm * Rep_OclType.OclType) list -> Rep_Core.Classifier list -> Rep_OclTerm.OclTerm
    val get_attr_or_assoc                : Rep_OclTerm.OclTerm -> string -> Rep_Core.Classifier list -> Rep_OclTerm.OclTerm

    (* operations/values for debugging/logging *)
    val trace                   : int -> string -> unit
    val log_level               : int ref
    val zero                    : int 
    val high                    : int 
    val medium                  : int 
    val low                     : int 
    val development             : int 
    
    (* exceptions *)
    exception InterferenceError of string
    exception TemplateInstantiationError of string
    exception NoModelReferenced of string
    exception GetClassifierError of string
    exception TemplateError of string
    exception NoCollectionTypeError of Rep_OclType.OclType
    exception NoSuchAttributeError of string
    exception NoSuchAssociationEndError of string
    exception NoSuchOperationError of string

end
structure Ext_Library:EXT_LIBRARY =
struct
        
open Rep_Core
open Rep_OclType
open Rep_OclTerm
open OclLibrary
     
exception InterferenceError of string
exception TemplateInstantiationError of string
exception NoModelReferenced of string
exception GetClassifierError of string
exception TemplateError of string
exception NoCollectionTypeError of OclType
exception NoSuchAttributeError of string
exception NoSuchAssociationEndError of string
exception NoSuchOperationError of string
				  
(* Error logging *)
(* default value *)
val log_level = ref 5
		
(* debugging-levels *)
val zero = 0
val high = 5
val medium = 20
val low = 100
val development = 200
		  
		  
		  
(* RETURN: unit *)
fun trace lev s = if (lev  <= !log_level ) then print(s) else ()

(* RETURN: OclType *)
fun dispatch_collection (selector,typ) = 
    case selector of
	"Set" => Set (typ)
      | "Sequence" => Sequence (typ)
      | "Collection" => Collection (typ)
      | "OrderedSet" => OrderedSet (typ)
      | "Bag" => Bag (typ)
      | _ => DummyT

(* RETURN: Boolean *)
fun correct_type_for_CollLiteral coll_typ (CollectionItem (term,typ)) = 
    if (typ = coll_typ) then 
	true
    else
	false
  | correct_type_for_CollLiteral coll_typ (CollectionRange (term1,term2,typ)) =
    if (typ = coll_typ) then
	true
    else
	false

(* RETURN: operation *)
fun find_operation op_name [] = raise NoSuchOperationError ("no such operation")
  | find_operation op_name ((oper:operation)::tail) = 
    if (op_name = #name oper)
    then
	let
	    val _ = trace low ("Operation found ... " ^ "\n");
	in 
	    oper
	end
    else
	let 
	    val _ = trace low ("no match: next op ..." ^ (#name oper) ^ "\n")
	in
	    find_operation op_name tail
	end

(* RETURN: attribute *)
fun find_attribute attr_name [] = 
    let
	val _ = trace low ("Error ... " ^ "\n")
    in
	raise (NoSuchAttributeError ("Error: no attribute '"^attr_name^" found"))
    end
  | find_attribute attr_name ((a:attribute)::attribute_list) = 
    if (attr_name = #name a) then 
	let
	    val _ = trace low ("Attribute found ... " ^ "\n")
	in 
 	    (a)
	end
    else
	let
	    val _ = trace low ("Attribute not found ... " ^ "\n")
	in
	    (find_attribute attr_name attribute_list)
	end

(* RETURN: OclType *)
fun flatten_type (Set(Set(t))) = Set(t)
  | flatten_type (Set(Collection(t))) = Set(t)
  | flatten_type (Set(Bag(t))) = Set(t)
  | flatten_type (Set(OrderedSet(t))) = Set(t)
  | flatten_type (Set(Sequence(t))) = Set(t)

  | flatten_type (Collection(Set(t))) = Collection(t)
  | flatten_type (Collection(Bag(t))) = Collection(t)
  | flatten_type (Collection(OrderedSet(t))) = Collection(t)
  | flatten_type (Collection(Sequence(t))) = Collection(t)
  | flatten_type (Collection(Collection(t))) = Collection(t)

  | flatten_type (OrderedSet(Collection(t))) = OrderedSet(t)
  | flatten_type (OrderedSet(Bag(t))) = OrderedSet(t)
  | flatten_type (OrderedSet(OrderedSet(t))) = OrderedSet(t)
  | flatten_type (OrderedSet(Sequence(t))) = OrderedSet(t)
  | flatten_type (OrderedSet(Set(t))) = OrderedSet(t)

  | flatten_type (Bag(Collection(t))) = Bag(t)
  | flatten_type (Bag(Bag(t))) = Bag(t)
  | flatten_type (Bag(OrderedSet(t))) = Bag(t)
  | flatten_type (Bag(Sequence(t))) = Bag(t)
  | flatten_type (Bag(Set(t))) = Bag(t)

  | flatten_type (Sequence(Collection(t))) = Sequence(t) 
  | flatten_type (Sequence(Bag(t))) = Sequence(t)
  | flatten_type (Sequence(OrderedSet(t))) = Sequence(t)
  | flatten_type (Sequence(Sequence(t))) = Sequence(t)
  | flatten_type (Sequence(Set(t))) = Sequence(t)

  | flatten_type typ = typ

(* RETURN: bool *)
fun member x [] = false
| member x (h::tail) = 
    if (x = h) then
	true
    else 
	member x tail

(* RETURN: Path *)
fun real_path ([]) = []
  | real_path ([x]) = []
  | real_path (x::tail) = x::real_path tail

(* RETURN: Boolean *)
fun isColl_Type (Set(x)) = true
  | isColl_Type (Sequence(x)) = true
  | isColl_Type (OrderedSet(x)) = true
  | isColl_Type (Bag(x)) = true
  | isColl_Type (Collection(x)) = true
  | isColl_Type _ = false

(* RETURN: OclTerm *)
fun type_of_term (Literal (s,typ)) = typ
  | type_of_term (AttributeCall (t,typ,p,res_typ)) = res_typ
  | type_of_term (AssociationEndCall (t,typ,p,res_typ)) = res_typ
  | type_of_term (OperationCall (t,typ,p,l,res_typ)) = res_typ
  | type_of_term (Variable (s,typ)) = typ
  | type_of_term (CollectionLiteral (set,typ)) = typ
  | type_of_term (Iterator (_,_,_,_,_,_,res_typ)) = res_typ
  | type_of_term (If(_,_,_,_,_,_,res_typ)) = res_typ
  | type_of_term (OperationWithType (_,_,_,_,res_typ)) = res_typ
  | type_of_term (Let (_,_,_,_,_,res_typ)) = res_typ
  | type_of_term (Iterate (_,_,_,_,_,_,_,_,res_typ)) = res_typ

fun type_of_CollPart (CollectionItem (term,typ)) = typ
  | type_of_CollPart (CollectionRange (term1,term2,typ)) = typ

(* RETURN: OclType *)
fun type_of_parent (Class {parent,...}) = 
    let
	val _ = trace development ("type_of_parent : Class{parent,...} \n")
    in
    ( case parent of 
	 NONE =>  OclAny
       | SOME (t) => t
    )
    end
  | type_of_parent (Primitive {parent, ...}) = 
    ( case parent of 
	 NONE =>  OclAny
       | SOME (t) => t
    )
  | type_of_parent (Interface {parents, ...}) = List.hd parents
  | type_of_parent (Template{classifier,...}) = 
    let
	val _ = trace development ("type_of_parent: Template {classifier,...} \n")
    in
	type_of_parent classifier
    end 

(* RETURN: string list *)
fun adjust_path [] [] = []
  | adjust_path [] list = list
  | adjust_path (h1::tail1) (h2::tail2) = 
    if (h1 = h2) 
    then (h1::(adjust_path tail1 tail2))
    else (h1::tail1)@(h2::tail2)

	
(* RETURN: OclType *)
fun prefix_type [] typ = typ
  | prefix_type h (Classifier (path)) = Classifier (h@[List.last path])
  | prefix_type h (Set (t)) = Set (prefix_type h t)
  | prefix_type h (Collection (t)) = Collection (prefix_type h t)
  | prefix_type h (OrderedSet (t)) = OrderedSet (prefix_type h t)
  | prefix_type h (Sequence (t)) = Sequence (prefix_type h t)
  | prefix_type h (Bag (t)) = Bag (prefix_type h t)
  | prefix_type h  basic_type = basic_type

(* RETURN: (string * OclType) list *)
fun prefix_signature ext_path [] = []
  | prefix_signature ext_path ((s,typ)::tail) = 
    (s,prefix_type ext_path typ)::(prefix_signature ext_path tail)

(* RETURN: CollectionPart *)
fun prefix_collectionpart ext_path (CollectionItem (term,typ)) = 
    (CollectionItem (prefix_expression ext_path term,typ))
  | prefix_collectionpart ext_path (CollectionRange (first_term,last_term,typ)) =
    (CollectionRange (prefix_expression ext_path first_term,prefix_expression ext_path last_term,typ))


(* RETURN: OclTerm *)
and prefix_expression ext_path (Variable (s,t)) = (Variable (s,t))
  | prefix_expression ext_path (Literal(s,t)) = Literal (s,t)
  | prefix_expression ext_path (AttributeCall (sterm,stype,path,res_typ)) =
    (AttributeCall (prefix_expression ext_path sterm,stype,path,res_typ))
  | prefix_expression ext_path (OperationCall (sterm,stype,path,args,res_typ)) =
    (OperationCall (prefix_expression ext_path sterm,stype,path,args,res_typ))
  | prefix_expression ext_path (Iterator (name,iter_vars,sterm,styp,expr,expr_typ,res_typ)) =
    let
	val prefixed_vars = List.map (fn a => (#1 a,prefix_type ext_path (#2 a))) iter_vars
    in
	Iterator (name,prefixed_vars,prefix_expression ext_path sterm,styp,prefix_expression ext_path expr,expr_typ,res_typ)
    end
  | prefix_expression ext_path (Let (var_name,var_type,rhs_term,rhs_type,expr,expr_type)) =
    (Let (var_name,prefix_type ext_path var_type,rhs_term,rhs_type,prefix_expression ext_path expr,expr_type))
  | prefix_expression ext_path (If (cond,cond_type,then_e,then_type,else_e,else_type,res_type)) = 
    (If (prefix_expression ext_path cond,cond_type,prefix_expression ext_path then_e,then_type,prefix_expression ext_path else_e,else_type,res_type))
  | prefix_expression ext_path (OperationWithType (sterm,stype,para_name,para_type,res_type)) = 
    (OperationWithType (prefix_expression ext_path sterm,stype,para_name,para_type,res_type)) 
  | prefix_expression ext_path (CollectionLiteral (coll_part_list,typ)) =
    (CollectionLiteral (List.map (prefix_collectionpart ext_path) coll_part_list,typ))

(* RETURN: OclType *)
fun template_parameter typ =
    case typ of
	Set(t) => t
      | Sequence(t) => t
      | Bag(t) => t
      | Collection(t) => t
      | OrderedSet(t) => t
      | t => raise NoCollectionTypeError t

(* RETURN: OclType *)
fun replace_templ_para (Collection(tt)) t = Collection (t)
  | replace_templ_para (Set (tt)) t = Set (t)
  | replace_templ_para (OrderedSet (tt)) t = OrderedSet (t)
  | replace_templ_para (Sequence (tt)) t = Sequence (t)
  | replace_templ_para (Bag (tt)) t = Bag (t)
  | replace_templ_para t1 t2 = raise TemplateError ("Not possible to replace template parameter of a basic type. Type is: " ^ string_of_OclType t1 ^ " \n")

(* RETURN: String list *)
fun package_of_template_parameter typ = 
    case (typ) of
	Set (t) => (package_of_template_parameter (template_parameter t)
		    handle NoCollectionTypeError t => package_of_template_parameter t)
      | OrderedSet (t) => (package_of_template_parameter (template_parameter t)
			   handle NoCollectionTypeError t => package_of_template_parameter t)
      | Collection (t) => (package_of_template_parameter (template_parameter t)
			   handle NoCollectionTypeError t => package_of_template_parameter t)
      | Sequence (t) => (package_of_template_parameter (template_parameter t)
			 handle NoCollectionTypeError t => package_of_template_parameter t)
      | Bag (t) => (package_of_template_parameter (template_parameter t)
		    handle NoCollectionTypeError t => package_of_template_parameter t)
      | Integer => [OclLibPackage]
      | String => [OclLibPackage]
      | Boolean => [OclLibPackage]
      | Real => [OclLibPackage]
      | DummyT => []
      | OclVoid => []
      | OclAny => []
      | Classifier (p) => 
	if (length p > 1) then 
	    List.take (p,(length p) -1) 
        else 
	    []

(* RETURN: IDENDITAET *)
fun swap1 f a b c = f c b a
  
(* RETURN: (a' list * a' list ) *)
fun parse_string c ([]) = ([],[])
fun parse_string c (h::tail) =
     if (c = h) then
	 ([],h::tail)
     else
 	 (h::(#1 (parse_string c tail)),(#2 (parse_string c tail)))

(* RETURN: OclType *)
fun string_to_cons "Set" typ = Set(typ)
  | string_to_cons "Bag" typ = Bag(typ)
  | string_to_cons "Collection" typ = Collection (typ)
  | string_to_cons "OrderedSet" typ = OrderedSet (typ)
  | string_to_cons "Sequence" typ = Sequence (typ)

(* RETURN: OclType *)
fun string_to_type ["Integer"] = Integer
  | string_to_type ["Boolean"] = Boolean
  | string_to_type ["Real"] = Real
  | string_to_type ["OclAny"] = OclAny
  | string_to_type ["DummyT"] = DummyT
  | string_to_type ["String"] = String
  | string_to_type ["OclVoid"] = OclVoid
  | string_to_type [set] = 
    if (List.exists (fn a => if (a = (#"(")) then true else false) (String.explode set)) then
	(* set *)
	let
	    val tokens = parse_string (#"(") (String.explode set)
	    val cons = (#1 tokens)
	    (* delete first "(" and last ")" element *)
	    val tail = List.tl (real_path (#2 tokens))
	    val _ = TextIO.output(TextIO.stdOut,"tail "^ (String.implode tail) ^ "\n")

	in
	    string_to_cons (String.implode cons) (string_to_type ([String.implode tail]))
	end
    else
	Classifier ([set])
  | string_to_type list = Classifier (list)


(* RETURN: OclType *)
fun type_of_template (T as Template{classifier,parameter}) =
    case (name_of classifier) of
	["Collection(T)"] => Collection (parameter)
      | ["Set(T)"] => Set (parameter)
      | ["OrderedSet(T)"] => OrderedSet (parameter)
      | ["Bag(T)"] => Bag (parameter)
      | ["Sequence(T)"] => Sequence (parameter)


(*** TYPE INFERENCE ***)

(* RETURN: Boolean *)
fun type_equals Integer (Classifier ([OclLibPackage,"Real"])) = true
  | type_equals (Classifier ([OclLibPackage,"Integer"])) Real = true
  | type_equals _ OclAny = true
  | type_equals _ (Classifier ([OclLibPackage,"OclAny"])) = true
  | type_equals x y = 
    if (x = y) then
	true
    else
	false


(* INHERITANCE *)

(* RETURN: OclTyp *)
fun substitute_typ typ templ_type =
    let 
	val _ = trace low ("substitute type : " ^ (string_of_OclType typ) ^ " instead of " ^ (string_of_OclType templ_type) ^ " \n")
    in    
	case templ_type of  
	    (* innerst type *)
	    Sequence(TemplateParameter "T") => Sequence (typ)
	  | Set(TemplateParameter "T") => Set(typ)
	  | OrderedSet(TemplateParameter "T") => OrderedSet (typ)
	  | Collection(TemplateParameter "T") => Collection(typ)
	  | Bag(TemplateParameter "T") => Bag(typ)
	  (* nested types *)
	  | Sequence (t) => Sequence (substitute_typ typ t)
	  | Set (t) => Set (substitute_typ typ t)
	  | OrderedSet (t) => OrderedSet (substitute_typ typ t)
	  | Collection (t) => Collection (substitute_typ typ t)
	  | Bag (t) => Bag (substitute_typ typ t)
	  (* only parameter  *)
	  | TemplateParameter "T" => typ
	  (* basic types *)
	  | OclAny => OclAny
	  | OclVoid => OclVoid
	  | Integer => Integer
	  | Real => Real
	  | String => String
	  | Boolean => Boolean
	  | DummyT => DummyT
	  | Classifier (path) => Classifier (path)
	  (* else error *)
	  | _ => raise TemplateInstantiationError ("Template type not of type: Sequence, Set, OrderedSet, Collection or Bag")
    end
    
(* RETURN: (string * OclType ) list *)
fun substitute_args typ [] = []
  | substitute_args typ ((s,t)::tail) =
    let 
	val _ = trace low ("substitute argument : " ^ (string_of_OclType typ) ^ " template parameter of " ^ (string_of_OclType t) ^ " \n")
    in
	(s,substitute_typ typ t)::(substitute_args typ tail)
    end

(* RETURN: operation list*)
fun substitute_operations typ [] = []
  | substitute_operations typ ((oper:operation)::tail) =
    let 
	val _ = trace low ("\n\nsubstitute operation : " ^ (#name oper) ^ " ... \n")
	val args = substitute_args typ (#arguments oper)
	val res = substitute_typ typ (#result oper)
    in
	({
	 name = #name oper,
	 postcondition = #postcondition oper,
	 precondition = #precondition oper,
	 body = #body oper,
	 arguments = args,
	 result = res,
	 visibility = #visibility oper,
	 isQuery = #isQuery oper,
	 scope = #scope oper
	 }:operation)::(substitute_operations typ tail)
    end

(* RETURN: OclType option *)
fun substitute_parent (Set (t)) = SOME (Collection t)
  | substitute_parent (OrderedSet (t)) = SOME (Set (t))
  | substitute_parent (Sequence (t)) = SOME (Collection (t))
  | substitute_parent (Bag (t)) = SOME (Collection t)
  | substitute_parent (Collection (t)) = SOME (Collection (t))
  | substitute_parent t = SOME (Collection t)

(* RETURN: Classifier *) 
fun substitute_classifier typ classifier =
    let
	val _ = trace low ("substitute classifier: parameter type: " ^ string_of_OclType typ ^ " template type: " ^ string_of_OclType (type_of classifier) ^ "\n") 
	val styp = substitute_typ typ (type_of classifier)
	val ops = substitute_operations typ (operations_of classifier)
	val sparent = substitute_parent typ
    in
	(Class
	     {
	      name = styp,
	      (* take the parent of the template parameter *)
	      parent = sparent,
	      (* a template has no attributes *)
	      attributes = [],
	      operations = ops,
	      (* a template has no associationends *)
	      associationends = [],
	      (* a template has no invariants *)
	      invariant = [],
	      (* a template has no stereotypes *)
	      stereotypes = [],
	      (* a template has no interfaces *)
	      interfaces = [],
	      (* a template's thyname is NONE *)
	      thyname = NONE,
	      (* a template has no activity_graphs *)
	      activity_graphs = []
	})
    end
(* RETURN: classifier *) 


(* RETURN: Classifer *)
and get_classifier source model =
    let
	val typ = type_of_term (source)
	fun class_of_t typ cl = hd (List.filter (fn a => if ((type_of a) = typ) then true else false) cl) 
    in
	case typ of
	    (* Primitive types of lib *)
	    Boolean => class_of_t Boolean model
	  | Integer => class_of_t Integer model
	  | Real => class_of_t Real model
	  | String => class_of_t String model
	  (* Template types of lib *)
	  | Sequence (T) => templ_of (Sequence(TemplateParameter "T")) T model
	  | Set (T) => templ_of (Set(TemplateParameter "T")) T model
	  | OrderedSet (T) => templ_of (OrderedSet(TemplateParameter "T")) T model
	  | Bag (T) => templ_of (Bag(TemplateParameter "T")) T model
	  | Collection (T) => templ_of (Collection(TemplateParameter "T")) T model
	  (* Class types of lib *)
	  | OclVoid => class_of_t OclVoid model
	  | OclAny => class_of_t OclAny model
	  (* Model types *)
	  | Classifier (path) => class_of_t (Classifier (path)) model
	  | DummyT => 
	    let
		val _ = trace development ("GetClassifierError: DummyT \n")
	    in
		raise GetClassifierError ("No classifier of type: 'DummyT' \n")
	    end
	  | TemplateParameter (string) => 
	    let
		val _ = trace development ("GetClassifierError: TemplateParameter ("^ string ^") \n")
	    in
		raise GetClassifierError ("No classifier of type: 'TemplateParameter (string)' \n")
	    end
    end
 
(* RETURN: Classifier /* one-time classifier */ *)
(*         Return the "one-time" classifier.
           The classifier is instanciated form the corresponding
           template form the library. Its only instanciate for one
           use and not stored later in the library.
*) 
and templ_of temp_typ para_typ [] = raise TemplateInstantiationError ("Error during instantiating a template" ^ "\n")
  | templ_of temp_typ para_typ (Template{parameter,classifier}::tail) =
    let
	val _ = trace low ("Instantiate Template for classifier: " ^ (string_of_OclType (type_of classifier)) ^ "\n")
    in
	if ((type_of classifier) = temp_typ) then
	    substitute_classifier para_typ classifier
	else
	    templ_of temp_typ para_typ tail
    end
(* element in lib not a template *)
  | templ_of temp_typ para_typ (h::tail) = 
    let
	val _ = trace development ("shit")
    in
	templ_of temp_typ para_typ tail
    end

and class_of_type typ model = 
    get_classifier (Variable ("x",typ)) model

(* RETURN: Boolean *)
fun conforms_to_up _ OclAny _ = true
  | conforms_to_up (Set(T1)) (Collection(T2)) model =
    let
	val _ = trace low ("conforms_to_up: set -> collection \n")
    in
	if (conforms_to T1 T2 model) then 
	    true
	else 
	    false
    end
  | conforms_to_up (Bag(T1)) (Collection(T2)) model =
    let
	val _ = trace low ("conforms_to_up: bag -> collection \n")
    in    
	if (conforms_to T1 T2 model) then 
	    true
	else 
	    false
    end
  | conforms_to_up (Sequence(T1)) (Collection(T2)) model =
    let
	val _ = trace low ("conforms_to_up: sequence -> collection \n")
    in
	if (conforms_to T1 T2 model) then 
	    true
	else 
	    false
    end
  | conforms_to_up (OrderedSet(T1)) (Collection(T2)) model =
    let
	val _ = trace low ("conforms_to_up:  orderedset -> collection \n")
    in
	if (conforms_to T1 T2 model) then 
	    true
	else 
	    false
    end
  | conforms_to_up typ1 typ2  model = 
    let
	val class = class_of_type typ1 model
	val parents_types = type_of_parents (class) model
	val _ = trace low ("conforms_to_up:  ... \n")
    in
	member (typ2) (parents_types)
    end

and
(* RETRUN: Boolean *)
conforms_to x y model =
    let
	val _ = trace low ("conforms_to: " ^ string_of_OclType x ^ " -> " ^ string_of_OclType y ^ " ? \n")
    in
	if (x = y) then 
	    true
	else
	    if (type_equals x y) then
		true
	    else
		conforms_to_up x y model
    end

(* RETURN: OclTerm *)
and upcast (term,typ) =  
    if (type_equals (type_of_term term) typ) then
	term
    else
	OperationWithType (term,type_of_term term,"oclIsTypeOf",typ,typ)

(* RETURN: OclType list *)
and type_of_parents (Primitive {parent,...}) model =
    (    
     case parent of 
	 NONE => [OclAny]
       | SOME (OclAny) => [OclAny]
       | SOME (t) => (t)::(type_of_parents (class_of_type t model) model)
    )
  | type_of_parents (Class {parent,...}) model =
    (
     case parent of
	 NONE => [OclAny]
       | SOME (OclAny) => [OclAny]
       | SOME (t) => (t)::(type_of_parents (class_of_type t model) model)
    )
  | type_of_parents (Interface {parents,...}) model = parents
  | type_of_parents (Template {classifier,...}) model =
    raise TemplateInstantiationError ("During Instantiation of template parent needn't to be accessed")


(* RETURN: Classifier *)
fun class_of_parent (Class {parent,...}) clist = 
    (case parent of
	 NONE => get_classifier (Variable ("x",OclAny)) clist
       | SOME (others) => get_classifier (Variable ("x",others)) clist 
    )
  | class_of_parent (Primitive {parent,...}) clist = 
    (case parent of
	 NONE => class_of_type OclAny clist
       | SOME (others) => class_of_type  others clist
    )
  | class_of_parent (Interface {parents,...}) clist =
    (* TODO: change API *)
    (*  
     (case (List.last (parents)) of 
	  NONE => class_of_type OclAny clist
	| SOME (others) => class_of_type others clist
     )
     *)
    class_of_type (List.hd parents) clist
  | class_of_parent c (h::tail) = class_of_parent c tail 
 

(* RETURN: Boolean *)
fun args_interfereable [] [] model = true
  | args_interfereable ((str,typ)::tail) ((term,ttyp)::args) model =
    let
	val _ = trace low ("term type: " ^ (Ocl2String.ocl2string true term) ^ "\n")
	val _ = trace low ("must conform to: " ^ (string_of_OclType typ) ^ "\n")
    in
	if (conforms_to (type_of_term term) typ model) then 
	    true
	else
	    false
    end
  (* not same nuber of arguments *)
  | args_interfereable [x] list model = false
  | args_interfereable list [x] model = false

(* RETURN: (OclTerm * OclType) list *)
fun interfere_args [] [] model = []
  | interfere_args ((str,typ)::tail) ((term,_)::args) model  =
    let
	val _ = trace low ("interfere args" ^ "\n")
    in
	if (type_equals typ (type_of_term term)) then
	    (term,type_of_term term)::(interfere_args tail args model)
	else
	    if (conforms_to (type_of_term term) typ model) then
		(term,typ)::(interfere_args tail args model)
	    else
		raise InterferenceError ("Arguments are not interferebable \n") 
    end

(* RETURN: OclType *)
fun interfere_res_type t1 t2 model = 
    if (conforms_to t1 t2 model)
    then t2
    else raise InterferenceError ("Result type  does not conform \n") 


(* RETURN: OclTerm *)
fun interfere_methods [] source args model = 
    let
	val _ = trace development ("InterferenceError ... \n")
    in
	raise InterferenceError ("interefere_methods: No operation signature matches given types (source: "^(Ocl2String.ocl2string false source)^").")
    end
  | interfere_methods ((class,meth)::class_meth_list) source args model =
    let
	val _ = trace low ("\nInterfere method      : name : '" ^ name_of_op meth ^ "'\n")
       val check_source = conforms_to (type_of_term source) (type_of class) model
       val check_args = args_interfereable (#arguments meth) args model
       val _ = trace low ("Interfereable ?       : Source conforms : "  ^ Bool.toString check_source ^ "   Args conforms : " ^ Bool.toString check_args ^ "\n")
       val _ = trace low ("Return type of method : " ^ string_of_OclType (result_of_op meth) ^ "\n\n")
    in
	if (check_source andalso check_args) then
	    (* signature matches given types *)
	    (OperationCall(source,type_of class,(name_of class)@[name_of_op meth],interfere_args (#arguments meth) args model,result_of_op meth))	    
	else
	    (interfere_methods class_meth_list source args model)
    end

(* RETURN: (OclTerm) *)
fun interfere_attrs (class,attr:attribute) source model =
   let
       val check_source = conforms_to (type_of_term source) (type_of class) model
       val _ = trace low ("interfere attribute: check_source "^ Bool.toString check_source ^ "\n\n")
   in
       if check_source then
	   (* signature matches given types *)
	   SOME ((AttributeCall (source,type_of class,(name_of class)@[(#name attr)],(#attr_type attr))))
       else
	   NONE
   end

(* RETURN: OclTerm option*)
fun interfere_assocends (class,assocend:associationend) source model = 
    let 
	val check_source = conforms_to (type_of_term source) (type_of class) model 
	val _ = trace low ("Interfere assocend: check_source " ^ Bool.toString check_source ^ "\n")
	val _ = trace low ("type of assoc " ^ string_of_OclType (assoc_to_attr_type assocend) ^ "\n")
    in
	if check_source then
	    SOME ((AssociationEndCall (source,type_of class,(name_of class)@[(#name assocend)],assoc_to_attr_type assocend)))
	else
	    NONE
    end

(* RETURN: OclTerm *) 
fun interfere_attrs_or_assocends [] source model = raise InterferenceError ("interference_attr_or_assoc: No operation signature matches given types (source: " ^ (Ocl2String.ocl2string false source) ^ ").")
  | interfere_attrs_or_assocends ((class,SOME(attr:attribute),NONE)::class_attr_or_assoc_list) source model =
    (
     case (interfere_attrs (class,attr) source model) of
	 NONE => (interfere_attrs_or_assocends class_attr_or_assoc_list source model)
       | SOME (term) => term
    )
  | interfere_attrs_or_assocends ((class,NONE,SOME(assocend:associationend))::class_attr_or_assoc_list) source model = 
    (
     case (interfere_assocends (class,assocend) source model) of 
	 NONE => (interfere_attrs_or_assocends class_attr_or_assoc_list source model)
       | SOME (term) => term
    )

(* RETURN: Boolean *)
fun collection_type classifier = 
    case (type_of classifier) of
	Collection(T) => true
      | Set (T) => true
      | OrderedSet (T) => true
      | Sequence (T) => true
      | Bag (T) => true
      | x => false

(* RETURN: Boolean *)
fun end_of_recursion classifier = 
    case (type_of classifier) of
	Collection (T) => true
      | others => false

(* RETURN: (Classifier * operation ) list *)
fun get_overloaded_methods class op_name [] = raise NoModelReferenced ("in 'get_overloaded_methods' ...\n")
  | get_overloaded_methods class op_name model =
   let
        val _ = trace low("\n")
	val ops = operations_of class
	val _ = trace low("Look for methods for classifier: " ^ string_of_OclType (type_of class) ^ "\n")
	val ops2 = List.filter (fn a => (if ((#name a) = op_name) then true else false)) ops
	val _ = trace low("operation name                 : " ^ op_name ^ "  Found " ^ Int.toString (List.length ops2) ^ " method(s) \n")
	val parent = class_of_parent class model
	val _ = trace low("Parent class                   : " ^ string_of_OclType (type_of parent) ^ "\n\n")
	val cl_op = List.map (fn a => (class,a)) ops2
    in
       if (class = class_of_type OclAny model) 
       then (* end of hierarchie *)
	   if (List.length ops2 = 0) 
	   then[]
	   else[(class,List.hd(ops2))]
       else 
	   ( 
	    if (end_of_recursion class)  
	    then (* end of collection hierarchie *)
		if (List.length ops2 = 0) 
		then []
		else [(class,List.hd(ops2))]
	    else (* go up the hierarchie tree *)
		(
		 if (List.length ops2 = 0) 
		 then (get_overloaded_methods parent op_name model)
		 else (cl_op)@(get_overloaded_methods parent op_name model)
		)
	   )
   end

(* RETURN: (Classifier * attribute option * association option) list *)
fun get_overloaded_attrs_or_assocends class attr_name [] = raise NoModelReferenced ("in 'get_overloaded_attrs' ... \n")
  | get_overloaded_attrs_or_assocends class attr_name model =
   let
       val _ = trace low ("\n")
       val attrs = attributes_of class
       val assocends = associationends_of class
       val _ = trace low ("Look for attributes/assocends : Class: " ^ string_of_OclType (type_of class) ^ " \n")
       val attrs2 = List.filter (fn a => (if ((#name a) = attr_name) then true else false)) attrs
       val assocends2 = List.filter (fn a => (if ((#name a) = attr_name) then true else false)) assocends
       val _ = trace low ("Name of attr/assocend         : " ^ attr_name  ^ "   Found " ^ Int.toString (List.length attrs2) ^ " attribute(s), " ^ Int.toString (List.length assocends2) ^  " assocend(s) \n")
       val parent = class_of_parent class model
       val _ = trace low ("Parent class                  : " ^ string_of_OclType(type_of parent) ^ "\n\n")
       val cl_at = List.map (fn a => (class,SOME(a),NONE)) attrs2
       val cl_as = List.map (fn a => (class,NONE,SOME(a))) assocends2
   in
       if (class = class_of_type OclAny model) then
	   (* end of hierarchie *)
	   if (List.length attrs2 = 0) 
	   then if (List.length assocends2 = 0) 
		then []
		else
		    [(class,NONE,SOME(List.hd(assocends2)))]
	   else [(class,SOME(List.hd(attrs2)),NONE)]
       else
	   (
	    if (end_of_recursion class)
	    then (* end of collection hierarchie *)
		if (List.length attrs2 = 0)
		then if (List.length assocends2 = 0)
		     then []
		     else [(class,NONE,SOME(List.hd(assocends2)))]
		else [(class,SOME(List.hd(attrs2)),NONE)]
	    else (* go up the hierarchie tree *)
		(
		 if (List.length attrs2 = 0)
		 then if (List.length assocends2 = 0)
		      then (get_overloaded_attrs_or_assocends parent attr_name model)
		      else (cl_as)@(get_overloaded_attrs_or_assocends parent attr_name model)
		 else
		     (cl_at)@(get_overloaded_attrs_or_assocends parent attr_name model)
		)
	   )
   end
	
(* RETURN: OclTerm *)
fun get_meth source op_name args model=
    (* object type *)
    let
	val _ = trace low ("Type of Classifier : " ^ string_of_OclType (type_of_term source ) ^ "\n")
	val class = get_classifier source model
	val meth_list = get_overloaded_methods class op_name model
	val _ = trace low ("overloaded methods found: " ^ Int.toString (List.length meth_list) ^ "\n")
    in
	interfere_methods meth_list source args model
    end

(* RETURN: OclTerm *)
fun get_attr_or_assoc source attr_name model =
    let 
	val _ = trace low ("GET ATTRIBUTES OR ASSOCENDS: source term = " ^ Ocl2String.ocl2string false source ^ "\n")
	val class = get_classifier source model
	val attr_or_assocend_list = get_overloaded_attrs_or_assocends class attr_name model
	val _ = trace low ("overloaded attributes/associationends found: " ^ Int.toString (List.length attr_or_assocend_list) ^ "\n")
    in
	let 
	    val x = interfere_attrs_or_assocends attr_or_assocend_list source model
	    val _ = trace low ("\nReturn type of attribute: " ^ string_of_OclType (type_of_term x) ^ "\n\n")
	in
	    x
	end
    end
end
