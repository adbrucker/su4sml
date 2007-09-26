(*****************************************************************************
 * su4sml --- a SML repository for managing (Secure)UML/OCL models
 *             http://projects.brucker.ch/su4sml/
 *                                                                            
 * rep_transform.ML ---
 * This file is part of su4sml.
 *
 * Copyright (c) 2007, ETH Zurich, Switzerland
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
(* $Id: ROOT.ML 6662 2007-07-04 06:41:30Z brucker $ *)

signature REP_TRANSFORM =
sig

val transformClassifiers_ext : Rep_Core.transform_model -> Rep.Model
val transformClassifiers     : Rep_Core.transform_model -> Rep.Classifier list
val transformFile            : string -> Rep.Model

(* transforms *)
val transform_association_classes: Rep_Core.transform_model -> Rep_Core.transform_model (* split an association classe into a class and an association*)
val transform_qualifier          : Rep_Core.transform_model -> Rep_Core.transform_model
val transform_aggregation        : Rep_Core.transform_model -> Rep_Core.transform_model
val transform_n_ary_associations : Rep_Core.transform_model -> Rep_Core.transform_model (* remove n-ary associations *)
val transform_multiplicities     : Rep_Core.transform_model -> Rep_Core.transform_model (* remove multiplicities *)

(* helper functions *)
val get_prefix     : Rep_OclType.Path -> Rep_OclType.Path
val get_aend_name  : Rep_Core.associationend -> string
val generate_pairs : 'a list -> ('a * 'a) list (* including symmetry *)
(* quantify_allInstances : normal variables -> body -> result *)
val quantify_allInstances : Rep_OclTerm.OclTerm list -> Rep_OclTerm.OclTerm -> Rep_OclTerm.OclTerm
val update_classifier_with_constraint : Rep_Core.constraint -> Rep_Core.Classifier -> Rep_Core.Classifier
(* single: exactly 1 match *)
val update_classifiers_single : Rep_Core.Classifier list -> Rep_OclType.OclType -> (Rep_Core.Classifier -> Rep_Core.Classifier) -> Rep_Core.Classifier list
val update_classifiers_with_constraint: Rep_Core.Classifier list -> Rep_OclType.OclType -> Rep_Core.constraint -> Rep_Core.Classifier list

val get_association : Rep_Core.association list -> Rep_OclType.Path -> Rep_Core.association
(* only one of the below will remain *)
val get_other_associationends: Rep_Core.association list -> Rep_OclType.Path -> Rep_OclType.OclType -> Rep_Core.associationend list
val get_other_associationends_alt : Rep_Core.association list -> Rep_OclType.OclType -> Rep_OclType.Path -> Rep_Core.associationend list

val get_associationends : Rep_Core.association list -> Rep_OclType.Path -> Rep_Core.associationend list

end

structure Rep_Transform:REP_TRANSFORM =
struct

open library
open Rep_OclTerm
open Rep_OclType
open Rep_OclHelper
open Rep_Core

(** not found library funtioncs *)
fun uncurry f(x,y) = f x y
infix |>>
fun (x |>> f) = (f x)

(** thrown when something is not yet implemented *)
exception NotYetImplemented

(***********************************
 ******** Usefull functions ********
 ***********************************)
val triv_expr = Rep_OclTerm.Literal ("true",Rep_OclType.Boolean)

fun lowercase s = let val sl = String.explode s
		  in
		      String.implode ((Char.toLower (hd sl))::(tl sl))
		  end
	
(** chop-off the last part of the path *)	  
fun get_prefix (path:Path):Path =
    List.take (path,List.length path - 1)

fun get_aend_name (aend:associationend) =
    List.last (#name aend)

fun generate_pairs [] =
    error "in generate_pairs: argument list is empty" (* or simply return []? *)
  | generate_pairs [a] =
    [(a,a)]
  | generate_pairs [a,b] =
    (* not necessary *)
    [(a,b),(b,a)]
  | generate_pairs (x::xs) =
    let
	val pairs = map (fn a => (x,a)) xs
	val rev_pairs = map (fn a => (a,x)) xs
    in
	pairs@rev_pairs@(generate_pairs xs)
    end

(* nest the vars in x.allInstances->forAll expressions *)
(* FIXME: Literal correct? *)
fun quantify_allInstances [Variable var] body =  
    let
	val lit = Literal(#1 var ,#2 var)
    in
	ocl_forAll (ocl_allInstances lit) (Variable var) body
    end
  | quantify_allInstances ((Variable var)::vars) body = 
    let
	val lit = Literal(#1 var ,#2 var)
	val rest = quantify_allInstances vars body
    in
	ocl_forAll (ocl_allInstances lit) (Variable var) rest
    end
  | quantify_allInstances vars _ = 
    let
	val qnt = List.length vars
	val error_term = if (qnt > 0) then
			     ((Int.toString qnt)^" "^(term_name_of (hd vars)))
			 else
			     "nothing"
    in 
	error ("in quantify_allInstances: only Variables supported and at least 1 needed, "^
	       error_term^" provided")
    end


fun update_classifier_with_constraint constraint (Class {name,parent,attributes,operations,associations,
							 invariant,stereotypes,interfaces,thyname,activity_graphs}) =
    Class {name = name,
	   parent = parent,
	   attributes = attributes,
	   operations = operations,
	   associations = associations,
	   invariant = constraint::invariant,
	   stereotypes = stereotypes,
	   interfaces = interfaces,
	   thyname = thyname,
	   activity_graphs = activity_graphs}
  | update_classifier_with_constraint constraint (AssociationClass {name,parent,attributes,operations,associations,association,
								    invariant,stereotypes,interfaces,thyname,activity_graphs})=
    AssociationClass {name = name,
		      parent = parent,
		      attributes = attributes,
		      operations = operations,
		      associations = associations,
		      association = association,
		      invariant = constraint::invariant,
		      stereotypes = stereotypes,
		      interfaces = interfaces,
		      thyname = thyname,
		      activity_graphs = activity_graphs}
  | update_classifier_with_constraint constraint (Interface {name,parents,operations,stereotypes,invariant,thyname}) =
    Interface {name=name,
	       parents=parents,
	       operations=operations,
	       stereotypes=stereotypes,
	       invariant=constraint::invariant,
	       thyname=thyname} 
  | update_classifier_with_constraint constraint (Enumeration {name,parent,operations,literals,invariant,stereotypes,interfaces,thyname}) = 
     Enumeration {name = name,
		  parent = parent,
		  operations = operations,
		  literals=literals,
		  invariant = constraint::invariant,
		  stereotypes = stereotypes,
		  interfaces = interfaces,
		  thyname = thyname}
  | update_classifier_with_constraint constraint (Primitive {name,parent,operations,associations,invariant,stereotypes,interfaces,thyname}) = 
    Primitive{name = name,
	      parent = parent,
	      operations = operations,
	      associations = associations,
	      invariant = constraint::invariant,
	      stereotypes = stereotypes,
	      interfaces = interfaces,
	      thyname = thyname} 
  | update_classifier_with_constraint constraint (Template {parameter,classifier}) = 
    Template{parameter=parameter,
	     classifier = update_classifier_with_constraint constraint classifier} (* sensible? *)

fun update_classifiers_single (all_classifiers:Classifier list) (classifier:OclType) (update:Classifier -> Classifier) :Classifier list= 
    let
	val (match,rest) = List.partition (fn (Class {name,...}) => name=classifier
					    | (AssociationClass {name,...}) =>  name=classifier
					    | (Interface {name,...}) =>  name=classifier
					    | (Enumeration {name,...}) =>  name=classifier
					    | (Primitive {name,...}) =>  name=classifier
					    | _ => false ) all_classifiers
    in
	case match of 
	    [x] => (update x)::rest
	  | []  => error "in update_classifiers_single: no match found"
	  | _   => error "in update_classifiers_single: more than 1 match found"
    end

fun update_classifiers_with_constraint (all_classifiers:Classifier list) (classifier:OclType) (constraint:constraint) :Classifier list =
    update_classifiers_single all_classifiers classifier (update_classifier_with_constraint constraint)
    
fun get_association (all_assocs: Rep_Core.association list) (assoc_path:Path): association =
    let
    	val assoc = filter (fn {name,...}=> name=assoc_path) all_assocs
    in	
	case assoc of [x] => x
			  | []  => error "in get_association: no match found"
			  | _   => error "in get_association: more than 1 match found"
    end

fun get_other_associationends (all_assocs:association list) (assoc_path:Path) (cls_type:Rep_OclType.OclType):associationend list = 
    let
	fun all_others ({aend_type,...}:associationend) = (collection_type_of_OclType aend_type) <> cls_type
	val association = get_association all_assocs assoc_path
	val aends = filter all_others (#aends association)
    in
	aends
    end

(** a simple wrap for get_other_associationends *)
fun get_other_associationends_alt (all_assocs:association list) (cls_type:Rep_OclType.OclType) (assoc_path:Path):associationend list = 
    get_other_associationends all_assocs assoc_path cls_type


fun get_associationends (all_assocs:association list) (assoc_path:Path):associationend list = 
    let
	val assoc:association = get_association all_assocs assoc_path
    in
	#aends assoc
    end

(****************************
 ******** Transforms ********
 ****************************)

(** Remove qualifiers
 * requires: qualifier
 * generates: constraint, AssociationClass
 * removes: qualifier
 *)
fun transform_qualifier ((all_classifiers,all_associations):transform_model):transform_model = 
    (all_classifiers,all_associations) (*dummy*)

(** Remove aggregations
 * requires: aggregation
 * generates: constraint
 * removes: aggregation
 *)
fun transform_aggregation ((all_classifiers,all_associations):transform_model):transform_model = 
    (all_classifiers,all_associations) (*dummy*)


(** Transform an Association Class into a Class
 * requires: AssociationClass
 * generates: Class, constraint
 * removes:
 *)
fun transform_association_class_into_class (all_associations: association list )
    (Rep_Core.AssociationClass {name,parent,attributes,operations,
				associations,association,invariant,
				stereotypes,interfaces,thyname,activity_graphs})
    : Rep_Core.Classifier = 
    let
	(* the association of the association class to the original 
	 * association is injective, meaning that each instance of 
	 * the newly created class is associated with exactly one 
	 * association pair of the original association.
	 * The new association end already contains the 1..1 multi-
	 * plicity, so the constraint only needs to ensure, that
	 * each instance is "used" only once.
	 * To do this locally, simply make sure that all association
	 * end calls have a size of 1.
	 *)
	val self = self name
	val src_path = path_of_OclType name
	(* the corresponding association hasn't been modified yet *)
	val aends = get_associationends all_associations association
	fun handle_aend ({name,aend_type,...}:associationend) =
	    (src_path@[List.last name],aend_type)
	val parts = map handle_aend aends
	fun quantify (aend,target_type):OclTerm=
	    let
		(* FIXME: aendcall again *)
		val aend_call = ocl_aendcall self aend target_type
		val eq_constraint = ocl_eq (ocl_size aend_call) (Literal ("1",Integer))
	    in
		eq_constraint
	    end
	val quantified_parts = map quantify parts
 	val combined_parts:OclTerm = foldl (uncurry ocl_and) (hd quantified_parts) (tl quantified_parts)
	val constraint = (SOME (lowercase (List.last src_path) ^"_injective_constraint"),combined_parts)
    in
	Rep_Core.Class { name = name,
			 parent = parent,
			 attributes = attributes,
			 operations = operations,
			 associations = associations,
			 invariant = constraint::invariant,
			 stereotypes = stereotypes,
			 interfaces = interfaces,
			 thyname = thyname,
			 activity_graphs = activity_graphs}
    end
  | transform_association_class_into_class _ cls = error ("in transform_association_class_into_class: only AssociationClass supported, "^
							  (short_name_of cls)^" provided")

(** Transform an Association Class into an Association.
 * requires: AssociationClass 
 * generates: association
 * removes: 
 *)
fun transform_association_class_into_association (all_associations:association list)
						 (AssociationClass {name,association,...}):Rep_Core.association=
    let
	val name_path = path_of_OclType name
	val prefix = List.take(name_path,List.length name_path - 1)
	(* the name of the association end call (role) is the association class name *)
        val new_aend= {name = prefix@[List.last name_path] (* FIXME: convention? *), 
		       aend_type = name, (* target of the association is the original AssociationClass *)
		       multiplicity = [(1,1)], (* dummy *)
		       visibility = XMI.public (* dummy *),
		       ordered = false, (* dummy *)
		       init = NONE (* dummy *)
		      }:Rep_Core.associationend
	val aends = get_associationends all_associations association
	val names = map (List.last o #name) (new_aend::aends)
	(* pretty printing *)
	fun combine [] = ""
	  | combine [x,y] = x^"_"^y
	  | combine (x::xs) = x^"_"^(combine xs)
	val combined = combine names
    in
	{name = prefix@["Association_"^combined] (* FIXME: better/proper convention? *),
         aends = new_aend :: aends,
	 aclass = NONE
	}
    end
  | transform_association_class_into_association _ cls = error ("in transform_association_class_into_association: only AssociationClass supported, "^
								(short_name_of cls)^" provided")

(** Transform an AssociationClass into a Class and an Association
 * requires: AssociationClass
 * generates: Class, Association, constraint
 * removes: AssociationClass
 *)
fun transform_association_classes ((classifiers,associations):transform_model):transform_model =
    let
	val (association_classes,other_classifiers) = 
	    List.partition (fn (Rep_Core.AssociationClass x) => true
			     | _                             => false)  classifiers
	val modified_classifiers = map (transform_association_class_into_class associations) association_classes
	val modified_associations = map (transform_association_class_into_association associations) association_classes
    in
	(other_classifiers @ modified_classifiers,
	 associations @ modified_associations)
    end


(* FIXME: binary only or general? *)
(** Move multiplicities from association ends to classifier constraints.
 * requires: binary multiplicities
 * generates: constraints
 * removes: multiplicities
 *)
fun transform_multiplicities ((classifiers,all_associations):transform_model):transform_model =
    let
	fun binary_constraint (Variable src_var) (aend_path:Path) target_type (multiplicity as (lower,upper)):OclTerm =
	    let
		(* FIXME: is ocl_aendcall src aend t correct? *)
		(* FIXME: handling of *? *)
		val aend_call = ocl_aendcall (Variable src_var) aend_path target_type
		val lower_bound = ocl_geq (ocl_size aend_call) (Literal (Int.toString lower,Integer)) (* size >= lower *)
		val upper_bound = ocl_leq (ocl_size aend_call) (Literal (Int.toString upper,Integer)) (* size <= upper *)
		val combined_bound = ocl_and lower_bound upper_bound
	    in
		combined_bound
	    end	
	  | binary_constraint term _ _ _ = error ("in transform_multiplicities.binary_constraint: only Variables supported, "^
					       (term_name_of term)^" supplied")

	fun generate_multiplicity_constraints ({name,aends=[a,b],aclass}:association):(OclType * constraint) list =
	    let
		fun all_binary_constraints src_type ({name,aend_type,multiplicity,...}:associationend) =
		    let
			val self = self src_type
			val aend_name = List.last name
			val aend = path_of_OclType src_type @[aend_name]
			val binary_parts = map (binary_constraint self aend aend_type) multiplicity
			val combined_parts = foldl (uncurry ocl_or) (hd binary_parts) (tl binary_parts)
			val constraint_name = List.last (path_of_OclType src_type) ^"_"^ aend_name ^"_"^
					      (List.last (path_of_OclType aend_type))
		    in
			(SOME constraint_name,combined_parts)
		    end
		    
		val ab = all_binary_constraints (#aend_type a) b
		val ba = all_binary_constraints (#aend_type b) a
	    in
		[(#aend_type a,ab),(#aend_type b,ba)]
	    end
	  | generate_multiplicity_constraints {name,aends,aclass}  = 
	    let
		(* FIXME: all.. *)
		(* FIXME: take isNavigable into account *)
		fun update_wrap target_type (constraint,classifiers):Rep_Core.Classifier list= 
		    update_classifiers_with_constraint classifiers target_type constraint
		fun handle_aend (({multiplicity,aend_type=target_type,name=aend_name,...},classifiers)
				 :(Rep_Core.associationend * Rep_Core.Classifier list)):Rep_Core.Classifier list =
		    let
(*			val source_name = short_name_of classifier
			val src_var = Variable (string_of_OclType source_name ^"_to_"^ aend_name,source_type)
			val aend_path = path_of_OclType source_type @ [aend_name]
			val constraints = map (binary_constraint src_var aend_path target_type) multiplicity
*)		    in
(*			foldl (update_wrap target_type) classifiers constraints
*)                      []
		    end
	    in
(*		foldl handle_aend all_classifiers associationends
*)              []
	    end
	fun add_multiplicity_constraints ((assoc,classifiers):association * Classifier list):Classifier list =
	    let
		val constraint_list:((OclType * constraint) list) = generate_multiplicity_constraints assoc
		fun fold_update ((cls_type,constraint),classfiers) =
		    update_classifiers_with_constraint classifiers cls_type constraint
		val modified_classifiers = foldl fold_update classifiers constraint_list
	    in
		modified_classifiers
	    end
	fun strip_multiplicities ({name,aends,aclass}:association):association =
	    let
		fun handle_aend ({name,aend_type,multiplicity,visibility,ordered,init}):associationend =
		    {name=name,
		     aend_type=aend_type,
		     multiplicity=[],
		     visibility=XMI.public (* dummy *),
		     ordered=ordered,
		     init=init}
		val modified_aends = map handle_aend aends
	    in
		{name   = name,
		 aends  = modified_aends,
		 aclass = aclass}
	    end
	 
	(* add the constraints to the classifiers *)
	val modified_classifiers = foldl add_multiplicity_constraints classifiers all_associations
	(* update the associationends *)
	val modified_associations = map strip_multiplicities all_associations
    in
	(modified_classifiers, modified_associations)
    end
	
	
(** Process an association, add it to the processed list and update the relevant classifier invariants.
 * For each association end, generate the matching-constraint and add it to the classifier. Multiplicities 
 * are not handled here.
 *)
fun handle_n_ary_constraint ((association as {name,aends=[a,b],aclass},(all_classifiers,processed_assocs))
			     :(association * transform_model)): transform_model = 
    (all_classifiers,association::processed_assocs)
  | handle_n_ary_constraint ((association as {name,aends,aclass},(all_classifiers,processed_assocs))
			     :(association * transform_model)) :transform_model = 
    let
	fun collection_equality coll1 coll2 = 
	    ocl_and (ocl_includesAll coll1 coll2) (ocl_includesAll coll2 coll1)
		    
	(* generate the aend call *)
	fun generate_collection ({name=target_name, aend_type=target_type, ...}:associationend) 
				({name=souce_name, aend_type=source_type,...}:associationend) =
	    let
		(* FIXME: what's the proper convention? *)
		(* variable name: <souce_name>_<aend_call_name>_<target_name> *)
		val var_name =  lowercase (List.last (path_of_OclType source_type)) ^"_"^
				(lowercase (List.last target_name))^"_"^
				(lowercase (List.last (path_of_OclType target_type)))
		val var = Variable(var_name,source_type)
	    in
		(* FIXME: ocl_aendcall source aend t, is 't' correct/...? *)
		(* path of aend call: source path + aend name (as with 
                 * attributes *)
		(ocl_aendcall var ((path_of_OclType source_type)@[List.last target_name]) (Collection target_type),var)
	    end

	(* link the seperate aend call results together 
	 * needs at least 2 elements *)
	fun match_associations (match::coll::rem) variables:OclTerm = 
	    (* match is the linking element *)
	    let
		val equalities = map (collection_equality match) (coll::rem)
		val body = foldl (uncurry ocl_and) (hd equalities) (tl equalities)
	    in
		quantify_allInstances variables body
	    end
	   | match_associations elements variables = error ("in handle_n_ary_constraint.match_associations: at least 2 elements needed, "^
							    (Int.toString (List.length elements))^" provided")
	    
	(* update the participant with the new constraint *)
	fun n_ary_local_part (classifiers: Classifier list) (remaining_connection:associationend list) (aend:associationend):Classifier list = 
	    let
		val (collections,vars) = ListPair.unzip (map (generate_collection aend) remaining_connection)
		val constraint = (SOME ("n-ary association constraint for "^(List.last name)), match_associations collections vars)
		val classifier_type =  #aend_type aend
	    in
		update_classifiers_with_constraint all_classifiers classifier_type constraint
	    end
	
	(* Instead of fold, for clarity.
	 * Each step updates the participant of the association end *)
	fun iterate_over_connection f clses done [] = clses
	  | iterate_over_connection f clses done (x::xs) = iterate_over_connection f  (f clses (done@xs) x) (x::done) xs
	val modified_classifiers = iterate_over_connection n_ary_local_part all_classifiers [] aends (* aends > 2 *)
    in
	(modified_classifiers, association::processed_assocs)
    end

fun handle_n_ary_constraints ((classifiers,associations):transform_model):transform_model =
    (* using fold for commulative transformation *)
    foldl handle_n_ary_constraint (classifiers,[]) associations
 

fun split_n_ary_association (ac as {name,aends=[a,b],aclass}:association):association list =
    [ac]
  | split_n_ary_association (ac as {name,aends,aclass}:association) =
    (* We need to generate the pairs as well as the new names *)
    let
	val prefix = get_prefix name
	fun process (a,b) = {name=prefix@[get_aend_name a ^"_"^ (get_aend_name b)],
			     aends=[a,b],
			     aclass=aclass}
	(* FIXME: reflexiv parts? *)
	(* No dupplicates due to symmetry generated *)
	fun associate source targets = map (fn x => (source,x)) targets
	fun iterate [] = error "in split_n_ary_association.iterate: at least 2 elements needed, 0 provided"
	  | iterate [x] = error "in split_n_ary_association.iterate: at least 2 elements needed, 1 provided"
	  | iterate [x,y] = [(x,y)]
	  | iterate (src::rest) = associate src rest @ (iterate rest)
	val pairs = iterate aends
	val binary_associations = map process pairs
    in
	binary_associations 
    end
    
fun split_n_ary_associations ((classifiers,associations):transform_model):transform_model =
    (classifiers, List.concat (map split_n_ary_association associations))


(** We need to add OCL constraints to handle the broken relationship
 * The problem is, that when splitting an n-ary association into it's
 * components, the matching of the now 'local' associations is lost. 
 * For instance, participants A and B are associated to participant C
 * with mulitplicity 2..3.                                           
 * 1. After the splitting, A may point to 2 instances, while B points
 *    to 3 instances.                                                
 * 2. Even if the cardinalities agree, A and B may point to different
 *    instances of C.                                                
 * Instead of having every participant re-check the same constraints,
 * each participant will only validate it's own multiplicity boundary.
 * Additionally, the 2 points are handled separately, such that all  
 * multiplicities will be removed silmultaniously. For this, the 
 * newly generated binary associations need to retain the correct 
 * association names for the association end calls.
 *
 * requires: n-ary associations
 * generates: constraints, binary associations
 * removes: n-ary associations
 *)
fun transform_n_ary_associations ((classifiers,associations):transform_model):transform_model =
    handle_n_ary_constraints (classifiers,associations) |>> (* pack the association bindings into constraints *)
    split_n_ary_associations                                  (* n-ary -> binary *)



(*******************************
 ******** Control part  ********
 *******************************)


(** 
 * Transformations on Classifiers and Associations
 *)
fun transformClassifiers_ext (model:transform_model):transform_model =
    transform_association_classes model |>>  (* split an association classe into a class and an association*)
(*    transform_qualifier |>>
    transform_aggregation |>>
*)    transform_n_ary_associations |>>         (* remove n-ary associations *)
    transform_multiplicities              (* remove multiplicities *)

fun transformClassifiers (model:transform_model):Rep.Classifier list =
    fst (transformClassifiers_ext model) (* return classifiers *)


fun normalize_ext ((classifiers,associations):transform_model) =
    (map (Rep.normalize associations) classifiers, associations)


(** 
 * read and transform an .xmi file.
 * @return a list of rep classifiers, or nil in case of problems
 *) 
fun transformFile f:transform_model = (info ("opening "^f);
                       (normalize_ext  o transformClassifiers_ext o RepParser.transformXMI_ext o XmiParser.readFile) f)
(*    handle ex as (IllFormed msg) => raise ex *)

exception FileNotFound of string

fun printStackTrace e =
    let val ss = CompilerExt.exnHistory e
    in
        print_stderr ("uncaught exception " ^ (General.exnMessage e) ^ " at:\n");
        app (fn s => print_stderr ("\t" ^ s ^ "\n")) ss
    end



end
