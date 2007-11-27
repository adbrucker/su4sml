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


(* (JD) some ideas for medium to long-term refactorings:
 * 
 * maybe split up into two seperate structures. 
 * Rep_Transfrom for generic transformation functions, 
 * and Rep_TransformAssociations for transforming associations 
 * 
 * I could also imagine types like
 * type modelTransformation = Rep.Model -> Rep.Model 
 * type classifierTransformation = Rep.Classifier -> Rep.Classifier
 * ...
 * and functions like
 * forAllClassifiers : classifierTransformation -> modelTransformation
 * forMatchingClassifier : (Rep.Classifier -> Bool) -> classifierTransformation -> modelTransformation
 * ...
 *
 *)

signature REP_TRANSFORM =
sig

(* (JD) maybe not all of the following functions need to be exported.
 * e.g., generate_pairs, ...
 *)

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
val path_of_aend   : Rep_Core.associationend -> Rep_OclType.Path
val type_of_aend   : Rep_Core.associationend -> Rep_OclType.OclType
val association_of_aend : Rep_Core.associationend -> Rep_OclType.Path
val name_of_aend   : Rep_Core.associationend -> string
val multiplicities_of_aend :  Rep_Core.associationend -> (int*int) list

val get_qualifier  : Rep_OclType.Path -> Rep_OclType.Path
val get_short_name : Rep_OclType.Path -> string
val generate_pairs : 'a list -> ('a * 'a) list (* including symmetry *)
val update_classifier_with_constraint : Rep_Core.constraint -> Rep_Core.Classifier -> Rep_Core.Classifier
(* single: exactly 1 match *)
val update_classifiers_single : Rep_Core.Classifier list -> Rep_OclType.OclType -> (Rep_Core.Classifier -> Rep_Core.Classifier) -> Rep_Core.Classifier list
val update_classifiers_with_constraints: Rep_Core.Classifier list -> Rep_OclType.OclType -> Rep_Core.constraint  list -> Rep_Core.Classifier list

val get_association : Rep_Core.association list -> Rep_OclType.Path -> Rep_Core.association
val split_on_association: Rep_Core.association list -> Rep_OclType.Path -> Rep_Core.association * Rep_Core.association list
(* only one of the below will remain *)
val get_other_associationends: Rep_Core.association list -> Rep_OclType.Path -> Rep_OclType.OclType -> Rep_Core.associationend list
val get_other_associationends_alt : Rep_Core.association list -> Rep_OclType.OclType -> Rep_OclType.Path -> Rep_Core.associationend list
val get_associationends  : Rep_Core.association list -> Rep_OclType.Path -> Rep_Core.associationend list
val associationends_of   : Rep_Core.association -> Rep_Core.associationend list

(* result: (Variable list , OCL expression for set intersection)*)
val reachable_set  : Rep_Core.associationend -> Rep_Core.associationend list -> (Rep_OclTerm.OclTerm list * Rep_OclTerm.OclTerm)
val within_bounds  : Rep_OclTerm.OclTerm -> (int*int) -> Rep_OclTerm.OclTerm
val within_aend_multiplicities  : Rep_Core.associationend -> Rep_Core.associationend list -> string -> Rep_Core.constraint
val injective_constraint : Rep_OclType.Path -> Rep_OclType.OclType -> Rep_Core.associationend list -> string -> Rep_Core.constraint

exception NotYetImplemented of string
exception InvalidArguments of string

end

structure Rep_Transform:REP_TRANSFORM =
struct

open library
open Rep_OclTerm
open Rep_OclType
open Rep_OclHelper
open Rep_Core

infix |>>
fun (x |>> f) = (f x)

(** thrown when something is not yet implemented *)
exception NotYetImplemented of string
exception InvalidArguments of string

(***********************************
 ******** Usefull functions ********
 ***********************************)
val triv_expr = Rep_OclTerm.Literal ("true",Rep_OclType.Boolean)

(* (JD) -> Rep_Core? *)	
fun path_of_aend (aend:associationend) =
    #name aend

(* (JD) -> Rep_Core? *)	
fun type_of_aend (aend:associationend) =
    #aend_type aend

(* (JD) -> Rep_Core? *)	
fun association_of_aend (aend:associationend) =
    let 
	val name = #name aend
    in
	List.take(name, (List.length name)-1)
    end

(* (JD) -> Rep_Core? *)	
fun name_of_aend (aend:associationend):string =
    List.last (#name aend)

(* (JD) -> Rep_Core? *)	
fun multiplicities_of_aend (aend:associationend):(int*int)list =
    #multiplicity aend

(** chop-off the last part of the path *)	  
(* (JD) -> Rep_OclType? *)
fun get_qualifier (path:Path):Path =
    List.take (path,List.length path - 1)

(* (JD) -> Rep_OclType? *)
fun get_short_name (path:Path):string =
    List.last path

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

fun update_classifiers_with_constraints (all_classifiers:Classifier list) (classifier:OclType) (con::constraints:constraint list) :Classifier list =
    let
	val modified_clsses = update_classifiers_single all_classifiers classifier (update_classifier_with_constraint con)
    in
	update_classifiers_with_constraints modified_clsses classifier constraints
    end
  | update_classifiers_with_constraints all_classifiers _ [] = all_classifiers
    
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

(* (JD) -> Rep_Core? *)
fun associationends_of (assoc:association):associationend list =
    let
	val _ = print "associationends_of\n"
	val _ = print ("assocends_of: "^(string_of_path (#name assoc))^"\n")
	val _ = List.app (print o (fn x => x ^"\n") o name_of_aend) (#aends assoc)
    in
	#aends assoc
    end

fun reachable_set (_:associationend) ([]:associationend list) = error "rep_transform.get_reachable_set: empty source list"
  | reachable_set (target:associationend) ([source]:associationend list) =
    let
	val src_var = Variable(name_of_aend source ,type_of_aend source)
    in
	([src_var], ocl_aendcall src_var (path_of_aend target) (type_of_aend target))
    end
  | reachable_set (target:associationend) ((source::rest):associationend list) =
    let
	val (old_vars,intermediate) = reachable_set target rest
	val src_var = Variable(name_of_aend source ,type_of_aend source)
	val new_set = ocl_aendcall src_var (path_of_aend target) (type_of_aend target)
    in
	(src_var::old_vars ,ocl_intersection_set new_set intermediate)
    end


fun within_bounds (set:Rep_OclTerm.OclTerm) ((lower,upper):int*int):Rep_OclTerm.OclTerm =
    let 
	val size = ocl_size set
	val lower_lit = Literal (Int.toString lower,Integer)
	val upper_lit = Literal (Int.toString upper,Integer)
	val lower_bound = ocl_geq size lower_lit
	val upper_bound = ocl_leq size upper_lit
    in
	ocl_and lower_bound upper_bound
    end
fun within_aend_multiplicities (target:associationend) (sources:associationend list) (name:string):constraint =
    let
	val _ = trace function_calls "within_aend_multiplicities\n"
	val tgt_multiplicities = multiplicities_of_aend target
	val tgt_name = name_of_aend target
	val tgt_type = type_of_aend target
	val (variables,set) = reachable_set target sources
	val constr_body = ocl_or_all (map (within_bounds set) tgt_multiplicities)
	val tgt_variable = Variable(tgt_name,tgt_type)
	val allInstances = ocl_allInstances tgt_variable
	val constr_complete = ocl_forAll allInstances variables constr_body 
	val constraint = (SOME name,constr_complete)
    in
	constraint
    end

fun injective_constraint (source_path:Path) (source_type:OclType) (targets:associationend list) (name:string):constraint =
    let
	val source_name = get_short_name source_path
	val src_var = Variable(source_name,source_type)
	fun role_bounds src_var aend  = 
	    let
		val name = path_of_aend aend
		val aend_type = type_of_aend aend
		val set = ocl_aendcall src_var  name aend_type
		val size = ocl_size set
		val bounds = ocl_eq size (Literal("1",Integer))
	    in
		bounds
	    end
	fun role_equals src_var src2_var aend =
	    let
		val name = path_of_aend aend
		val aend_type = type_of_aend aend
		val set = ocl_aendcall src_var  name aend_type
		val size = ocl_size set
		val match = ocl_eq size (Literal("1",Integer))
	    in
		match
	    end
	val roles = map (role_bounds src_var) targets
	val roles_part =  ocl_and_all roles
	val allInstances = ocl_allInstances src_var
	val src_var2 = Variable(source_name^"2",source_type)
	val matches = map (role_equals src_var src_var2) targets
	val matches_anded = ocl_and_all roles
	val matches_equal = ocl_eq src_var src_var2
	val matches_imp = ocl_implies matches_anded matches_equal
	val allInstances2 = ocl_allInstances src_var2
	val matches_part = ocl_forAll allInstances2 [src_var2] matches_imp
	val constr_body = ocl_and roles_part matches_part
	val constr_complete = ocl_forAll allInstances [src_var] constr_body
	val constraint = (SOME name,constr_complete)
    in
	constraint
    end

fun split_on_association (associations:association list) (path:Path): (association * association list) =
    let
	fun belonging_association tgt {name,aends,aclass} =  tgt = name
	val ([assoc],others) = List.partition (belonging_association path) associations
    in
	(assoc, others)
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


(** Transform an AssociationClass into a Class
 * Each association class instance is associated with excatly one instance of the association it is 
 * attached to. Therefore, a simple (1,1) multiplicity isn't suffiecient, as it doesn't guarantee
 * that the association class is referenced only once. Since the 'perspective' is switched from
 * "generate_n_ary_constraint" from below, the constraint reflects this. For {A,X_1,...X_n}, A the 
 * association class as regular class, X_i the old association participants:
 * Constraint:  A.role_X_i == 1, for all X_i 
 *              A'.role_X_i == A.role_X_i, for all X_i implies A' == A
 * 
 * In OCL, this is:
 * context A inv:
 *   A.allInstances->forAll(a:A| a.role_X_1 = 1 and ... and a.role_X_n = 1 and
 *          A.allInstances->forAll(a2:A|a2.role_X_1=a.role_X_1 and ... a2.role_X_1=a.role_X_n implies a2=a )
 *   )
 * 
 * Since the name of the resulting class equals the original association class name/path/type,
 * paths referencing the original association class needn't be updated.
 * 
 * requires: AssociationClass
 * generates: Class, constraint
 * removes: AssociationClass
 *)
fun transform_association_class_into_class (all_associations: association list )
    (Rep_Core.AssociationClass {name,parent,attributes,operations,
				associations,association,invariant,
				stereotypes,interfaces,thyname,activity_graphs})
    : Rep_Core.Classifier = 
    let
	val _ = trace function_calls "transform_association_class_into_class\n"
	val (assoc,others) = split_on_association all_associations association
	val assoc_class_path = path_of_OclType name
	val assoc_class_name = get_short_name assoc_class_path
	val constr_name = "InjectiveAssociationClass"^assoc_class_name
	val src_path =  assoc_class_path
	val src_type = name
	val constraint = injective_constraint src_path src_type (associationends_of assoc) constr_name
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
 * Add the association class to the belonging association. All constraints are handled in 
 * "transform_association_class_into_class", so no special treatment is required here.
 * 
 * requires: AssociationClass 
 * generates: association
 * removes: 
 *)
fun transform_association_class_into_association (AssociationClass {name,association,...}, 
						  all_associations:association list):Rep_Core.association list =
    let
	val _ = trace function_calls "transform_association_class_into_association\n"
	val (assoc,others) = split_on_association all_associations association
	val assoc_path = association
	val assoc_class_path = path_of_OclType name
	val assoc_class_name = get_short_name assoc_class_path
	val new_aend= {name = assoc_path@[assoc_class_name] (* FIXME: convention? *), 
		       aend_type = name (* target of the association is the original AssociationClass *),
		       multiplicity = [],
		       visibility = XMI.public (* dummy *),
		       ordered = false (* dummy *),
		       init = NONE (* dummy *)
		      }:Rep_Core.associationend
	fun add_aend_to_and_update_association (new_aend:associationend) {name,aends,aclass} = {name=name,
									       aends=new_aend::aends,
									       aclass=NONE}
	val modified_association = add_aend_to_and_update_association new_aend assoc
    in
	modified_association::others
    end
  | transform_association_class_into_association (cls,_) = error ("in transform_association_class_into_association: only AssociationClass supported, but "^
								(short_name_of cls)^" provided")

(** Transform an AssociationClass into a Class and an Association
 * requires: AssociationClass
 * generates: Class, Association, constraint
 * removes: AssociationClass
 *)
fun transform_association_classes ((classifiers,associations):transform_model):transform_model =
    let
	val _ = trace function_calls "transform_association_classes\n"
	val (association_classes,other_classifiers) = 
	    List.partition (fn (Rep_Core.AssociationClass x) => true
			     | _                             => false)  classifiers
	val new_classifiers = map (transform_association_class_into_class associations) association_classes
	val modified_associations = foldl transform_association_class_into_association associations association_classes
    in
	(new_classifiers @ other_classifiers,
	 modified_associations)
    end



(** Move binary multiplicities from association ends to classifier constraints.
 * requires: binary associations
 * generates: constraints
 * removes: binary association multiplicities
 *)
fun transform_multiplicities ((classifiers,all_associations):transform_model):transform_model =
    let
	fun add_multiplicity_constraints (({name,aends=[],aclass},classifiers):association * Classifier list):Classifier list =
	    raise InvalidArguments "transform_multiplicities: asociation has no aends\n" 
	  | add_multiplicity_constraints ({name,aends=[a],aclass},classifiers) =
	    raise InvalidArguments "transform_multiplicities: asociation has only 1 aend\n"
	  | add_multiplicity_constraints (assoc as {name,aends=[a,b],aclass},classifiers) =
	    let
		val _ = trace function_calls "add_multiplicity_constraints\n"
		val a_type = type_of_aend a
		val b_type = type_of_aend b
		val a_name = name_of_aend a
		val b_name = name_of_aend b
		val a_constr_name = "BinaryAssociation"^a_name
		val b_constr_name = "BinaryAssociation"^b_name
		val modified_tmp = if multiplicities_of_aend a = [] 
				   then 
				       classifiers
				   else
				       let
					   val a_constraint = within_aend_multiplicities a [b] a_constr_name
				       in
					   update_classifiers_with_constraints classifiers a_type [a_constraint]
				       end
		val modified_classifiers = if multiplicities_of_aend b = []
					   then 
					       modified_tmp
					   else
					       let
						  val b_constraint = within_aend_multiplicities b [a] b_constr_name
					       in 
						   update_classifiers_with_constraints modified_tmp b_type [b_constraint]		   
					       end
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
 * For each association end, generate the matching-constraint and add it to the classifier.
 * 
 * {A,X_1,...,X_n} are the participants of an association, where A is the type the multiplicity is
 * handled for. There are always at least 2 elements.
 * The semantics of n-ary associations is, that if the X_i are fixed and A may vary, the size of the
 * resulting set is within the multiplicities specified. That set is the intersection of the As 
 * associated to each X_i.
 * Constraint:  X_1.role_A intersect X_2.role_A .... intersect X_n.role_A == set
 *              loweri <= set.size <= upperi, for all multiplicity pairs i of A
 * 
 * In OCL, this is:
 * context A inv:
 *   let set(x_1:X_1,...,x_n:X_n) : Set(A) = x_n.role_A->intersection(x_(n-1).role_A->intersection(...(x_1.role_A)...)) 
 *   let bounds(lower:Interger,upper:Integer,x_1:X_1,..,x_n:X_n): Boolean = 
 *                            set(x1,..,xn).size >= lower and set(x1,..,xn).size <= upper 
 *   in
 *   A.allInstances->forAll(a:A,x1:X_1,...,xn:X_n| bounds(a.lower_1,a.upper_1,..) or ... or bounds(a.lower_n,a.upper_n,...))
 * 
 * FIXME: Exact Let syntax? Currently no Let used.
 *)
fun generate_n_ary_constraint ((association as {name,aends=[],aclass},(all_classifiers,processed_assocs)) 
			       :(association * transform_model)): transform_model = 
    raise InvalidArguments "generate_n_ary_constraint: no aends\n" 
  | generate_n_ary_constraint ((association as {name,aends=[a],aclass},(all_classifiers,processed_assocs)) 
			       :(association * transform_model)): transform_model = 
    raise InvalidArguments "generate_n_ary_constraint: only 1 aend\n"
  | generate_n_ary_constraint ((association as {name,aends=[a,b],aclass},(all_classifiers,processed_assocs))
			     :(association * transform_model)): transform_model = 
    (all_classifiers,association::processed_assocs)
  | generate_n_ary_constraint ((association as {name,aends,aclass},(all_classifiers,processed_assocs))
			     :(association * transform_model)) :transform_model = 
    let
	val _ = trace function_calls "generate_n_ary_constraint\n"
	(** generate the constraint and update all classifiers *)
	fun n_ary_local_part classifiers a_part rest =
	    let
		val a_type =  type_of_aend a_part
		val assoc_name = string_of_path (association_of_aend a_part)
		val a_name = name_of_aend a_part
		val constr_name = "NAryToBinary"^assoc_name^a_name
	    in
		if multiplicities_of_aend a_part = []
		then
		    classifiers
		else
		    let
			val constraint = within_aend_multiplicities a_part rest constr_name
		    in
			update_classifiers_with_constraints classifiers a_type [constraint]
		    end
	    end

	(* iterate over the participants of the association *)
	fun process_assoc classifiers done [] = classifiers
	  | process_assoc classifiers done (x::xs) = 
	    let
		val rest = done@xs
		val modified_clsses = n_ary_local_part classifiers  x rest (* || rest || >= 2 *)
	    in
		process_assoc modified_clsses (x::done) xs
	    end

	val modified_classifiers = process_assoc all_classifiers [] aends  (* || aends || > 2 *)
    in
	(modified_classifiers, association::processed_assocs)
    end

(* traverse the set of associations, generate the necessary constraints and update the classifiers *)
(* accordingly *)
fun generate_n_ary_constraints ((classifiers,associations):transform_model):transform_model =
    (* using fold for commulative transformation *)
    foldl generate_n_ary_constraint (classifiers,[]) associations
 

fun split_n_ary_association (ac as {name,aends=[],aclass}:association):association list =
    raise InvalidArguments "split_n_ary_association: no aends\n"
  | split_n_ary_association (ac as {name,aends=[a],aclass}:association):association list =
    raise InvalidArguments "split_n_ary_association: only 1 aend\n"
  | split_n_ary_association (ac as {name,aends=[a,b],aclass}:association):association list =
    [ac]
  | split_n_ary_association (ac as {name,aends,aclass}:association) =
    (* We need to generate the pairs as well as the new names *)
    let
	val _ = trace function_calls "split_n_ary_associatio\n"
	val qualifier = get_qualifier name
	(* FIXME: update the name paths of all references to the new names *)
	fun to_association (a,b) = {name=qualifier@[name_of_aend a ^ (name_of_aend b)],
				    aends=[a,b],
				    aclass=aclass}
	(* FIXME: reflexiv parts? *)
	(* No dupplicates due to symmetry generated *)
	fun pair source targets = map (fn x => (source,x)) targets
	fun gen_pairs [] = error "in split_n_ary_association.gen_pairs: at least 2 elements needed, 0 provided"
	  | gen_pairs [x] = error "in split_n_ary_association.gen_pairs: at least 2 elements needed, 1 provided"
	  | gen_pairs [x,y] = [(x,y)]
				  (* pair src with all parts and continue *)
	  | gen_pairs (src::rest) = pair src rest @ (gen_pairs rest)
	val pairs = gen_pairs aends
	val binary_associations = map to_association pairs
    in
	binary_associations 
    end
    
fun split_n_ary_associations ((classifiers,associations):transform_model):transform_model =
    (trace function_calls "split_n_ary_associations\n";
    (classifiers, List.concat (map split_n_ary_association associations)))


(** 
 * We need to add OCL constraints to handle the broken relationship.
 * The problem is, that when splitting an n-ary association into it's
 * components, the matching of the now 'local' associations is lost. 
 * For instance, participants A and B are associated to participant C
 * with mulitplicity 2..3.                                           
 * 1. After the splitting, A may point to 2 instances, while B points
 *    to 3 instances.                                                
 * 2. Even if the cardinalities agree, A and B may point to different
 *    instances of C.
 * 3. The cardinalities are per association, not an absolute barrier. 
 *    This means A may be associated to 2..3 Cs several times, provided
 *    they form separate association instances.
 * Instead of having every participant re-check the same constraints,
 * each participant will only validate it's own multiplicity boundary.
 *
 * requires: n-ary associations
 * generates: constraints, binary associations
 * removes: n-ary associations
 *)
fun transform_n_ary_associations ((classifiers,associations):transform_model):transform_model =
    generate_n_ary_constraints (classifiers,associations) |>>   (* pack the association bindings into constraints and update the classifiers *)
    split_n_ary_associations                                    (* n-ary -> binary *)



(*******************************
 ******** Control part  ********
 *******************************)


(** 
 * Transformations on Classifiers and Associations
 *)
fun transformClassifiers_ext (model:transform_model):transform_model =
    transform_association_classes model |>>  (* split an association classe into a class and an association*)
(*    transform_qualifier |>>
    transform_aggregation |>>  *)  
    transform_n_ary_associations |>>         (* remove n-ary associations *)
    transform_multiplicities              (* remove multiplicities *)

fun transformClassifiers (model:transform_model):Rep.Classifier list =
    fst (transformClassifiers_ext model) (* return classifiers *)


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
