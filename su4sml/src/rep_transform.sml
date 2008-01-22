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

datatype transformFlag = BinaryAssociationsOnly
type modelTransformation = Rep_Core.transform_model * transformFlag list
			   -> Rep_Core.transform_model * transformFlag list


(* (JD) maybe not all of the following functions need to be exported.
 * e.g., generate_pairs, ...
 *)

val transformClassifiers_ext : Rep_Core.transform_model -> Rep.Model
val transformClassifiers     : Rep_Core.transform_model -> Rep.Classifier list
val transformFile            : string -> Rep.Model

(* transforms *)
val transform_association_classes: Rep_Core.transform_model -> 
		Rep_Core.transform_model (* split an association classe into a class and an association*)
val transform_qualifiers : Rep_Core.transform_model -> Rep_Core.transform_model
val transform_aggregation: Rep_Core.transform_model -> Rep_Core.transform_model
val transform_n_ary_associations : Rep_Core.transform_model -> 
		Rep_Core.transform_model (* remove n-ary associations *)
val transform_multiplicities : Rep_Core.transform_model -> 
		Rep_Core.transform_model (* remove multiplicities *)

(* helper functions *)
(**
 * returns the path of an association end. The path of an association end
 * is <path_of_association>@[<name_of_aend>].
 * @params {aend}
 * @param aend association end
 * @return path of association end
 *)
val path_of_aend   : Rep_Core.associationend -> Rep_OclType.Path
(**
 * returns the type of the classifier this association end belongs to.
 * @params {aend}
 * @param aend association end
 * @return type of the classifier at the association end
 *)
val type_of_aend   : Rep_Core.associationend -> Rep_OclType.OclType
(**
 * returns the association this association end belongs to.
 * @params {aend}
 * @param aend association end
 * @return the path of the enclosing association
 *)
val association_of_aend : Rep_Core.associationend -> Rep_OclType.Path
(**
 * returns the name of the association end.  The name of the association
 * end is the last part of the association end's path.
 * @params {aend}
 * @param aend association end
 * @return name of the association end as string.
 *)
val name_of_aend   : Rep_Core.associationend -> string
(**
 * returns the list of specified multiplicities for this association end.
 * @params {aend}
 * @param aend association end
 * @return the list of multiplicities of this association end. If there are
 * no multiplicities, an empty list is returned.
 *)
val multiplicities_of_aend :  Rep_Core.associationend -> (int*int) list
(**
 * returns the qualifying part of the (fully) qualified name.
 * @params {qualifiedName}
 * @param qualifiedName path denoting a name
 * @return all but the last part of qualifiedName
 *)
val get_qualifier  : Rep_OclType.Path -> Rep_OclType.Path
(**
 * returns the last part of the (fully) qualified name.
 * @params {qualifiedName}
 * @param qualifiedName path denoting a name
 * @return the last part of qualifiedName
 *)
val get_short_name : Rep_OclType.Path -> string
(**
 * Remove all multiplicities from the association
 * @params {assoc}
 * @param assoc association
 * @return assoc with all multiplicities removed
 *)
val stripMultiplicities : Rep_Core.association -> Rep_Core.association
val generate_pairs : 'a list -> ('a * 'a) list (* including symmetry *)
val update_classifier_with_constraint : Rep_Core.constraint -> 
		Rep_Core.Classifier -> Rep_Core.Classifier
(* single: exactly 1 match *)
val update_classifiers_single : Rep_Core.Classifier list -> Rep_OclType.OclType -> (Rep_Core.Classifier -> Rep_Core.Classifier) -> Rep_Core.Classifier list
val updateClassifiersWithConstraints: Rep_Core.Classifier list -> 
				      Rep_OclType.OclType -> 
				      Rep_Core.constraint  list -> 
				      Rep_Core.Classifier list
val get_association : Rep_Core.association list -> Rep_OclType.Path -> 
		Rep_Core.association
val split_on_association: Rep_Core.association list -> Rep_OclType.Path -> 
		Rep_Core.association * Rep_Core.association list
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

datatype transformFlag = BinaryAssociationsOnly
type modelTransformation = Rep_Core.transform_model * transformFlag list
			   -> Rep_Core.transform_model * transformFlag list
open library
open Transform_Library
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
§ ***********************************)
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

fun package_of_association {name=package@[assoc],aends,aclass} =
    package

(* (JD) -> Rep_Core? *)	
fun multiplicities_of_aend (aend:associationend):(int*int)list =
    #multiplicity aend

(** chop-off the last part of the path *)	  
(* (JD) -> Rep_OclType? *)
fun get_qualifier (path:Path):Path =
    List.take (path,List.length path - 1)

fun short_name_of_aend {name,aend_type,...}:string =
    List.last name

(* (JD) -> Rep_OclType? *)
fun get_short_name (path:Path):string =
    List.last path

fun stripMultiplicities ({name,aends,aclass}:association):association =
    let
      fun handleAend {name,aend_type,multiplicity,visibility,
                      ordered,init} =
	  {name=name,
	   aend_type=aend_type,
	   multiplicity=[],
	   visibility=visibility,
	   ordered=ordered,
	   init=init}
    in
      {name   = name,
       aends  = map handleAend aends,
       aclass = aclass}
    end

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

fun update_classifier_with_constraint constraint (Class{name,parent,attributes,
    operations,associations,invariant,stereotypes,interfaces,thyname,
    activity_graphs}) =
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
  | update_classifier_with_constraint constraint (AssociationClass {name,parent,
    attributes,operations,associations,association,invariant,stereotypes,
    interfaces,thyname,activity_graphs}) =
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
  | update_classifier_with_constraint constraint (Interface {name,parents,
    operations,stereotypes,invariant,thyname}) =
    Interface {name=name,
	       parents=parents,
	       operations=operations,
	       stereotypes=stereotypes,
	       invariant=constraint::invariant,
	       thyname=thyname} 
  | update_classifier_with_constraint constraint (Enumeration {name,parent,
    operations,literals,invariant,stereotypes,interfaces,thyname}) = 
     Enumeration {name = name,
		  parent = parent,
		  operations = operations,
		  literals=literals,
		  invariant = constraint::invariant,
		  stereotypes = stereotypes,
		  interfaces = interfaces,
		  thyname = thyname}
  | update_classifier_with_constraint constraint (Primitive {name,parent,
    operations,associations,invariant,stereotypes,interfaces,thyname}) = 
    Primitive{name = name,
	      parent = parent,
	      operations = operations,
	      associations = associations,
	      invariant = constraint::invariant,
	      stereotypes = stereotypes,
	      interfaces = interfaces,
	      thyname = thyname} 
  | update_classifier_with_constraint constraint (Template {parameter,
							    classifier}) = 
    Template{parameter=parameter,
	     classifier = update_classifier_with_constraint 
			      constraint classifier} (* sensible? *)

fun update_classifiers_single (all_classifiers:Classifier list) 
    (classifier:OclType) (update:Classifier -> Classifier) :Classifier list= 
  let
    val (match,rest) = List.partition 
			   (fn (Class {name,...}) => name=classifier
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
							    
fun update_classifiers_with_constraints (all_classifiers:Classifier list) 
  (classifier:OclType) (con::constraints:constraint list) :Classifier list =
  let
    val modified_clsses = update_classifiers_single all_classifiers classifier
					(update_classifier_with_constraint con)
  in
      update_classifiers_with_constraints modified_clsses classifier 
					  constraints
  end
  | update_classifiers_with_constraints all_classifiers _ [] = all_classifiers
    
fun get_association (all_assocs: Rep_Core.association list) (assoc_path:Path):
    association =
  let
    val assoc = filter (fn {name,...}=> name=assoc_path) all_assocs
  in
    case assoc of 
	[x] => x
      | []  => error "in get_association: no match found"
      | _   => error "in get_association: more than 1 match found"
  end
  
fun get_other_associationends (all_assocs:association list) (assoc_path:Path)
  (cls_type:Rep_OclType.OclType):associationend list = 
  let
    fun all_others ({aend_type,...}:associationend) = 
	(collection_type_of_OclType aend_type) <> cls_type
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

fun withinAendMultiplicities targetAend sourceAends name =
  let
    val _ = trace function_calls "withinAendMultiplicities\n"
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
  in
    (SOME name,constr_complete)
  end
  
fun split_on_association (associations:association list) (path:Path): (association * association list) =
  let
    fun belonging_association tgt {name,aends,aclass} =  tgt = name
    val ([assoc],others) = List.partition (belonging_association path) 
                                          associations
  in
    (assoc, others)
  end


(****************************
 ******** Transforms ********
 ****************************)

(**
 * 
 * Remove qualifiers
 * requires: qualifier
 * generates: constraint, AssociationClass
 * removes: qualifier
 *)
fun transform_qualifiers ((all_classifiers,all_associations):transform_model):
    transform_model = 
    (all_classifiers,all_associations) (*dummy*)

(** Remove aggregations
 * requires: aggregation
 * generates: constraint
 * removes: aggregation
 *)
fun transform_aggregation ((all_classifiers,all_associations):transform_model):transform_model = 
    (all_classifiers,all_associations) (*dummy*)


(*****************************************************************************)
(*****************************************************************************)
(**** update *****************************************************************)
(*****************************************************************************)



(** 
 * Transform an AssociationClass into a Class
 * Strip the association class related information from the association class
 * and turn it into a regular class. Constraints, etc are handled elsewhere;
 * this is purely a conversion function. 
 *
 * requires: AssociationClass
 * generates: Class
 * removes: AssociationClass
 *)
fun transformAssociationClassIntoClass (AssociationClass 
                                            {name,parent,attributes,operations,
				             associations,association,
                                             invariant,stereotypes,interfaces,
                                             thyname,activity_graphs}) =
    (trace function_calls "transformAssociationClassIntoClass\n",
     Class { name = name,
	     parent = parent,
	     attributes = attributes,
	     operations = operations,
	     associations = associations,
	     invariant = constraint::invariant,
	     stereotypes = stereotypes,
	     interfaces = interfaces,
	     thyname = thyname,
	     activity_graphs = activity_graphs})

(** 
 * Transform an AssociationClass into a Class and an Association
 * requires: AssociationClass
 * generates: Class, Association, constraint
 * removes: AssociationClass
 *)
fun transformAssociationClasses (allClassifiers,allAssociations) =
    let
      val _ = trace function_calls "transformAssociationClasses\n"
      fun transformAssociationClass ({name,aends,aclass=SOME aClass},
                                     (classifiers,procAssocs)) =
          let 
            val ([dummy],rem) = List.partition (fn x => name_of x = aClass) 
                                               classifiers
          in
            generalTransfromNAryAssociation dummy ({name=name,aends=aends,aclass=NONE},
                                                   (rem,procAssocs)) 
          end         
          
      fun stripAcAssoc ({name,aends,aclass=SOME aClass},classifiers) =
          let
            val (ac,rem) = List.partition (fn x => name_of x = aClass) classifiers
          in
            transformAssociationClassIntoClass ac ::rem
          end

      val (acAssocs,rem) = List.partition isPureAcAssoc allAssociations
      val modifiedClassifiers = foldl stripAcAssoc allClassifiers acAssocs
      val (modifiedClassifiers,modifiedAssociations) = 
          foldl transformAssociationClass (modifiedClassifiers,[]) acAssocs
    in
      (modifiedClassifiers,modifiedAssociations@rem)
    end

fun transformNAryAssociation (association,(classifiers,procAssocs)) =
    generalTransfromNAryAssociation (newDummyClass o package_of_association
                                                         association)
                                    (association,(classifiers,procAssocs)) 
                                    
(** 
 * Process an association: add the dummy class, generate the matching-
 * constraint and update the classifiers with that constraint.
 *)
fun generalTransfromNAryAssociation dummy (association as {name,aends,NONE},
			                   (classifiers,processedAssocs)) =
    let
      val _ = trace function_calls "transformNAryAssociation\n"
      fun modifyClassifiers ((assoc,classifier),classifiers) =
          let
            val ([cls],rem) = partition (fn x => name_of x = name_of 
                                                                 classifier)
                                        classifiers
          in
            modifyAssociationsOfClassifier [assoc] [association] cls ::rem
          end

      fun consistency [] ref [] [] [] = []
        | consistency (source::xs) ref (selfAend::ys) (roles::zs) 
                      (refRoles::us) =
          consistencyOclConstraint source ref selfAend roles refRoles ::
          (consistency xs ref ys zs us)

      fun addOcl ((classifier,ocls), classifiers) =
          let
            val ([cls],rem) = partition (fn x => classifier = x) classifiers
          in
            addInvariants ocls cls :: rem
          end

      fun matchClassifiers oppRefAends (cls,role) =
          matchClassifier oppRefAends cls role
          
       (* extract participants/members and form associations *)
      val (assocMembers,rem) = matchClassifierAtAend classifiers aends
      val (binaryAssocs,oppRefAends) = binaryAssociations dummy assocMembers 
                                                          aends
      val (clsses,roleNames, oppAends, splitAssocs) = splitNAryAssociation 
                                                          association
                                                          assocMembers
      val assocMemberPairs = zip binaryAssocs assocMembers
      val splitMemberPairs = zip splitAssocs assocMembers

      (* update association membership info in classifiers *)
      val modifiedClassifiers = foldl modifyClassifiers classifiers 
                                      (assocMemberPairs@splitMemeberPairs)
      val dummy = modifyAssociationsOfClassifier binaryAssocs [] dummy

      (* generate and add OCL constraints *)
      val uniquenessOCL = uniquenessOclConstraint dummy binaryAssocs
      val selfAends = matchClassifiers oppRefAends (zip clsses roleNames)
      val refRoles = map (matchAends oppRefAends) oppAends
      val namedConsistencyOCLs = consistency clsses dummy selfAends oppAends 
                                             refRoles 
      val multiplicitiesOCL = 
          multiplicityOclConstraint dummy (map multiplicity_of_aend aends) 
                                    binaryAssocs
      val dummy = addInvariants (uniquenessOCL::multiplicitiesOCL) dummy
      val modifiedClassifiers = foldl addOcl modifiedClassifiers 
                                      namedConsistencyOCLs   

      (* update references to removed associations *)
      (*val modifiedClassifiers = TODO *)
    in
      (dummy::modifiedClassifiers, binaryAssocs@splitAssocs@processedAssocs)
    end

(** 
 * We need OCL constraints and an additional dummy class to handle the broken
 * relationship. The problem is fixing a particular association instance, as
 * OCL navigation doesn't allow fixing more than one variable of an association
 * tuple. Therefore, a dummy class is added to preserve association instances
 * and allow multiplicity restrictions.
 *
 * requires: "pure" n-ary associations, i.e. no association class.
 * generates: constraints, binary associations, dummy class
 * removes: n-ary associations
 *)
fun transformNAryAssociations (allClassifiers,allAssociations) =
    let
      val _ = trace function_calls "transform_n_ary_associations\n"
      val (nAryAssocs,rem) = partition isPureNAryAssoc allAssociations
      val (modifiedClassifiers,modifiedAssociations) =
          foldl transformNAryAssociation (allClassifiers,[]) nAryAssocs
    in
      (modifiedClassifiers,modifiedAssociations@rem)
    end


(** 
 * Move multiplicities from association ends to classifier constraints.
 * requires: "pure" binary associations, i.e. no qualifiers, aggregation, 
 *           association classes, etc.
 * generates: constraints
 * removes: binary association multiplicities
 *)

fun transformMultiplicities (allClassifiers,allAssociations) =
    let
      val _ = trace function_calls "transformMultiplicities\n"
      fun withinBound selfVar targetType role (low,high)=
          let
            val returnType = Bag targetType
            val aendCallSize = ocl_size (ocl_aendcall selfVar role returnType)
            val lowTerm = ocl_geq aendCallSize low
            val highTerm = ocl_leq aendCallSize high
          in
            ocl_or lowTerm highTerm
          end
          
      fun binaryConstraint sourceType targetType role multis name =
          let
            val selfVar = self sourceType
            val orTerms = map withinBound selfVar targetType role multis
            val term = ocl_and_all orTerms
          in
            (SOME name, term)
          end
          
      fun addMultiplicityConstraints (assoc as {name,aends=[a,b],NONE},
                                      localClassifiers) =
          let
	    val _ = trace function_calls "addMultiplicityConstraints\n"
	    val aType = type_of_aend a
	    val bType = type_of_aend b
            val aPath = path_of_aend a
            val bPath = path_of_aend b
	    val aName = name_of_aend a
	    val bName = name_of_aend b
	    val aConstrName = "BinaryMultiplicity"^a_name
	    val bConstrName = "BinaryMultiplicity"^b_name
	    val modifiedTmp = 
                (case (multiplicities_of_aend a) of 
                   []     => localClassifiers
                 | multis => 
                   let
		     val aConstraint = binaryConstraint aType bType bPath 
                                                        multis aConstrName
		   in
		     updateClassifiersWithConstraints classifiers aType 
                                                      [aConstraint]
		   end)
	    val modifiedClassifiers = 
                (case (multiplicities_of_aend b) of
                   []     => modifiedTmp
                 | multis =>
		   let
		     val bConstraint = binaryConstraint bType aType aPath
                                                        multis bConstrName
		   in 
		     updateClassifiersWithConstraints modifiedTmp bType 
                                                      [bConstraint]
		   end)
	  in
	    modifiedClassifiers
	  end
          
      (* filter the valid associations *)
      val (binaryAssociations,rem) = partition isPureBinAssoc allAssociations

      (* add the constraints to the classifiers *)
      val modifiedClassifiers = foldl addMultiplicityConstraints 
                                      allClassifiers binaryAssociations
      
      (* update the associationends *)
      val modifiedAssociations = map stripMultiplicities binaryAssociations
    in
      (modifiedClassifiers, modifiedAssociations@rem)
    end
        
(*******************************
 ******** Control part  ********
 *******************************)


(** 
 * Transformations on Classifiers and Associations
 *)
fun transformClassifiersExt (model:Rep_Core.transform_model):Rep_Core.transform_model =
  (* remove qualifiers *)
  transformQualifiers model |>>
  (* remove multiplicities *)
  transformMultiplicities |>>
  
  (** At this point, only n-ary associations without multiplicities and 
   * possibly with an associacation class are left. *)
  (* remove association classes *)
  transformAssociationClasses |>>
  (* remove n-ary associations *)  
  transformNAryAssociations

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
