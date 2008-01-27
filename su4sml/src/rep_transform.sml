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
 *)

val transformClassifiersExt : Rep_Core.transform_model -> Rep.Model
val transformClassifiers     : Rep_Core.transform_model -> Rep.Classifier list
val transformFile            : string -> Rep.Model

(* transforms *)
val transformAssociationClasses: Rep_Core.transform_model -> 
		                 Rep_Core.transform_model  
val transformQualifiers : Rep_Core.transform_model -> Rep_Core.transform_model
val transformAggregation: Rep_Core.transform_model -> Rep_Core.transform_model
val transformNAryAssociations : Rep_Core.transform_model -> 
		Rep_Core.transform_model (* remove n-ary associations *)
val transformMultiplicities : Rep_Core.transform_model -> 
		Rep_Core.transform_model (* remove multiplicities *)

(* helper functions *)

val updateClassifiersWithConstraints: Rep_Core.Classifier list -> 
				      Rep_OclType.OclType -> 
				      Rep_Core.constraint  list -> 
				      Rep_Core.Classifier list
val get_association : Rep_Core.association list -> Rep_OclType.Path -> 
		Rep_Core.association
(* only one of the below will remain *)
val get_other_associationends: Rep_Core.association list -> Rep_OclType.Path -> Rep_OclType.OclType -> Rep_Core.associationend list
val get_other_associationends_alt : Rep_Core.association list -> Rep_OclType.OclType -> Rep_OclType.Path -> Rep_Core.associationend list
val get_associationends  : Rep_Core.association list -> Rep_OclType.Path -> Rep_Core.associationend list
val associationends_of   : Rep_Core.association -> Rep_Core.associationend list

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
fun transformQualifiers ((all_classifiers,all_associations):transform_model):
    transform_model = 
    (all_classifiers,all_associations) (*dummy*)

(** Remove aggregations
 * requires: aggregation
 * generates: constraint
 * removes: aggregation
 *)
fun transformAggregation (allClassifiers,allAssociations) = 
    (allClassifiers,allAssociations) (*dummy*)


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
     Class { name = name,
	     parent = parent,
	     attributes = attributes,
	     operations = operations,
	     associations = associations,
	     invariant = invariant,
	     stereotypes = stereotypes,
	     interfaces = interfaces,
	     thyname = thyname,
	     activity_graphs = activity_graphs}
        
(** 
 * Process an association: add the dummy class, generate the matching-
 * constraint and update the classifiers with that constraint.
 *)
fun generalTransfromNAryAssociation dummy (association as {name,aends,
                                                           qualifiers=[],
                                                           aclass=NONE},
			                                     (classifiers,processedAssocs)) =
    let
      val _ = trace function_calls "transformNAryAssociation\n"
      fun modifyClassifier ((assocs,classifier),classifiers) =
          let
            val ([cls],rem) = List.partition (fn x => name_of x = 
                                                      name_of classifier)
                                             classifiers
          in
            modifyAssociationsOfClassifier assocs [association] cls ::rem
          end

      fun consistency [] refer [] [] [] = []
        | consistency (source::xs) refer (selfAend::ys) (roles::zs) 
                      (refRoles::us) =
          consistencyOclConstraint source refer selfAend roles refRoles ::
          (consistency xs refer ys zs us)

      fun addOcl ((classifier,ocls), classifiers) =
          let
            val ([cls],rem) = List.partition (fn x => classifier = x) 
                                             classifiers
          in
            addInvariants ocls cls :: rem
          end

       (* extract participants/members and form associations *)
      val (assocMembers,rem) = matchClassifiersAtAend aends classifiers
      val (binaryAssocs,oppRefAends) = binaryAssociations dummy assocMembers 
                                                          aends
      val (clsses,roleNames, oppAends, splitAssocs) = splitNAryAssociation 
                                                          association
                                                          assocMembers
      val assocMemberPairs = ListPair.zip (map (fn x => [x]) binaryAssocs,
                                           assocMembers)
      val splitMemberPairs = ListPair.zip (splitAssocs,assocMembers)

      (* update association membership info in classifiers *)
      val modifiedClassifiers = foldl modifyClassifier classifiers 
                                      (assocMemberPairs @ splitMemberPairs)
      val dummy = modifyAssociationsOfClassifier binaryAssocs [] dummy

      (* generate and add OCL constraints *)
      val uniquenessOCL = uniquenessOclConstraint dummy binaryAssocs
      val selfAends = matchAendsAtClassifier oppRefAends 
                                             (ListPair.zip (clsses,roleNames))
      val refRoles = map (matchAends oppRefAends) oppAends
      val namedConsistencyOCLs = consistency clsses dummy selfAends oppAends 
                                             refRoles 
      val multiplicitiesOCL = 
          multiplicityOclConstraint dummy (map multiplicity_of_aend aends) 
                                    oppRefAends
      val dummy = addInvariants (uniquenessOCL::multiplicitiesOCL) dummy
      val modifiedClassifiers = foldl addOcl modifiedClassifiers 
                                      namedConsistencyOCLs   

      (* update references to removed associations *)
      (*val modifiedClassifiers = TODO *)
    in
      (dummy::modifiedClassifiers, binaryAssocs@(List.concat splitAssocs)@
                                   processedAssocs)
    end

(** 
 * Transform an AssociationClass into a Class and an Association
 * requires: AssociationClass
 * generates: Class, Association, constraint
 * removes: AssociationClass
 *)
fun transformAssociationClasses (allClassifiers,allAssociations) =
    let
      val _ = trace function_calls "transformAssociationClasses\n"
      fun transformAssociationClass ({name,aends,qualifiers=[],
                                      aclass=SOME aClass},
                                     (classifiers,procAssocs)) =
          let 
            val ([dummy],rem) = List.partition (fn x => name_of x = aClass) 
                                               classifiers
          in
            generalTransfromNAryAssociation dummy ({name=name,aends=aends,
                                                    qualifiers=[],aclass=NONE},
                                                   (rem,procAssocs)) 
          end         
          
      fun stripAcAssoc ({name,aends,qualifiers,aclass=SOME aClass},
                        classifiers) =
          let
            val ([ac],rem) = List.partition (fn x => name_of x = aClass) 
                                            classifiers
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
      fun transformNAryAssociation (association,(classifiers,procAssocs)) =
          generalTransfromNAryAssociation 
              (newDummyClass (package_of_association association))
              (association,(classifiers,procAssocs)) 

      val (nAryAssocs,rem) = List.partition isPureNAryAssoc allAssociations
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
            val lowTerm = ocl_geq aendCallSize (Literal(Int.toString low, 
                                                        Integer))
            val highTerm = ocl_leq aendCallSize (Literal(Int.toString high,
                                                         Integer))
          in
            ocl_or lowTerm highTerm
          end
          
      fun binaryConstraint sourceType targetType role multis name =
          let
            val selfVar = self sourceType
            val orTerms = map (withinBound selfVar targetType role) multis
            val term = ocl_and_all orTerms
          in
            (SOME name, term)
          end
          
      fun addMultiplicityConstraints (assoc as {name,aends=[a,b],qualifiers=[],
                                                aclass=NONE},
                                      localClassifiers) =
          let
	          val _ = trace function_calls "addMultiplicityConstraints\n"
	          val aType = type_of_aend a
	          val bType = type_of_aend b
            val aPath = path_of_aend a
            val bPath = path_of_aend b
	          val aName = name_of_aend a
	          val bName = name_of_aend b
	          val aConstrName = "BinaryMultiplicity"^aName
	          val bConstrName = "BinaryMultiplicity"^bName
	          val modifiedTmp = 
                (case (multiplicities_of_aend a) of 
                   []     => localClassifiers
                 | multis => 
                   let
		                 val aConstraint = binaryConstraint aType bType bPath 
                                                        multis aConstrName
		               in
		                 updateClassifiersWithConstraints localClassifiers aType 
                                                      [aConstraint]
		               end)
	          val modifiedClassifiers = 
                (case (multiplicities_of_aend b) of
                   []     => modifiedTmp
                 | multis =>
		               let
		                 val bConstraint = binaryConstraint bType aType 
                                                        aPath multis 
                                                        bConstrName
		               in 
		                 updateClassifiersWithConstraints modifiedTmp bType 
                                                      [bConstraint]
		               end)
	        in
	          modifiedClassifiers
	        end
              
      (* filter the valid associations *)
      val (binaryAssociations,rem) = List.partition isPureBinAssoc 
                                                    allAssociations
                                               
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
    fst (transformClassifiersExt model) (* return classifiers *)


(** 
 * read and transform an .xmi file.
 * @return a list of rep classifiers, or nil in case of problems
 *) 
fun transformFile f:transform_model = (info ("opening "^f);
                       (normalize_ext  o transformClassifiersExt o 
                        RepParser.transformXMI_ext o XmiParser.readFile) f)
(*    handle ex as (IllFormed msg) => raise ex *)

exception FileNotFound of string

fun printStackTrace e =
    let val ss = CompilerExt.exnHistory e
    in
      print_stderr ("uncaught exception " ^ (General.exnMessage e) ^ " at:\n");
      app (fn s => print_stderr ("\t" ^ s ^ "\n")) ss
    end



end
