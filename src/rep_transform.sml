(*****************************************************************************
 * su4sml --- an SML repository for managing (Secure)UML/OCL models
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
type modelTransformation = Rep.Model * transformFlag list
			   -> Rep.Model * transformFlag list


(* (JD) maybe not all of the following functions need to be exported.
 *)

val transformClassifiersExt  : Rep.Model -> Rep.Model
val transformClassifiers     : Rep.Model -> Rep.Classifier list
val transformFile            : string -> Rep.Model

(* transforms *)
val transformAssociationClasses : Rep.Model -> Rep.Model 
val transformAssociationClassesToNAryAssociations: Rep.Model -> Rep.Model 
val transformQualifiers         : Rep.Model -> Rep.Model
val transformAggregation        : Rep.Model -> Rep.Model
(* remove n-ary associations *)
val transformNAryAssociations   : Rep.Model ->  Rep.Model
val transformNAryAssociationsToAssociationClasses : Rep.Model -> Rep.Model
(* remove multiplicities *)
val transformMultiplicities     : Rep.Model -> Rep.Model

(* helper functions *)

val updateClassifiersWithConstraints: Rep.Classifier list -> 
				      Rep_OclType.OclType -> 
				      Rep.constraint  list -> 
				      Rep.Classifier list
val get_association : Rep.association list -> Rep_OclType.Path -> 
		      Rep.association
(* only one of the below will remain *)
val get_other_associationends: Rep.association list -> Rep_OclType.Path -> 
                               Rep_OclType.OclType -> Rep.associationend list
val get_associationends  : Rep.association list -> Rep_OclType.Path -> 
                           Rep.associationend list
val associationends_of   : Rep.association -> Rep.associationend list

exception NotYetImplemented of string
exception InvalidArguments of string

end

structure Rep_Transform:REP_TRANSFORM =
struct

datatype transformFlag = BinaryAssociationsOnly
type modelTransformation = Rep_Core.transform_model * transformFlag list
			   -> Rep_Core.transform_model * transformFlag list
open Rep_Helper
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
    (case assoc of 
       [x] => x
     | []  => Logger.error ("in get_association: no match found ("^(string_of_path (assoc_path))^")")
     | _   => Logger.error "in get_association: more than 1 match found")
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
	val _ = List.app (print o (fn x => x ^"\n") o name_of_aend) 
                         (#aends assoc)
    in
	#aends assoc
    end

(****************************
 ******** Transforms ********
 ****************************)


(** Remove aggregations
 * requires: aggregation
 * generates: constraint
 * removes: aggregation
 *)
fun transformAggregation (allClassifiers,allAssociations) = 
    (allClassifiers,allAssociations) (*dummy*)


(**
 * 
 * Remove qualifiers
 * requires: qualified binary associations
 * generates: constraints, binary associations
 * removes: qualifiers
 *)
fun transformQualifiers ((allClassifiers,allAssociations):transform_model):
    transform_model =
    let
      val _ = Logger.debug2 "transformQualifiers\n"
      (* connects the dummy class to the new qualifier classes *)
      fun handleQualifier assocPath (role,attributes) =
          let
            fun addAttrPair (cls,attr) = addAttribute cls attr
                                   
            val package = qualifier_of_path assocPath      
            val dummy = newDummyClass package
            val newClasses = map (newNamedClass package) 
                                 (map name_of_attribute attributes)
            val newClasses = map addAttrPair (ListPair.zip(newClasses,
                                                           attributes))
            val clsRolePairs = map (fn x => (x,NONE)) newClasses
            val (newBinaryAssocs,newOppAends) = binaryAssociations dummy NONE 
                                                                   clsRolePairs
          in
            (role,dummy, newClasses, newBinaryAssocs, newOppAends)
          end
              
      (* connects the original classifiers to the new dummy classes *)
      fun handleSources aends classifiers (role,dummy,newClasses,
                                           newBinaryAssocs,newOppAends) =
          let
            val [source] = List.filter (fn x => role_of_aend x = role) aends
            val [sourceClass] = List.filter (fn cls => type_of cls = 
                                                       type_of_aend source)
                                            classifiers
            val sourceRole = SOME role
            val [opp] = List.filter (fn x => role_of_aend x <> role) aends
            val dummyRole = SOME (role_of_aend opp)
            val ([dummyAssoc],[dummyAend]) = 
                binaryAssociations sourceClass sourceRole [(dummy,dummyRole)]
          in
            (dummyAssoc,dummyAend)
          end

      (* handled this way, in case of a binary to n-ary transition *)
      fun updateAend ((role,dummy,newClasses,newBinaryAssocs,newOppAends),
                      {name,aends,qualifiers,aclass}) =
          let
            fun modAend newType {name,aend_type,multiplicity,ordered,
                                 init,visibility} =
                {name=name,
                 aend_type=newType,
                 multiplicity=multiplicity,
                 ordered=ordered,
                 visibility=visibility,
                 init=init}
                
            val ([aend],rem) =
                List.partition (fn {name=aendName,aend_type,...} =>
                                   aendName = (name@[role])) aends
            val modifiedAends = modAend (type_of dummy) aend :: rem
          in
            {name=name,
             aends=modifiedAends,
             qualifiers=[],
             aclass=aclass}
          end
          
      fun addAssocs ((role,dummy,newClasses,newBinaryAssocs,newOppAends),
                     (collectedAssocs,collectedClassifiers))=
          let
            val modifiedDummy = modifyAssociationsOfClassifier newBinaryAssocs
                                                               []
                                                               dummy
            val modifiedNewClasses = 
                map (fn (x,y) => modifyAssociationsOfClassifier [y] [] x)
                                         (ListPair.zip(newClasses,
                                                       newBinaryAssocs))
          in
            (newBinaryAssocs@collectedAssocs,
             modifiedDummy::modifiedNewClasses@collectedClassifiers)
          end

      fun addTranslation (binaryAssoc,classifiers)=
          let
            val (matched,rem) = matchClassifiersAtAend (aends_of_association
                                                            binaryAssoc)
                                                       classifiers
            val matched= map (modifyAssociationsOfClassifier [binaryAssoc]
                                                             [])
                         matched
          in
            (matched@rem)
          end    
          
      fun removeQualifiers (assoc as {name=assocPath,aends,qualifiers,aclass}:
                            association,(classifiers,associations)):
          (Classifier list * association list) =
          let
            (** transform an qualifedAendCall into an aendCall: 
             * path and sourceType change (dummy1 -> dummy2/target)
             * source needs to be expressed in terms of the original, qualified
             * classifier. (source -> dummy1)
             * if the target is also changed, resultType changes and the entire
             * expression needs to be wrapped for the translation to the
             * original target. (dummy2 -> target)
             *)
            fun updateQualifier oldAssocPath newAssoc sourcePairs qualiTuple
                                qualifiers 
                                (QualifiedAssociationEndCall
                                     (source,sourceType,qualifierVals,
                                      path,resultType)) =
                let
                  fun modifySource sourcePairs source oppAends qualifierVals
                                   qualifiers role dummy =
                      let
                        fun restrict var (oppAend,(qualiVal,qualiType),quali) =
                          let
                            val aendCall = ocl_aendcall var (path_of_aend 
                                                                 oppAend)
                                                        (type_of_aend oppAend)
                            val qualiPath = path_of_OclType (type_of_aend 
                                                                 oppAend)@
                                            [name_of_attribute quali]
                            val attCall = ocl_attcall aendCall qualiPath
                                                      qualiType
                          in
                            ocl_eq attCall qualiVal
                          end

                        val [(_,qualis)] = List.filter (fn (name,_) => 
                                                           name=role)
                                                       qualifiers
                        val triples = zip3(oppAends,qualifierVals,qualis)
                        val sourceType = if is_Collection 
                                                (Rep_OclHelper.type_of source)
                                         then collection_type_of_OclType (
                                              Rep_OclHelper.type_of source)
                                         else Rep_OclHelper.type_of source
                        val [(_,sourceAend)] = 
                            List.filter (fn (_,aend) => type_of_aend aend
                                                        = sourceType) 
                                        sourcePairs
                        val translation = ocl_aendcall source (path_of_aend 
                                                                   sourceAend)
                                                       (Bag dummy)
                        val dummyVar = variableFromOclType dummy
                        val body = ocl_and_all (map (restrict dummyVar)triples)
                        val restriction = ocl_select translation dummyVar
                                                     body
                      in
                        restriction
                      end

                  fun modifySourceType role newAssoc sourceType =
                      let
                        val [aend] = List.filter (fn x => role_of_aend x<>role)
                                                 (aends_of_association 
                                                      newAssoc)
                        val newType = type_of_aend aend
                      in
                        if is_Collection sourceType then
                          (case sourceType of
                             (Set _) => Set newType
                           | (Sequence _) => Sequence newType
                           | (OrderedSet _) => OrderedSet newType
                           | (Bag _) => Bag newType
                           | (Collection _) => Collection newType)
                        else newType
                      end

                  fun modifyPath role newAssoc =
                      let
                        val [aend] = List.filter (fn x => role_of_aend x=role) 
                                                 (aends_of_association 
                                                      newAssoc)
                      in
                        path_of_aend aend
                      end

                  val role = short_name_of_path path
                  val [dummyAend] = List.filter (fn x => role_of_aend x <> 
                                                         role) 
                                                (aends_of_association 
                                                     newAssoc) 
                  val dummy = type_of_aend dummyAend
                  val [(_,_,_,_,oppAends)] = 
                      List.filter (fn (roleRef,_,_,_,_) => roleRef = role) 
                                  qualiTuple
                in
                  if qualifier_of_path path = oldAssocPath then
                    AssociationEndCall(modifySource sourcePairs source oppAends
                                                    qualifierVals qualifiers 
                                                    role dummy,
                                       modifySourceType role newAssoc 
                                                        sourceType,
                                       modifyPath role newAssoc,
                                       resultType)
                  else QualifiedAssociationEndCall(source,sourceType,
                                                   qualifierVals,
                                                   path,resultType)
                end
              | updateQualifier oldAssocPath newAssoc sourcePairs oppAends 
                                qualifiers x = x

            fun copyAssoc {name,aends,qualifiers,aclass} =
                let
                  fun updateAssocOfAend path {name,aend_type,multiplicity,
                                             init,ordered,visibility} =
                      {name=path@[short_name_of_path name],
                       aend_type=aend_type,
                       multiplicity=multiplicity,
                       init=init,
                       ordered=ordered,
                       visibility=visibility}

                  val newName = qualifier_of_path name@[short_name_of_path
                                                            name ^nextUid()]
                in
                  {name=newName,
                   aends=map (updateAssocOfAend newName) aends,
                   qualifiers=qualifiers,
                   aclass=aclass}
                end

            fun stripQualifier {name,aends,qualifiers,aclass} =
                  {name=name,
                   aends=aends,
                   qualifiers=[],
                   aclass=aclass}

            fun addUniqueness ((role,dummy, newClasses, newBinaryAssocs, 
                                newOppAends),
                               (dummyAssoc,dummyAend)) =
                (role,
                 addInvariants [uniquenessOclConstraint 
                                    dummy (dummyAssoc::newBinaryAssocs)] dummy,
                 newClasses, newBinaryAssocs,newOppAends)

            (* generate the new classes and assocs for possibly both
             * aend ends *)
            val qualiTuple = map (handleQualifier assocPath) qualifiers
            (* connect the source to the dummy *)
            val sourcePairs = map (handleSources aends classifiers) qualiTuple
            (* keep the original aend as side-link and remove the mults *)
            val copy = copyAssoc assoc
            val assoc = stripMultiplicities assoc
            val assoc = stripQualifier assoc
            (* update the copied aend to point to the new dummy class,
             * possibly at both ends *)
            val modifiedCopy = foldl updateAend copy qualiTuple
            (* add the uniqueness constraint *)
            val qualiTuple = map addUniqueness (ListPair.zip(qualiTuple,
                                                             sourcePairs)) 
            (* add the new assocs to the respective classifiers *)
            val (newAssocs, newClassifiers) = foldl addAssocs ([],[]) 
                                                    qualiTuple
            val (matched,rem) = matchClassifiersAtAend (aends_of_association 
                                                            modifiedCopy)
                                                       (newClassifiers@
                                                        classifiers)
            val allClassifiers = map (modifyAssociationsOfClassifier 
                                          [modifiedCopy] []) matched 
                                 @ rem
            val allClassifiers = foldl addTranslation allClassifiers
                                       (#1 (ListPair.unzip sourcePairs))  
            (* update all references to the original qualified pairs *)
            val newAssocs = newAssocs @ (#1 (ListPair.unzip sourcePairs))
            val modifiedClassifiers = 
                mapCalls (updateQualifier assocPath modifiedCopy sourcePairs
                                          qualiTuple qualifiers)
                         allClassifiers
(**         val modifiedClassifiers = updateQualifierReferences 
                                          (newClassifiers@classifiers)
                                          [(assoc,modifiedAssoc)]
            val modifiedClassifiers = updateAssociationReferences 
                                          modifiedClassifiers 
                                          [(assoc,newAssocs)]
*)          in
            (modifiedClassifiers, assoc::modifiedCopy::newAssocs@associations)
          end

      val (qualified, rem) = List.partition isPureQualifier allAssociations
      val (modifiedClassifiers, modifiedAssociations) =
          foldl removeQualifiers (allClassifiers,[]) qualified
    in
      (modifiedClassifiers, modifiedAssociations@rem)
    end

fun transformNAryAssociationsToAssociationClasses (allClassifiers,
                                                   allAssociations) =
    let
      val _ = Logger.debug2 "transformNAryAssociationsTo\
                                   \AssociationClasses\n"
      fun toAssocClass (assoc as {name,aends,qualifiers,aclass=NONE}) =
          let
            val newAC = newDummyAssociationClass (qualifier_of_path name)
            val newAC = setAssociationOfAssociationClass newAC name
          in
            (setAssociationOfAssociationClass newAC name,
             setAssociationClassOfAssociation assoc (name_of newAC))
          end

      val (nAry, rem) = List.partition isPureNAryAssoc allAssociations
      val (binary, rem) = List.partition isPureBinAssoc rem
      val (newClassifiers,modifiedAssocs) = ListPair.unzip(map toAssocClass 
                                                               (nAry@binary))
    in
      (newClassifiers@allClassifiers,modifiedAssocs@rem)
    end

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
                                             visibility,thyname,
                                             activity_graphs}) =
     Class { name = name,
	     parent = parent,
	     attributes = attributes,
	     operations = operations,
	     associations = associations,
	     invariant = invariant,
	     stereotypes = stereotypes,
	     interfaces = interfaces,
	     thyname = thyname,
	     visibility = visibility,
	     activity_graphs = activity_graphs}
        
fun transformAssociationClassesToNAryAssociations (allClassifiers,
                                                   allAssociations) =
    let
      val _ = Logger.debug2 "transformAssociationClassesTo\
                                   \NAryAssociations\n"
      fun morph {name,aends,qualifiers,aclass} class =
          let
            val newAend = {name=name@[StringHandling.uncapitalize 
                                          (short_name_of class)],
                           aend_type=type_of class,
                           multiplicity=[],
                           visibility= visibility_of class,
                           ordered=false,
                           init=NONE}
          in
            {name=name,
             aends=newAend::(map stripMultiplicityOfAend aends),
             qualifiers=qualifiers,
             aclass=NONE}
          end

      fun aCToNAry (newACAssoc,(localClassifiers,localAssociations)) =
          let
            val (AC,rem) = findClassifier localClassifiers 
                                          (associationClassOfAssociation 
                                               newACAssoc)
            val modifiedAC = transformAssociationClassIntoClass AC
            val modifiedAssoc = morph newACAssoc modifiedAC
            val aends = aends_of_association modifiedAssoc
            val multiplicityConstraints = 
                multiplicityOclConstraints modifiedAC 
                                           (map multiplicity_of_aend aends)
                                           aends
            val uniquenessConstraint =
                uniquenessOclConstraint modifiedAC
                                        [modifiedAssoc]
            val modifiedAC = addInvariants (uniquenessConstraint::
                                            multiplicityConstraints) modifiedAC
          in
            (modifiedAC::rem,modifiedAssoc::localAssociations)
          end

      val (aCs, rem) = List.partition isPureAcAssoc allAssociations
      val (modifiedClassifiers,modifiedAssociations) = 
          foldl aCToNAry (allClassifiers,[]) aCs
    in
      (modifiedClassifiers, modifiedAssociations@rem)
    end

(** 
 * Process an association: add the dummy class, generate the matching-
 * constraint and update the classifiers with that constraint.
 *)
fun generalTransfromNAryAssociation dummy (association as {name,aends,
                                                           qualifiers=[],
                                                           aclass=NONE},
			                   (classifiers,processedAssocs)) =
    let
      val _ = Logger.debug2 "generalTransformNAryAssociation\n"
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
            val ([cls],rem) = List.partition (fn cls => type_of cls =
                                                        type_of classifier) 
                                             classifiers
          in
            addInvariants ocls cls :: rem
          end

       (* extract participants/members and form associations *)
      val (assocMembers,rem) = matchClassifiersAtAend aends classifiers
      val (newBinaryAssocs,oppRefAends) = orderedBinaryAssociations dummy 
                                                                 assocMembers 
                                                                 aends
      val (clsses,roleNames, oppAends, splitAssocs, allNewSplitAssocs) = 
          splitNAryAssociation association assocMembers
      val assocMemberPairs = ListPair.zip (map (fn x => [x]) newBinaryAssocs,
                                           assocMembers)
      val splitMemberPairs = ListPair.zip (splitAssocs,assocMembers)

      (* update association membership info in classifiers *)
      val modifiedClassifiers = foldl modifyClassifier classifiers 
                                      (assocMemberPairs @ splitMemberPairs)
      val dummy = modifyAssociationsOfClassifier newBinaryAssocs [] dummy

      (* generate and add OCL constraints *)
      val uniquenessOCL = uniquenessOclConstraint dummy newBinaryAssocs
      val selfAends = matchAendsFromClassifier newBinaryAssocs dummy roleNames
      val refAends = map (matchAends oppRefAends) oppAends
      val namedConsistencyOCLs = consistency clsses dummy selfAends oppAends 
                                             refAends
      val multiplicitiesOCL = 
          multiplicityOclConstraints dummy (map multiplicity_of_aend aends) 
                                     oppRefAends
      val dummy = addInvariants (uniquenessOCL::multiplicitiesOCL) dummy
      val modifiedClassifiers = foldl addOcl modifiedClassifiers 
                                      namedConsistencyOCLs   

      (* update references to removed associations *)
      val modifiedClassifiers = updateAssociationReferences 
                                    modifiedClassifiers 
                                    [(association,
                                      newBinaryAssocs@allNewSplitAssocs)]
    in
      (dummy::modifiedClassifiers, newBinaryAssocs@allNewSplitAssocs@
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
      val _ = Logger.debug2 "transformAssociationClasses\n"
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
          
      fun stripAc ({name,aends,qualifiers,aclass=SOME aClass},
                        classifiers) =
          let
            val ([ac],rem) = List.partition (fn x => name_of x = aClass) 
                                            classifiers
          in
            transformAssociationClassIntoClass ac ::rem
          end

      val (acAssocs,rem) = List.partition isPureAcAssoc allAssociations
      val modifiedClassifiers = foldl stripAc allClassifiers acAssocs
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
      val _ = Logger.debug2 "transformNAryAssociations\n"
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
      val _ = Logger.debug2 "transformMultiplicities\n"
      fun withinBound selfVar targetType role (low,high)=
          let
            val returnType = Set targetType
            val aendCallSize = ocl_size (ocl_aendcall selfVar role returnType)
            val lowTerm = ocl_geq aendCallSize (Literal(Int.toString low, 
                                                        Integer))
            val highTerm = ocl_leq aendCallSize (Literal(Int.toString high,
                                                         Integer))
          in
            ocl_and lowTerm highTerm
          end
          
      fun binaryConstraint sourceType targetType role multis name =
          let
            val selfVar = self sourceType
            val orTerms = map (withinBound selfVar targetType role) multis
            val term = ocl_or_all orTerms
          in
            (SOME name, term)
          end
          
      fun addMultiplicityConstraints (assoc as {name,aends=[a,b],qualifiers=[],
                                                aclass=NONE},
                                      localClassifiers) =
          let
	          val _ = Logger.debug2 "addMultiplicityConstraints\n"
	          val aType = type_of_aend a
	          val bType = type_of_aend b
            val aPath = path_of_aend a
            val bPath = path_of_aend b
	          val aName = name_of_aend a
	          val bName = name_of_aend b
	          val aConstrName = "BinaryMultiplicity_"^bName (* opposite *)
	          val bConstrName = "BinaryMultiplicity_"^aName
	          val modifiedTmp = 
                (case (multiplicities_of_aend b) of (* opposite *)
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
                (case (multiplicities_of_aend a) of
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
  (* remove association classes *)
  transformAssociationClasses |>>
  (* remove n-ary associations *)  
  transformNAryAssociations |>>  
  (* remove multiplicities *)
  transformMultiplicities


fun transformClassifiers (model:transform_model):Rep.Classifier list =
    fst (transformClassifiersExt model) (* return classifiers *)


(** 
 * read and transform an .xmi file.
 * @return a list of rep classifiers, or nil in case of problems
 *) 
fun transformFile f:transform_model = (Logger.info ("opening "^f);
                       (normalize_ext  o transformClassifiersExt o 
                        RepParser.transformXMI_ext o XmiParser.readFile) f)
(*    handle ex as (IllFormed msg) => raise ex *)

exception FileNotFound of string

end
