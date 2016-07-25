signature TRANSFORM_LIBRARY =
sig

(**
 * Generate an OCL constraint guaranteeing that source is unique over the
 * supplied associations. In case of binary associations, the aend is checked
 * for a multiplicity of 1. Else this is enforced via OCL.
 * @params {source,associations}
 * @param source classifier that needs a uniqueness constraint
 * @param associations associations the uniqueness constraint should be 
 *                     defined over. An n-ary association are not a valid 
 *                     argument.
 * @return an OCL constraint expressing the uniqueness requirement
 *)
val uniquenessOclConstraint : Rep.Classifier -> Rep.association list
                              -> Rep.constraint

(**
 * @params {source,multis,aends}
 *)
val multiplicityOclConstraints: Rep.Classifier -> (int*int) list list ->
                                Rep.associationend list -> 
                                Rep.constraint list
val consistencyOclConstraint: Rep.Classifier -> Rep.Classifier -> 
                              Rep.associationend -> 
                              Rep.associationend list -> 
                              Rep.associationend list ->
                              (Rep.Classifier * Rep.constraint list)

(**
 * Works through the list of classifiers and updates uses of oldAssoc
 * to the appropriate association in newAssocs. This function only handles
 * the case where the classifiers aren't changed, i.e. source type, target type
 * and role remain the same. The qualifier transformation is not covered, for
 * instance.
 *
 * params {classifiers,[(oldAssoc,newAssocs)]}
 * @param classifiers
 * @param oldAssoc the association that has been removed from the model
 * @param newAssocs the associations that have replaced oldAssoc
 * @return the list of classifiers with references to the old association
 *         removed
 *)
val updateAssociationReferences: Rep.Classifier list ->
                                 (Rep.association * 
                                  Rep.association list) list ->
                                 Rep.Classifier list

(**
 * If newly created classifiers take the place of already existing classifiers,
 * references may need to be updated to
 *     a) pass-through the new classifier
 *     b) retain their meaning (i.e. qualifier split)
 * params {[association,[(Hike,D1,f1,f2)]],classifiers}
 * @param oldAssoc the original association with the original types
 * @param newAssoc the new association with the modified types but same
 *                 role names
 *)
val updateQualifierReferences: Rep.Classifier list -> 
                               (Rep.association * (Rep_OclType.OclType * 
                                                   Rep_OclType.OclType *
                                                   (Rep_OclType.OclType -> 
                                                    Rep_OclTerm.OclTerm list ->
                                                    Rep_OclType.OclType) *
                                                   (Rep_OclType.OclType -> 
                                                    Rep_OclType.OclType)
                                                  ) list) list ->
                               Rep.Classifier list
(**
 * @params {association,assocMembers}
 * @param association n-ary association that should be split into it's binary
 * links.
 * @param assocMembers defines the ordering of the returned values, namely it
 *                     defines the ordering of clsses.
 * @return a tuple containting a list classifiers, a list of role names, a list
 * of association ends and a list of binary associations: 
 * (clsses, roleNames, oppAends, splitAssocs).
 * Each "line" is associated with a classifier of clsses: roleName 1, oppAends
 * 1 and splitAssocs 1 belong to clsses 1.
 * Each element of the result lists:
 * clsses: Classifiers of the association
 * roleNames: unqualified role name that denoted the associated classifier in 
 *            the original association
 * oppAends: the opposite aends of the newly generated binary associations, 
 *           that the associated classifier from clsses is part of.
 * splitAssocs: ass oppAends, only the full binary association
 *)
val splitNAryAssociation: Rep.association -> Rep.Classifier list ->
                          (Rep.Classifier list * string list * 
                           Rep.associationend list list * 
                           Rep.association list list *
                           Rep.association list)

(**
 * Rearrange the oppRefAends to mirror the ordering of oppAends
 * @params {oppRefAends,oppAends}
 * @param oppRefAends
 * @param oppAends
 * @return the elements of oppRefAends in the same order as oppAends, given
 *         the classifier and the role name
 *)
val matchAends: Rep.associationend list -> Rep.associationend list -> 
                Rep.associationend list

(**
 * {association pool,source,roles}
 *)
val matchAendsFromClassifier: Rep.association list -> 
                              Rep.Classifier -> string list
                              -> Rep.associationend list

val matchClassifiersAtAend: Rep.associationend list ->
                            Rep.Classifier list ->
                            (Rep.Classifier list * 
                             Rep.Classifier list )

val findClassifier: Rep.Classifier list -> Rep_OclType.Path ->
                    (Rep.Classifier * Rep.Classifier list)

val mapCalls: (Rep_OclTerm.OclTerm -> Rep_OclTerm.OclTerm) ->
              Rep.Classifier list ->
              Rep.Classifier list

(**
 * @params source sourceRole (target,targetRole)
 *)
val binaryAssociations : Rep.Classifier -> string option ->
                         (Rep.Classifier * string option) list -> 
                         (Rep.association list * Rep.associationend list)
(**
 * Form binary associations between
 * @params {source,targets,aends}
 * @param source one of the ends for each of the binary associations
 * @param targets each target is the second aend in exactly one of the
 *                resulting associations
 * @param aends original associationends at the targets. The returned
 *              associationends. oppRefAends are in the same order as the 
 *              supplied aends and denote the new association ends
 * @return (binaryAssocs,oppRefAends)
 *)
val orderedBinaryAssociations: Rep.Classifier -> Rep.Classifier list -> 
                               Rep.associationend list -> 
                               (Rep.association list * Rep.associationend list)

val nextUid: unit -> string
(**
 * Helper function for generating new, unique classes within a given
 * package.
 *)
val newDummyClass: Rep_OclType.Path -> Rep.Classifier
val newDummyAssociationClass: Rep_OclType.Path -> Rep.Classifier
(**
 * Generate a new Class for the given package and having the given name.
 * @params {package, name}
 * @param package the packgae the new class should belong to
 * @param name the name of the new class. Will be suffixed to ensure 
 *             uniqueness
 * @return returns a new class for the given package having a unique name
 *                 starting with the given name
 *)
val newNamedClass: Rep_OclType.Path -> string -> Rep.Classifier

val fixAends: Rep_OclTerm.OclTerm -> Rep.associationend list 
              -> (Rep_OclTerm.OclTerm * Rep_OclTerm.OclTerm list)

(* Filters *)

val isPureNAryAssoc   : Rep.association -> bool
(**
 * For filtering pure qualified associations. At the moment, only binary
 * associations are supported.
 * @params {association}
 * @param association test association for being purely qualified, meaning
 *        no other adornments, such as aggregation, partitioning, etc
 * @return true iff the association is purely qualified
 *)
val isPureQualifier   : Rep.association -> bool
val isPureAcAssoc     : Rep.association -> bool
(**
 * returns true iif assoc is purely a binary association, without any 
 * additional adornments, such as aggregation, qualifier, association class, 
 * etc.
 * @params {assoc}
 * @param assoc association to be tested
 * @return true iif assoc is a pure binary association
 *)
val isPureBinAssoc    : Rep.association -> bool

val multiplicities_of_aend :  Rep.associationend -> (int*int) list

val stripMultiplicityOfAend: Rep.associationend -> Rep.associationend
(**
 * Remove all multiplicities from the association
 * @params {assoc}
 * @param assoc association
 * @return assoc with all multiplicities removed
 *)
val stripMultiplicities : Rep.association -> Rep.association


val modifyAssociationsOfClassifier: Rep_Core.association list -> 
                                    Rep_Core.association list ->
                                    Rep_Core.Classifier -> Rep_Core.Classifier


val updateClassifiersWithConstraints: Rep_Core.Classifier list -> 
				      Rep_OclType.OclType -> 
				      Rep_Core.constraint  list -> 
				      Rep_Core.Classifier list



val uid: int ref
val setAssociationOfAssociationClass: Rep.Classifier -> Rep_OclType.Path ->
                                      Rep.Classifier
val setAssociationClassOfAssociation: Rep.association -> Rep_OclType.Path ->
                                      Rep.association
val aend_of_association: Rep.association -> Rep_OclType.Path ->
                         Rep.associationend
val qualifier_of_path :Rep_OclType.Path -> Rep_OclType.Path
val name_of_attribute : Rep.attribute -> string
val addAttribute : Rep.Classifier -> Rep.attribute -> Rep.Classifier
val addAttributes : Rep.Classifier -> Rep.attribute list -> Rep.Classifier
val multiplicity_of_aend: Rep.associationend -> (int * int) list
val associationClassOfAssociation: Rep.association -> Rep_OclType.Path
val package_of_aend: Rep.associationend -> Rep_OclType.Path
val package_of_association: Rep.association -> Rep_OclType.Path
val variableFromOclType: Rep_OclType.OclType -> Rep_OclTerm.OclTerm
val variableFromAend: Rep.associationend -> Rep_OclTerm.OclTerm
val variableFromClassifier: Rep.Classifier -> Rep_OclTerm.OclTerm
val quantifyForAll: Rep_OclTerm.OclTerm list -> Rep_OclTerm.OclTerm ->
                    Rep_OclTerm.OclTerm

val zip3 : 'a list * 'b list * 'c list -> ('a * 'b * 'c) list

exception InvalidArguments of string
end


structure Transform_Library:TRANSFORM_LIBRARY =
struct

open Rep_Helper
open StringHandling
open Rep_OclTerm
open Rep_OclHelper
open Rep_Core

exception InvalidArguments of string

val uid = ref 0

fun nextUid () =  (uid := !uid + 1; "_S"^(Int.toString (!uid)))

fun zip3(a::ass,b::bs,c::cs) = (a,b,c)::zip3(ass,bs,cs)

fun get_short_name (path:Path):string =
    List.last path

fun stripMultiplicityOfAend {name,aend_type,multiplicity,visibility,
                              ordered,init} =
    {name=name,
	   aend_type=aend_type,
	   multiplicity=[],
	   visibility=visibility,
	   ordered=ordered,
	   init=init}

fun stripMultiplicities ({name,aends,qualifiers,aclass}:association):
    association =
    {name   = name,
     aends  = map stripMultiplicityOfAend aends,
     qualifiers = qualifiers,
     aclass = aclass}


fun multiplicity_of_aend ({aend_type,multiplicity,...}:associationend) = 
    multiplicity

fun associationClassOfAssociation {name,aends,qualifiers,aclass=SOME path} =
    path

fun aend_of_association {name,aends,qualifiers,aclass} path =
    let 
      val [aend] = List.filter (fn ({name,aend_type,...}:associationend) => 
                                   name=path) aends
    in
      aend
    end

fun package_of_aend ({name,aend_type,...}:associationend) =
    List.take(name, List.length name - 2)

fun package_of_association ({name,aends,qualifiers,aclass}:association) =
    List.take(name, List.length name - 1)

fun qualifier_of_path path = List.take(path, List.length path - 1)

(* (JD) -> Rep_Core? *)	
fun multiplicities_of_aend ({aend_type,multiplicity,...}:associationend) = 
    multiplicity

fun short_name_of_aend ({name,aend_type,...}:associationend) = 
    short_name_of_path name

fun name_of_attribute ({name,...}:attribute) = name

fun addAttributes (Class {name,parent,attributes,operations,associations,
                        invariant,stereotypes,interfaces,thyname,
                        activity_graphs,visibility}) newAttributes =
    Class {name=name,
           parent=parent,
           attributes=newAttributes@attributes,
           operations=operations,
           associations=associations,
           invariant=invariant,
           visibility=visibility,
           stereotypes=stereotypes,
           interfaces=interfaces,
           thyname=thyname,
           activity_graphs=activity_graphs}
  | addAttributes (AssociationClass {name,parent,attributes,operations,
                                   associations,association,invariant,
                                   stereotypes,interfaces,thyname,
                                   visibility,activity_graphs}) newAttributes =
    AssociationClass {name=name,
                      parent=parent,
                      attributes=newAttributes@attributes,
                      operations=operations,
                      associations=associations,
                      association=association,
                      invariant=invariant,
                      stereotypes=stereotypes,
                      interfaces=interfaces,
                      thyname=thyname,
		      visibility=visibility,
                      activity_graphs=activity_graphs}
  | addAttributes (Template {parameter,classifier}) newAttributes= 
    Template {parameter=parameter,
              classifier=addAttributes classifier newAttributes}
    
fun addAttribute classifier attribute = addAttributes classifier [attribute]

fun quantifyForAll variables body =
    let
      fun quantify (variable as Variable(_,varType),body) =
          ocl_forAll (ocl_allInstances (Literal (List.last (path_of_OclType 
                                                                varType),
                                                 varType))) 
                     [variable] body
    in
      (* right most variable at the inner most position *)
      foldr quantify body variables
    end

fun findClassifier allClassifiers path =
    let
      val ([match],rem ) = List.partition (fn x => name_of x = path) 
                                          allClassifiers
    in
      (match,rem)
    end

fun isPureBinAssoc {name,aends=[a,b],qualifiers=[],aclass=NONE} = true
  | isPureBinAssoc _ = false

fun isPureNAryAssoc {name,aends,qualifiers=[],aclass=NONE} =
    List.length aends > 2 
  | isPureNAryAssoc _ = false

fun isPureQualifier {name,aends=[a,b],qualifiers,aclass=NONE} = 
    List.length qualifiers > 0
  | isPureQualifier _ = false
                        
fun isPureAcAssoc {name,aends,qualifiers=[],aclass=SOME ac} = 
    List.length aends > 1
  | isPureAcAssoc _ = false

fun newDummyClass package =
    Class{name=Classifier (package@["Dummy"^ nextUid ()]),
          parent=NONE,
          attributes=[],
          operations=[],
          associations=[],
          invariant=[],
          stereotypes=[],
          visibility=XMI.public (* FIXME: private? *),
           interfaces=[],
          thyname=NONE,
          activity_graphs=[]}

fun newDummyAssociationClass package =
    AssociationClass{name=Classifier (package@["DummyAC"^ nextUid ()]),
                     parent=NONE,
                     attributes=[],
                     operations=[],
                     associations=[],
                     association=["dummyPath"],
                     invariant=[],
                     stereotypes=[],
                     visibility=XMI.public (* FIXME: private? *),
                     interfaces=[],
                     thyname=NONE,
                     activity_graphs=[]}

fun newNamedClass package name =
    Class{name=Classifier (package@[name^ nextUid ()]),
          parent=NONE,
          attributes=[],
          operations=[],
          associations=[],
          visibility=XMI.public (* FIXME: private? *),
          invariant=[],
          stereotypes=[],
          interfaces=[],
          thyname=NONE,
          activity_graphs=[]}

fun addAssociations (newAssocs:Path list) (associations:Path list) =
    let
      fun replaceAssoc (newAssoc:Path,oldAssociations:Path list) =
          newAssoc ::(List.filter (fn x => x <> newAssoc) oldAssociations)
    in 
      foldl replaceAssoc associations newAssocs
    end
    
fun removeAssociations oldAssocs associations =
    let
      fun removeAssoc (oldAssoc,oldAssociations) =
          List.filter (fn x => x <> oldAssoc) oldAssociations
    in 
      foldl removeAssoc associations oldAssocs
    end
    
fun setAssociationOfAssociationClass (AssociationClass 
                                          {name,parent,attributes,operations,
				                                   associations,association,
                                           invariant,stereotypes,interfaces,
                                           visibility,thyname,
                                           activity_graphs}) 
                                     associationPath =
    AssociationClass 
        {name=name,
         parent=parent,
         attributes=attributes,
         operations=operations,
				 associations=associations,
         association=associationPath,
         invariant=invariant,
         stereotypes=stereotypes,
         interfaces=interfaces,
         visibility=visibility,
         thyname=thyname,
         activity_graphs=activity_graphs}

fun setAssociationClassOfAssociation {name,aends,qualifiers,aclass}
                                     associationClassPath =
    {name=name,
     aends=aends,
     qualifiers=qualifiers,
     aclass=SOME associationClassPath}

fun updateQualifierReferences classifiers [] = classifiers
  | updateQualifierReferences classifiers ((association,updates)::xs) =
    let
      val modifiedClassifiers = classifiers (* FIXME *)
    in
      updateQualifierReferences modifiedClassifiers xs
    end


(** aendCall, attrCall, qualiCall *)
fun mapCalls f [] = []
  | mapCalls f (classifier::classifiers) =
    let
      fun handleConstraint f (name,term) = (name,mapOclCalls f term)

      fun modifyClassifier f (Class{name,parent,attributes,operations,
                                    associations,invariant,stereotypes,
                                    interfaces,thyname,visibility,
                                    activity_graphs}) =
          Class{name=name,
                parent=parent,
                attributes=attributes,
                operations=operations,
		            visibility=visibility,
                associations= associations,
                invariant=map (handleConstraint f) invariant,
                stereotypes=stereotypes,
                interfaces=interfaces,
                thyname=thyname,
                activity_graphs=activity_graphs}
        | modifyClassifier f (AssociationClass{name,parent,attributes,
                                               operations,associations,
                                               invariant,association,
                                               stereotypes,interfaces,
                                               visibility,thyname,
                                               activity_graphs}) =
          AssociationClass{name=name,
                           parent=parent,
                           attributes=attributes,
                           operations=operations,
			   visibility=visibility,
                           associations= associations,
                           association=association,
                           invariant=map (handleConstraint f) invariant,
                           stereotypes=stereotypes,
                           interfaces=interfaces,
                           thyname=thyname,
                           activity_graphs=activity_graphs}
        | modifyClassifier f (Template{parameter,classifier})=
          Template{parameter=parameter,
                   classifier=modifyClassifier f classifier}
                                               
    in
      modifyClassifier f classifier :: (mapCalls f classifiers)
    end


(* FIXME: CollectionParts? *)
fun updateAssociationReferences classifiers [] = classifiers
  | updateAssociationReferences classifiers updates =
    let
      val _ = Logger.debug2 "updateAssociationReferences\n"
          
      fun findNewPath oldAssoc newAssocs source path =
          let
            fun selectAend source role ({name,aends=[a,b],qualifiers,aclass},
                                        selected) =
                if (type_of_aend a = source) andalso role_of_aend b = role
                then b::selected
                else if (type_of_aend b = source) andalso role_of_aend a = role
                then a::selected
                else selected

            val role = short_name_of_path path
            val sourceType = if is_Collection source 
                             then collection_type_of_OclType source
                             else source
            val {name=newPath,aend_type,...} = hd(foldl(selectAend 
                                                            sourceType role) 
                                                       [] newAssocs)
          in
            newPath
          end

      fun traverseOcl oldAssoc newAssocs (If(cond,condType,thenn,thennType,
                                             elsee,elseeType,resultType))=
          If(traverseOcl oldAssoc newAssocs cond,condType,
             traverseOcl oldAssoc newAssocs thenn,thennType,
             traverseOcl oldAssoc newAssocs elsee,elseeType,
             resultType)
        | traverseOcl (oldAssoc as{name,aends,qualifiers,aclass}) newAssocs
                      (QualifiedAssociationEndCall (source,sourceType,
                                                    qualifierVals,path,
                                                    resultType)) =
          (* qualifiers are illegal on n-ary associations *)
           QualifiedAssociationEndCall(traverseOcl oldAssoc newAssocs source,
                                      sourceType,qualifierVals,path,
                                      resultType)
        | traverseOcl (oldAssoc as{name,aends,qualifiers,aclass}) newAssocs
                      (AssociationEndCall(source,sourceType,path,
                                          resultType)) =
          let
            (* match path and resultType to the correct newAssocs *)
            val newPath = if qualifier_of_path path <> name then path
                          else findNewPath oldAssoc newAssocs sourceType path
          in
            AssociationEndCall(traverseOcl oldAssoc newAssocs source,
                               sourceType,newPath,resultType)
          end
        | traverseOcl oldAssoc newAssocs (AttributeCall(source,sourceType,
                                         path,resultType)) =
          AttributeCall(traverseOcl oldAssoc newAssocs source,sourceType,
                        path,resultType)
        | traverseOcl oldAssoc newAssocs (OperationCall(source,sourceType,
                                                        path,parameters,
                                                        resultType)) =
          let
            fun handleParameters (term,termType) = 
                (traverseOcl oldAssoc newAssocs term,termType)
          in
            OperationCall(traverseOcl oldAssoc newAssocs source,sourceType,
                          path,map handleParameters parameters,resultType)
          end
        | traverseOcl oldAssoc newAssocs (OperationWithType(source,sourceType,
                                                            var,varType,
                                                            resulType)) =
          OperationWithType(traverseOcl oldAssoc newAssocs source,sourceType,
                            var,varType,resulType)
        | traverseOcl oldAssoc newAssocs (Let(name,nameType,rhs,rhsType,body,
                                              bodyType)) =
          Let(name,nameType,
              traverseOcl oldAssoc newAssocs rhs,rhsType,
              traverseOcl oldAssoc newAssocs body,bodyType)
        | traverseOcl oldAssoc newAssocs (Iterate (vars,name,nameType,nameTerm,
                                                   source,sourceType,body,
                                                   bodyType,resultType)) =
          Iterate (vars,
                   name,nameType,traverseOcl oldAssoc newAssocs nameTerm,
                   traverseOcl oldAssoc newAssocs source,sourceType,
                   traverseOcl oldAssoc newAssocs body,bodyType,
                   resultType)
        | traverseOcl oldAssoc newAssocs (Iterator (name,vars,source,
                                                    sourceType,body,bodyType,
                                                    resultType)) =
          Iterator (name,vars,
                    traverseOcl oldAssoc newAssocs source, sourceType,
                    traverseOcl oldAssoc newAssocs body,bodyType,
                    resultType)
        | traverseOcl oldAssoc newAssocs x = x
      
      fun handleConstraint oldAssoc newAssocs (name,term) =
          let
            val _ = Logger.debug2 "handleConstraint\n"
          in
            (name,traverseOcl oldAssoc newAssocs term)
          end

      fun modifyClassifier oldAssoc newAssocs (Class{name,parent,attributes,
                                                     operations,associations,
                                                     invariant,stereotypes,
                                                     interfaces,thyname,
                                                     visibility,
                                                     activity_graphs}) =
          Class{name=name,
                parent=parent,
                attributes=attributes,
                operations=operations,
		            visibility=visibility,
                associations= associations,
                invariant=map (handleConstraint oldAssoc newAssocs) invariant,
                stereotypes=stereotypes,
                interfaces=interfaces,
                thyname=thyname,
                activity_graphs=activity_graphs}
        | modifyClassifier oldAssoc newAssocs (AssociationClass
                                                   {name,parent,attributes,
                                                    operations,associations,
                                                    invariant,association,
                                                    stereotypes,interfaces,
                                                    visibility,thyname,
                                                    activity_graphs}) =
          AssociationClass{name=name,
                           parent=parent,
                           attributes=attributes,
                           operations=operations,
			   visibility=visibility,
                           associations= associations,
                           association=association,
                           invariant=map (handleConstraint oldAssoc newAssocs) 
                                         invariant,
                           stereotypes=stereotypes,
                           interfaces=interfaces,
                           thyname=thyname,
                           activity_graphs=activity_graphs}
        | modifyClassifier oldAssoc newAssocs (Template{parameter,classifier})=
          Template{parameter=parameter,
                   classifier=modifyClassifier oldAssoc newAssocs classifier}
                                               
      fun updateReferences ((oldAssoc,newAssocs),tmpClassifiers) =
          let
            val _  = Logger.debug2 "updateReferences\n"
          in
            map (modifyClassifier oldAssoc newAssocs) tmpClassifiers
          end
    in
      foldl updateReferences classifiers updates
    end

fun updateClassifiersWithConstraints classifiers oclType constraints = 
    let
      val (match,rem) = List.partition (fn cls => type_of cls = oclType) 
                                       classifiers
    in
      map (addInvariants constraints) match @ rem
    end

fun modifyAssociationsOfClassifier (newAssociations:association list) 
                                   (oldAssociations:association list)
                                   (Class{name,parent,attributes,
                                          operations,associations,invariant,
                                          stereotypes,interfaces,thyname,
                                          visibility,activity_graphs}) =
    Class{name=name,
          parent=parent,
          attributes=attributes,
          operations=operations,
          associations= addAssociations 
                        (map path_of_association newAssociations) 
                        (removeAssociations 
                             (map path_of_association oldAssociations)
                             associations),
          invariant=invariant,
          stereotypes=stereotypes,
		     visibility=visibility,
          interfaces=interfaces,
          thyname=thyname,
          activity_graphs=activity_graphs}
  | modifyAssociationsOfClassifier newAssociations oldAssociations 
                                   (AssociationClass{name,parent,attributes,
                                                     operations,associations,
                                                     invariant,association,
                                                     stereotypes,interfaces,
                                                     visibility,thyname,activity_graphs}) =
    AssociationClass{name=name,
                     parent=parent,
                     attributes=attributes,
                     operations=operations,
                     associations= addAssociations 
                                   (map path_of_association newAssociations) 
                                   (removeAssociations 
                                        (map path_of_association 
                                             oldAssociations) associations),
                     association=association,
                     invariant=invariant,
                     stereotypes=stereotypes,
                     interfaces=interfaces,
                     thyname=thyname,
		     visibility=visibility,
                     activity_graphs=activity_graphs}
    
  | modifyAssociationsOfClassifier newAssociations oldAssociations 
                                   (Primitive{name,parent,operations,
                                              associations,invariant,
                                              stereotypes,interfaces,
                                              thyname}) =
    Primitive {name=name,
	             parent=parent,
	             operations=operations,
	             associations= addAssociations 
                             (map path_of_association newAssociations) 
                             (removeAssociations 
                                  (map path_of_association oldAssociations)
                                  associations),
	             invariant=invariant,
	             stereotypes=stereotypes,
	             interfaces=interfaces,
	             thyname=thyname}
    	
fun uniquenessOclConstraint (source:Classifier)
                            (associations:association list) =
    let
      val _ = Logger.debug2 "uniquenessOclConstraint\n"
      fun assocAendCalls (self:OclTerm) (iter:OclTerm) {name,aends=[a,b],
                                                        qualifiers,
                                                        aclass} = 
          let
            (* FIXME: reflexiv *)
            val [{name,aend_type,multiplicity,...}] = 
                filter (fn {aend_type,name,multiplicity,ordered,visibility,
                            init} => Rep_OclHelper.type_of self <> aend_type)
                       [a,b]
            val selfCall = ocl_aendcall self name (Collection aend_type)
            val iterCall = ocl_aendcall iter name (Collection aend_type)
            val sizeCall = ocl_eq (ocl_size selfCall) 
                                  (Literal("1",Rep_OclType.Integer))
          in
            if multiplicity = [(1,1)] orelse multiplicity = [(0,1)] then ocl_eq selfCall iterCall
            else ocl_and (ocl_eq selfCall iterCall)
                         sizeCall
          end
        | assocAendCalls (self:OclTerm) (iter:OclTerm) {name,aends,qualifiers,
                                                        aclass} = 
          (* n-ary case *)
          let
            fun match self iter (role,roleType) =
                let
                  val selfCall = ocl_aendcall self role (Collection roleType)
                  val iterCall = ocl_aendcall iter role (Collection roleType)
                  val sizeCall = ocl_eq (ocl_size selfCall) 
                                        (Literal("1",Rep_OclType.Integer))
                in
                  [ocl_eq selfCall iterCall,sizeCall]
                end
                
            (* FIXME: reflexive? *)
            val ([selfAend],others) = 
                List.partition (fn x => Rep_OclHelper.type_of self =
                                        type_of_aend x)
                               aends
            val pairs = map (fn x => (path_of_aend x,
                                      type_of_aend x)) others
            val parts = List.concat (map (match self iter) pairs) 
          in
            ocl_and_all parts
          end
          
      val selfVar = self (type_of source)
      val iterVar = Variable ("other"^nextUid (),type_of source)
      val aendCalls = map (assocAendCalls selfVar iterVar) associations
      val oclBody = ocl_implies (ocl_and_all aendCalls) (ocl_eq selfVar
                                                                iterVar)
      val constr = quantifyForAll [iterVar] oclBody
    in
      (SOME "Uniqueness", constr)
    end

fun binaryAssociations (source:Classifier) (sourceRole:string option)
                       (targetRolePairs:(Classifier*string option) list):
    (association list * associationend list)=
    let
      val _ = Logger.debug2 "binaryAssociations\n"
      fun generateAssociation srcRole (target,roleOpt):
          (association * associationend)=
          let
            val role = if isSome roleOpt then valOf roleOpt
                       else uncapitalize (short_name_of target)
            val assocName =  package_of source @
                             ["BinaryAssoc"^nextUid ()]
            val oppAend = {name=assocName@[role],
                           aend_type=type_of target,
                           multiplicity=[(1,1)],
                           ordered=false,
                           visibility=XMI_DataTypes.public,
                           init=NONE}:associationend
          in
            ({name= assocName,
              aends=[{name=assocName@ [srcRole],
                      aend_type=type_of source,
                      multiplicity=[],
                      ordered=false,
                      visibility=XMI_DataTypes.public,
                      init=NONE},
                     oppAend],
              qualifiers=[],
              aclass=NONE},
             oppAend)
          end
          
      val srcRole = if isSome sourceRole then valOf sourceRole
                    else uncapitalize (short_name_of source)
      val pairs = map (generateAssociation srcRole) targetRolePairs
    in
      ListPair.unzip pairs
    end

fun orderedBinaryAssociations (source:Classifier) (targets:Classifier list) 
                              aends: (association list * associationend list)=
    let
      val _ = Logger.debug2 "orderedBinaryAssociations\n"

      fun order [] [] = []
        | order [] (x::xs) = 
          raise InvalidArguments ("binaryAssociations.order:"^
                                  "arguments don't agree\n")
        | order (x::xs) [] = 
          raise InvalidArguments ("binaryAssociations.order:"^
                                  "arguments don't agree\n")
        | order pairs (({name=refName,aend_type,multiplicity,
                        ordered,init,visibility}:associationend)::aends) =
          let
            val ([oppAend],rem) = 
                List.partition (fn (_,aend) => role_of_aend aend =
                                               short_name_of_path refName)
                               pairs
          in
            (oppAend :: (order rem aends))
          end
          
      (* TODO: role names *)
      val pairList = binaryAssociations source NONE 
                                        (ListPair.zip(targets,
                                                      (map(SOME o role_of_aend)
                                                          aends)))
    in
      ListPair.unzip (order (ListPair.zip pairList) aends)
    end

fun variableFromOclType oclType = 
    Variable (short_name_of_OclType oclType^nextUid (),oclType)

fun variableFromAend ({name,aend_type,...}:associationend) =
    Variable (toLower (short_name_of_path name)^nextUid (),aend_type)

fun variableFromClassifier (cls:Classifier) =
    Variable (toLower (short_name_of cls)^nextUid () ,type_of cls)

fun roleToAend source ({name,aend_type,...}:associationend) =
    ocl_aendcall source name (Collection aend_type)

fun fixAends source (aends:associationend list) =
    let
      fun equal (a,b) = ocl_eq a b

      val vars = map variableFromAend aends
      val roles = map (roleToAend source) aends
      val body = ocl_and_all (map equal (ListPair.zip (roles,vars)))
      val sourceType = Rep_OclHelper.type_of source
      val ocl = ocl_select (ocl_allInstances 
                                (Literal (short_name_of_OclType sourceType,
                                          sourceType)))
                           source body
    in
      (ocl,vars)
    end

fun multiplicityOclConstraints source multis oppAends =
    let
      val _ = Logger.debug2 "multiplicityOclConstraint\n"
      fun bound set (low,high) =
          if low = high then
            ocl_eq (ocl_size set) (Literal(Int.toString high,Integer))
          else ocl_and (ocl_leq (ocl_size set) 
                                (Literal(Int.toString high,Integer)))
                       (ocl_geq (ocl_size set) 
                                (Literal(Int.toString low,Integer)))

      fun iterate _ [] done [] = []
        | iterate source (multi::ys) done (a::xs) = 
          let
            val (set,vars) = fixAends source (xs@done)
            val body = ocl_or_all (map (bound set) multi)
          in
            (SOME "MultiplicityConstraint",quantifyForAll vars body)::
            (iterate source ys (a::done) xs)
          end

      val selfVar = self (type_of source)
    in
      iterate selfVar multis [] oppAends
    end

(**
 * @params {source,reference,selfAend,roles,refRoles}
 * @param source source of the new binary association
 * @param reference dummy class used for constraints
 * @param selfAend aend from dummy class to source
 * @param roles aends from source to targets
 * @param refRoles aends from dummy to same targets as roles
 *)
fun consistencyOclConstraint source reference selfAend roles refRoles =
    let
      val _ = Logger.debug2 "consistencyOclConstraint\n"
      fun implies selfVar refVar {name=selfPath,aend_type=selfType,
                                  multiplicity,init,visibility,ordered} 
                  ((role as {name=newPath,aend_type=newType,ordered=ord2,
                             init=init2,multiplicity=mult2,visibility=vis2}),
                   {name=refPath,aend_type=refType,multiplicity=multi3,
                    init=init3,visibility=vis3,ordered=ord3}) =
          let
            val var = variableFromAend role
            val refVarType = Rep_OclHelper.type_of refVar
            val refLit = Literal (short_name_of_OclType refVarType,
                                  refVarType)
            (* new binary link *)
            val link = ocl_exists (ocl_aendcall selfVar newPath 
                                                (Set newType))
                                  var ocl_true
            (* links from dummy class *)
            val refOther = ocl_aendcall refVar refPath newType (* mult of 1 *)
            val refSelf = ocl_aendcall refVar selfPath selfType (* mult of 1 *)
            (* combining all *)
            val included = ocl_exists (ocl_allInstances refLit) refVar
                                      (ocl_and (ocl_eq refOther refVar)
                                               (ocl_eq refSelf selfVar))
            val body = ocl_implies link included
          in
            (SOME "ConsistencyConstraint", quantifyForAll [var] body)
          end

      val selfVar = self (type_of source)
      val refVar = variableFromClassifier reference

    in
      (source, map (implies selfVar refVar selfAend ) 
                   (ListPair.zip (roles,refRoles)))
    end

fun splitNAryAssociation (association as {name,qualifiers,aends=[a,b],
                                          aclass}) classifiers =
    let
      val modifiedClassifiers = List.filter (fn cls => type_of cls = 
                                                       type_of_aend a) 
                                            classifiers
      val modifiedClassifiers = modifiedClassifiers @
                                (List.filter (fn cls => type_of cls = 
                                                        type_of_aend b) 
                                             classifiers)
      val roleNames = [role_of_aend a, role_of_aend b]
    in
      (modifiedClassifiers, roleNames,[[b],[a]], [[association],[association]],
       [association])
    end
  | splitNAryAssociation (association as {name,qualifiers,
                                          aends,aclass}) classifiers =
    let
      val _ = Logger.debug2 "splitNAryAssociation\n"
      fun updateClassifier ((clsType,newAssocs),classifiers) =
        let
	        val ([cls],rem) = List.partition (fn x => type_of x = clsType )
					                                 classifiers
	        val modifiedCls = modifyAssociationsOfClassifier newAssocs 
                                                           [association]
                                                           cls
        in
	        modifiedCls::rem
        end
        
      fun group assocs (aend as {name,aend_type,multiplicity,ordered,
                                 visibility,init}) =
          let
            fun collectPairs (((a,b),newAssoc),(oppAends,binaryAssocs)) =
                (* need to check type and role in case of reflexive 
                 * associations *)
                if (type_of_aend a = aend_type) andalso
                   role_of_aend a = role_of_aend aend 
                then
                  (b::oppAends,newAssoc::binaryAssocs)
                else if (type_of_aend b = aend_type) andalso
                        role_of_aend b = role_of_aend aend then
                  (a::oppAends,newAssoc::binaryAssocs)
                else (oppAends,binaryAssocs)
                     
            val (oppAends,binaryAssocs) = foldl collectPairs ([],[]) assocs
            val role = role_of_aend aend
          in
            (aend_type,role,oppAends,binaryAssocs)
          end
          
      fun iterate [a] = []
        | iterate ((aend as {name,aend_type,multiplicity,ordered,
                             visibility,init})::xs) =
          let
            fun makeAssoc (targetAend as {name=targetName,aend_type=targetType,
                                          init=init2,ordered=ord2,
                                          multiplicity=mult2,
                                          visibility=vis2}) =
                let
                  val assocPath = package_of_aend aend
                  val assocName = short_name_of_path (association_of_aend
                                                          aend)
                  val newAssocName = assocPath@[assocName^ nextUid ()]
                  val sourceRole = role_of_aend aend
                  val targetRole = role_of_aend targetAend
                  val targetAend = {name=newAssocName@[targetRole],
                                    aend_type=targetType,
                                    multiplicity=[],
                                    ordered=ord2,
                                    visibility=vis2,
                                    init=init2}
                  val sourceAend = {name=newAssocName@[sourceRole],
                                    aend_type=aend_type,
                                    multiplicity=[],
                                    ordered=ordered,
                                    visibility=visibility,
                                    init=init}
                  val binaryAssoc = {name=newAssocName,
                                     aends=[sourceAend,targetAend],
                                     qualifiers=[],
                                     aclass=NONE}
                in
                  ((sourceAend,targetAend),binaryAssoc)
                end
          in
            map makeAssoc xs @ (iterate xs)
          end

      fun order [] [] = []
        | order [] (x::xs) = 
          raise InvalidArguments ("splitNAryAssociation.order:"^
                                  "arguments don't agree\n")
        | order (x::xs) [] = 
          raise InvalidArguments ("splitNAryAssociation.order:"^
                                  "arguments don't agree\n")
        | order pairs (cls::clsses) =
          let
            val ([oppAend],rem) = List.partition (fn (oppType,_,_,_) => 
                                                     type_of cls = oppType) 
                                               pairs
          in
            oppAend :: (order rem clsses)
          end
          
      fun unzip4 [] = ([],[],[],[])
        | unzip4 ((a,b,c,d)::xs) =
          let
            val (az,bs,cs,ds) = unzip4 xs
          in
            (a::az,b::bs,c::cs,d::ds)
          end

      fun getAssoc ((_,_),assoc) = assoc

      (* generate new associations *)
      val namedAssocs = iterate aends
      val pairs = map (group namedAssocs) aends
      val (types,roleNames,oppAends,splitAssocs) = unzip4 (order pairs 
                                                                 classifiers)
      val assocs = map getAssoc namedAssocs
      (* update associations in classifiers to the new names *)
      val modifiedClassifiers = foldl updateClassifier classifiers 
                                      (ListPair.zip (types,splitAssocs))
    in
      (modifiedClassifiers, roleNames, oppAends, splitAssocs, assocs)
    end
  
(* target type and role name is unqiue, even with reflexive links *)
fun matchAends (oppRefAends:associationend list) oppAends =
    let
      fun findMatch {aend_type=oppAendType,name=oppName,multiplicity,init,
                     ordered,visibility}  =
          hd (List.filter (fn {aend_type=refAendType,name=refName,ordered,
                               visibility,init,multiplicity} => 
                              oppAendType = refAendType andalso 
                              List.last oppName = List.last refName)
                          oppRefAends)
    in
      map findMatch oppAends
    end

fun matchClassifiersAtAend aends classifiers =
    let
      fun match {name,aend_type,ordered,init,multiplicity,visibility} =
          hd (List.filter (fn cls => type_of cls = aend_type) 
                          classifiers)

      fun remove (cls,classifiers) =
          #2 (List.partition (fn x => type_of x = type_of cls) classifiers)

      val matched = map match aends
      val rem = foldl remove classifiers matched
    in
      (matched,rem)
    end
       
(* source type and role name is still unqiue for classifiers and role *)
fun matchAendsFromClassifier (assocs:association list) source roles =
    let
      fun matchAend aendPairs cls role =
          let
            val [(_,b)] = List.filter (fn (a,b) 
                                     => type_of_aend a = type_of cls andalso
                                        role_of_aend b = role ) aendPairs
          in
            b
          end
          
      fun mirror {name,aends=[a,b],qualifiers,aclass} = [(a,b),(b,a)]
          
      val aendPairs = List.concat (map mirror assocs)
    in
      map (matchAend aendPairs source) roles
    end
    


end
