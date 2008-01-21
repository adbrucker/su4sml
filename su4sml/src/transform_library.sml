signature TRANSFORM_LIBRARY =
sig

(**
 * Generate an OCL constraint guaranteeing that source is unique over the
 * supplied binary associations.
 * @params {source,associations}
 * @param source classifier that needs a uniqueness constraint
 * @param associations binary associations the uniqueness constraint is 
 *        defined over. An n-ary association are not a valid argument.
 * @return an OCL constraint expressing the uniqueness requirement
 *)
val uniquenessOclConstraint: Rep_OclType.OclType -> Rep_Core.association list
                             -> Rep_OclTerm.OclTerm

(**
 * @params {source,multis,binaryAssocs}
 *)
val multiplicityOclConstraint: Rep_OclType.OclType -> (int*int) list list ->
                               Rep_Core.association list -> Rep_Core.constraint

(**
 * @params {association}
 * @param association n-ary association that should be split into it's binary
 * links.
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
val splitNAryAssociation: Rep_Core.association -> (Rep_Core.Classifier list * 
                          string list * Rep_Core.aend list list * 
                          Rep_Core.association list list)

(**
 * Rearrange the oppRefAends to mirror the ordering of oppAends
 * @params {oppRefAends,oppAends}
 * @param oppRefAends
 * @param oppAends
 * @return the elements of oppRefAends in the same order as oppAends, given
 *         the classifier and the role name
 *)
val matchAends: Rep_Core.aend list -> Rep_Core.aend list -> Rep_Core.aend list

(**
 * 
 *)
val matchClassifiers: Rep_Core.aend list -> (Rep_Core.Classifier * string) list

val binaryAssociations: Rep_Core.Classifier -> Rep_Core.Classifier list -> 
                        Rep_Core.aends -> 
                        (Rep_Core.association list * Rep_Core.aend list)

(**
 * Helper function for generating new, unique classes within a given
 * package.
 *)
val newDummyClass: Rep_OclType.Path -> Rep_OclType.Classifier

val isPureNAryAssoc: Rep_Core.association -> bool

(**
 * returns true iif assoc is purely a binary association, without any 
 * additional adornments, such as aggregation, qualifier, association class, 
 * etc.
 * @params {assoc}
 * @param assoc association to be tested
 * @return true iif assoc is a pure binary association
 *)
val isPureBinAssoc : Rep_Core.association -> bool


end

open library
open StringHandling

structure Transform_Library:TRANSFORM_LIBRARY =
struct

fun isPureBinAssoc (_,[a,b],NONE) =
    let
      (* TODO: update when qualifiers added *)
      val _ = trace function_calls "isPureBinAssoc\n"
    in
      true
    end
  | isPureBinAssoc (_,_,_)= false

fun addAssociations newAssocs associations =
    let
    in
    end

fun removeAssociations oldAssocs associations =
    let
    in
    end

fun modifyAssociationOfClassifier newAssociations olAssociations 
                                  (Class{name,parent,attributes,
                                         operations,associations,invariant,
                                         stereotypes,interfaces,thyname,
                                         activity_graphs}) =
    Class{name=name,
          parent=parent,
          attributes=attributes,
          operations=operations,
          associations= addAssociations newAssociations (removeAssociations
                                                             oldassociations
                                                             associations),
          invariant=invariant,
          stereotypes=stereotypes,
          interfaces=interfaces,
          thyname=thyname,
          activity_graphs=activity_graphs}
  | modifyAssociationToClassifier newAssociations oldAssociation 
                                  (AssociationClass{name,parent,attributes,
                                                    operations,associations,
                                                    invariant,associationclass,
                                                    stereotypes,interfaces,
                                                    thyname,activity_graphs}) =
    AssociationClass{name=name,
                     parent=parent,
                     attributes=attributes,
                     operations=operations,
                     associations= addAssociations newAssociations 
                                   (removeAssociations oldassociations
                                                       associations),
                     associationclass=associationclass,
                     invariant=invariant,
                     stereotypes=stereotypes,
                     interfaces=interfaces,
                     thyname=thyname,
                     activity_graphs=activity_graphs}
    
  | modifyAssociationToClassifier newAssociations oldAssociations 
                                  (Primitive{name,parent,operations,
                                             associations,invariant,
                                             stereotypes,interfaces,
                                             thyname}) =
    Primitive {name=name,
	             parent=parent,
	             operations=operations,
	             associations= addAssociations newAssociations 
                             (removeAssociations oldassociations
                                                 associations),
	             invariant=invariant,
	             stereotypes=stereotypes,
	             interfaces=interfaces,
	             thyname=thyname}
    	
fun quantifyForAll variables body =
    let
      fun quantify (variable,body) =
          ocl_forAll (ocl_allInstances (Literal (type_of variable))) 
                     [variable] body
    in
      (* right most variable at the inner most position *)
      foldr quantify body variables
    end

fun uniquenessOclConstraint source associations =
    let
      fun assocAendCalls self iter {_,aends,_} = 
          let
            val [{name,aend_type,...}] = filter (fn x => type_of_aend x =/=
                                                          type_of self) aends
            val selfCall = ocl_aendcall self name (Collection aend_type)
            val iterCall = ocl_aendcall iter name (Collection aend_type)
          in
            ocl_eq selfCall iterCall
          end

      val _ = trace function_calls "uniquenessOclConstraint\n"
      val selfVar = self (type_of source)
      val iterVar = Variable ("other",type_of source)
      val aendCalls = map (assocAendCalls selfVar iterVar) associations
      val oclBody = ocl_implies (ocl_and_all aendsCalls) (ocl_eq selfVar
                                                                 iterVar)
      val constr = quantifyForAll [iterVar] oclBody
    in
      (SOME "Uniqueness", constr)
    end


fun binaryAssociations source targets =
    let
      val _ = trace function_calls "binaryAssociations\n"
      fun generateAssociation target =
          let
            val assocName =  path_of_package source::
                             [("dummyBinaryAssoc"^nextUID)]
          in
            {name= assocName,
             aends=[{name=assocName:: (short_name_of source),
                     aend_type=type_of source,
                     multiplicity=[],
                     ordered=false,
                     visibility=public,
                     init=NONE,},
                    {name=assocName:: (short_name_of target),
                     aend_type=type_of target,
                     multiplicity=[(1,1)],
                     ordered=false,
                     visibility=public,
                     init=NONE,
                    ],
                    aclass=NONE}
          end
    in
      map generateAssociation targets
    end

fun variableFromAend {name,aend_type,...} =
    Variable((toLower o get_short_name name)^nextUID,aend_type)

fun roleToAend source {name,aend_type,...} =
    ocl_aendcall source name (Collection aend_type)

fun fixAends source aends =
    let
      fun equal (a,b) = ocl_eq a b

      val vars = map variableFromAend aends
      val roles = map (roleToAend source) aends
      val body = ocl_and_all (map equal (zip roles vars))
      val ocl = ocl_select (ocl_allInstances (Literal (type_of source))) 
                           source body
    in
      (ocl,vars)
    end

fun multiplicityOclConstraint source multis binaryAssocs =
    let
      val _ = trace_function_calls "multiplicityOclConstraint\n"
      fun bound set (low,high) =
          ocl_and (ocl_leq (ocl_size set) high)
                  (ocl_geq (ocl_size set) low)

      fun iterate _ [] done [] = []
        | iterate source multi::ys done a::xs = 
          let
            val (set,vars) = fixAends source (xs@done)
            val body = ocl_or_all (map (bound set) multi)
          in
            (SOME "MultiplicityConstraint",quantifyAll vars body)::
            (iterate self ys (a::done) xs)
          end

      val selfVar = self (type_of source)
    in
      iterate selfVar multis [] binaryAssocs
    end

fun consistencyOclConstraint source reference selfAend roles refRoles =
    let
      val _ = trace function_calls "consistencyOclConstraint\n"
      fun implies self ref {name=selfPath,aend_type,...} 
                  ((role as {name=newPath,aend_type=newType,...}),
                   {name=refPath,aend_type=refType,...}) =
          let
            val var = variableFromAend role
            val refLit = Literal (type_of ref)
            val link = ocl_exists (ocl_aendcall self newPath 
                                                (Collection newType))
                                  var ocl_true
            val refOther = ocl_aendcall ref refPath (Collection newType)
            val refSelf = ocl_aendcall ref selfPath (Collection selfType)
            val included = ocl_exists (ocl_allInstances refLit) ref
                                      (ocl_and (ocl_eq refOther ref)
                                               (ocl_eq refSelf self))
            val body = ocl_implies link included
          in
            (SOME "ConsistencyConstraint", quantifyAll [var] body)
          end

      val self = self (type_of source)
      val ref = variableFromClassifier reference

    in
      (source, map (self ref selfAend ) (zip roles refRoles))
    end

fun splitNAryAssociation {name as (qualifier::assocName),aends,aclass} 
                         classifiers =
    let
      val _ = trace function_calls "splitNAryAssociation\n"
      fun updateClassifier ((clsType,newPaths),classifiers) =
          let
	          val ([cls],rem) = List.partition (fn x => type_of x = clsType )
					                                   classifiers
	          val modifiedCls = modifyAssociationsOfClassifier newPaths 
                                                                 [association]
                                                                 cls
          in
	          modifiedCls@rem
          end

      fun iterate done [] = []
        | iterate done (aend as {name,aend_type,...}::xs) =
          let
            fun makeAssoc {name=assocPath@assocName::[role],aend_type,...} 
                          {name=_@[targetRole],aend_type=targetType,...} =
                let
                  val newAssocName = assocPath@[assocName^nextUid]
                  val oppAend = {name=newAssocName@[targetRole],
                                 aend_type=targetType,
                                 multiplicity=[],
                                 ordered=false,
                                 visibility=public,
                                 init=NONE}
                  val binaryAssoc = {name=newAssocName,
                                     aends=[{name=newAssocName@[role],
                                             aend_type=aend_type,
                                             multiplicity=[],
                                             ordered=false,
                                             visibility=public,
                                             init=NONE},
                                            oppAend],
                                     aclass=NONE}
                in
                  (oppAend,binaryAssoc)
                end

            val (oppAends,binaryAssocs) = unzip o map (makeAssoc aend) 
                                                      (done@xs)
            val role = short_name_of name
          in
            (aend_type,role,oppAends,binaryAssocs)::
            (iterate (aend::done) xs)
          end

      fun unzip4 [] = []
        | unzip4 (a,b,c,d)::xs =
          let
            val (az,bs,cs,ds) = unzip xs
          in
            (a::az,b::bs,c::cs,d::ds)
          end

      fun getPaths assocs = map name_of_assoc assocs

      (* generate new associations *)
      val (clsses,roleNames,oppAends,splitAssocs) = 
          unzip4 o iterate classifiers [] aends

      (* update associations in classifiers to the new names *)
      val modifiedClassifiers = foldl updateClassifier classifiers 
                                      (zip (map type_of clssses)
                                           (map getPaths splitAssocs))
    in
      (modifiedClassifiers, roleNames, oppAends, splitAssocs)
    end
  
(* target type and role name is unqiue, even with reflexive links *)
fun matchAends oppRefAends oppAends =
    map (fn x => hd o  filter (fn y => type_of_aend x = type_of_aend y
                                       andalso short_name_of_aend x =
                                               short_name of_aend y) 
                              oppRefAends) oppAends
        
(* target type and role name is still unqiue for classifiers and role *)
fun matchClassifiers oppRefAends pairs =
    let
      fun matchClassifier oppRefAends (cls,role) =
          hd o filter (fn x => type_of cls = type_of_aend x andalso
                               role = short_name_of_aend) oppRefAends
    in
       map (matchClassifier oppRefAends)  pairs
    end
    

fun binaryAssociations source targets aends =
    let
      val _ = trace function_calls "binaryAssociations\n"
      fun generateAssociation target =
          let
            val assocName =  path_of_package source::
                             ("dummyBinaryAssoc"^nextUID )
          in
            ({name= assocName,
              aends=[{name=assocName:: (short_name_of source),
                      aend_type=type_of source,
                      multiplicity=[],
                      ordered=false,
                      visibility=public,
                      init=NONE,},
                     {name=assocName:: (short_name_of target),
                      aend_type=type_of target,
                      multiplicity=[(1,1)],
                      ordered=false,
                      visibility=public,
                      init=NONE}],
              aclass=NONE},
             {name=assocName:: (short_name_of target),
              aend_type=type_of target,
              multiplicity=[(1,1)],
              ordered=false,
              visibility=public,
              init=NONE}
            )
          end

      val (assocs,refAends) = unzip (map generateAssociation target)
      val oppAends = matchAends refAends aends
    in
      (assocs, oppAends)
    end


end