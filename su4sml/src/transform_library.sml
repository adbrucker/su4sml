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
val uniquenessOclConstraint : Rep_Core.Classifier -> 
                              Rep_Core.association list
                              -> Rep_Core.constraint

(**
 * @params {source,multis,binaryAssocs}
 *)
val multiplicityOclConstraint: Rep_Core.Classifier -> (int*int) list list ->
                               Rep_Core.association list -> 
                               Rep_Core.constraint

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
val splitNAryAssociation: Rep_Core.association -> Rep_Core.Classifier list ->
                          (Rep_Core.Classifier list * string list * 
                           Rep_Core.associationend list list * 
                           Rep_Core.association list list)

(**
 * Rearrange the oppRefAends to mirror the ordering of oppAends
 * @params {oppRefAends,oppAends}
 * @param oppRefAends
 * @param oppAends
 * @return the elements of oppRefAends in the same order as oppAends, given
 *         the classifier and the role name
 *)
val matchAends: Rep_Core.associationend list -> 
                Rep_Core.associationend list -> 
                Rep_Core.associationend list

(**
 * 
 *)
val matchClassifiers: Rep_Core.associationend list -> 
                      (Rep_Core.Classifier * string) list
                      -> Rep_Core.associationend list

(**
 * Form binary associations between
 * @params {source,targets,aends}
 * @param source one of the ends for each of the binary associations
 * @param targets each target is the second aend in exactly one of the
 *                resulting associations
 * @param aends original associationends at the targets. The returned
 *              associationends. oppRefAends are in the same order as the 
 *              supplied aends and denote the new association ends
 * @returns (binaryAssocs,oppRefAends)
 *)
val binaryAssociations: Rep_Core.Classifier -> Rep_Core.Classifier list -> 
                        Rep_Core.associationend list -> 
                        (Rep_Core.association list * 
                         Rep_Core.associationend list)

val nextUid: unit -> string
(**
 * Helper function for generating new, unique classes within a given
 * package.
 *)
val newDummyClass: Rep_OclType.Path -> Rep_Core.Classifier

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

val uid: int ref

exception InvalidArguments of string
end


structure Transform_Library:TRANSFORM_LIBRARY =
struct

open library
open ListPair
open List
open StringHandling
open Rep_OclTerm
open Rep_OclType
open Rep_OclHelper
open Rep_Core

exception InvalidArguments of string

val uid = ref 0

fun nextUid () =  (uid := !uid + 1; "_S"^(Int.toString (!uid)))

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


fun isPureBinAssoc {name,aends=[a,b],aclass=NONE} =
    let
      (* TODO: update when qualifiers added *)
      val _ = trace function_calls "isPureBinAssoc\n"
    in
      true
    end
  | isPureBinAssoc _ = false

fun isPureNAryAssoc _ = false (*FIXME*)

fun newDummyClass package =
    Class{name=Classifier (package@["Dummy"^ nextUid ()]),
          parent=NONE,
          attributes=[],
          operations=[],
          associations=[],
          invariant=[],
          stereotypes=[],
          interfaces=[],
          thyname=NONE,
          activity_graphs=[]}

fun addAssociations newAssocs associations =
    let
    in 
      associations (* FIXME *)
    end

fun removeAssociations oldAssocs associations =
    let
    in 
      associations (* FIXME *)
    end

fun modifyAssociationsOfClassifier newAssociations oldAssociations 
                                   (Class{name,parent,attributes,
                                          operations,associations,invariant,
                                          stereotypes,interfaces,thyname,
                                          activity_graphs}) =
    Class{name=name,
          parent=parent,
          attributes=attributes,
          operations=operations,
          associations= addAssociations newAssociations (removeAssociations
                                                             oldAssociations
                                                             associations),
          invariant=invariant,
          stereotypes=stereotypes,
          interfaces=interfaces,
          thyname=thyname,
          activity_graphs=activity_graphs}
  | modifyAssociationsOfClassifier newAssociations oldAssociations 
                                   (AssociationClass{name,parent,attributes,
                                                     operations,associations,
                                                     invariant,association,
                                                     stereotypes,interfaces,
                                                     thyname,activity_graphs}) =
    AssociationClass{name=name,
                     parent=parent,
                     attributes=attributes,
                     operations=operations,
                     associations= addAssociations newAssociations 
                                   (removeAssociations oldAssociations
                                                       associations),
                     association=association,
                     invariant=invariant,
                     stereotypes=stereotypes,
                     interfaces=interfaces,
                     thyname=thyname,
                     activity_graphs=activity_graphs}
    
  | modifyAssociationsOfClassifier newAssociations oldAssociations 
                                   (Primitive{name,parent,operations,
                                              associations,invariant,
                                              stereotypes,interfaces,
                                              thyname}) =
    Primitive {name=name,
	             parent=parent,
	             operations=operations,
	             associations= addAssociations newAssociations 
                             (removeAssociations oldAssociations
                                                 associations),
	             invariant=invariant,
	             stereotypes=stereotypes,
	             interfaces=interfaces,
	             thyname=thyname}
    	
fun uniquenessOclConstraint source associations =
    let
      fun assocAendCalls self iter {name,aends,aclass} = 
          let
            val [{name,aend_type,...}] = filter (fn {aend_type,...} => 
                                                    aend_type <> type_of self)
                                                aends
            val selfCall = ocl_aendcall self name (Collection aend_type)
            val iterCall = ocl_aendcall iter name (Collection aend_type)
          in
            ocl_eq selfCall iterCall
          end

      val _ = trace function_calls "uniquenessOclConstraint\n"
      val selfVar = self (type_of source)
      val iterVar = Variable ("other"^nextUid (),type_of source)
      val aendCalls = map (assocAendCalls selfVar iterVar) associations
      val oclBody = ocl_implies (ocl_and_all aendCalls) (ocl_eq selfVar
                                                                 iterVar)
      val constr = quantifyForAll [iterVar] oclBody
    in
      (SOME "Uniqueness", constr)
    end


fun binaryAssociations source targets aends=
    let
      val _ = trace function_calls "binaryAssociations\n"
      fun generateAssociation target =
          let
            val assocName =  path_of_package source @
                             ["BinaryAssoc"^nextUid ()]
            val oppAend = {name=assocName#[short_name_of target],
                           aend_type=type_of target,
                           multiplicity=[(1,1)],
                           ordered=false,
                           visibility=XMI_DataTypes.public,
                           init=NONE}
          in
            ({name= assocName,
             aends=[{name=assocName@ [short_name_of source],
                     aend_type=type_of source,
                     multiplicity=[],
                     ordered=false,
                     visibility=XMI_DataTypes.public,
                     init=NONE},
                    oppAend],
                    aclass=NONE}
           ,oppAend)
          end

      fun order [] (x::xs) = 
          raise InvalidArguments ("binaryAssociations.order:"^
                                  "arguments don't agree\n")
        | order (x::xs) [] = 
          raise InvalidArguments ("binaryAssociations.order:"^
                                  "arguments don't agree\n")
        | order pairs ({name,aend_type,...}::aends) =
          let
            val (oppAend,rem) = List.partition (fn (_,{oppAendName,...}) => 
                                                     oppAendName = name) pairs
          in
            if length oppAend = 1 then oppAend :: order rem aends
            else raise InvalidArguments ("binaryAssociations.order:"^
                                         "arguments don't agree\n")
          end
          

      val pairs = map generateAssociation targets
    in
      unzip o order pairs aends
    end

fun variableFromAend {name,aend_type,...} =
    Variable((toLower o get_short_name name)^nextUid (),aend_type)

fun roleToAend source {name,aend_type,...} =
    ocl_aendcall source name (Collection aend_type)

fun fixAends source aends =
    let
      fun equal (a,b) = ocl_eq a b

      val vars = map variableFromAend aends
      val roles = map (roleToAend source) aends
      val body = ocl_and_all (map equal (ListPair.zip roles vars))
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
        | iterate source (multi::ys) done (a::xs) = 
          let
            val (set,vars) = fixAends source (xs@done)
            val body = ocl_or_all (map (bound set) multi)
          in
            (SOME "MultiplicityConstraint",quantifyForAll vars body)::
            (iterate self ys (a::done) xs)
          end

      val selfVar = self (type_of source)
    in
      iterate selfVar multis [] binaryAssocs
    end

fun consistencyOclConstraint source reference selfAend roles refRoles =
    let
      val _ = trace function_calls "consistencyOclConstraint\n"
      fun implies self refer {name=selfPath,aend_type=selfType,...} 
                  ((role as {name=newPath,aend_type=newType,...}),
                   {name=refPath,aend_type=refType,...}) =
          let
            val var = variableFromAend role
            val refLit = Literal (type_of refer)
            val link = ocl_exists (ocl_aendcall self newPath 
                                                (Collection newType))
                                  var ocl_true
            val refOther = ocl_aendcall refer refPath (Collection newType)
            val refSelf = ocl_aendcall refer selfPath (Collection selfType)
            val included = ocl_exists (ocl_allInstances refLit) refer
                                      (ocl_and (ocl_eq refOther refer)
                                               (ocl_eq refSelf self))
            val body = ocl_implies link included
          in
            (SOME "ConsistencyConstraint", quantifyForAll [var] body)
          end

      val self = self (type_of source)
      val refer = variableFromClassifier reference

    in
      (source, map (self refer selfAend ) (zip roles refRoles))
    end

fun splitNAryAssociation (association as {name as (qualifier::assocName),
                                          aends,aclass}) classifiers =
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
            fun makeAssoc (sourceAend as {name,aend_type,...})
                          {name=targetName,aend_type=targetType,...} =
                let
                  val assocPath = package_of_aend sourceAend
                  val assocName = association_of_aend sourceAend
                  val role = role_of_aend sourceAend
                  val newAssocName = assocPath@[assocName^nextUid]
                  val oppAend = {name=newAssocName@[List.last targetName],
                                 aend_type=targetType,
                                 multiplicity=[],
                                 ordered=false,
                                 visibility=XMI_DataTypes.public,
                                 init=NONE}
                  val binaryAssoc = {name=newAssocName,
                                     aends=[{name=newAssocName@[role],
                                             aend_type=aend_type,
                                             multiplicity=[],
                                             ordered=false,
                                             visibility=XMI_DataTypes.public,
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
          
      fun order [] (x::xs) = 
          raise InvalidArguments ("splitNAryAssociation.order:"^
                                  "arguments don't agree\n")
        | order (x::xs) [] = 
          raise InvalidArguments ("splitNAryAssociation.order:"^
                                  "arguments don't agree\n")
        | order pairs (cls::clsses) =
          let
            val (oppAend,rem) = List.partition (fn (oppCls,_,_,_) => 
                                                   name_of oppCls = 
                                                   name_of cls) 
                                               pairs
          in
            if length oppAend = 1 then oppAend :: order rem clsses
            else raise InvalidArguments ("splitNAryAssociation.order:"^
                                         "arguments don't agree\n")
          end
          
      fun unzip4 [] = []
        | unzip4 ((a,b,c,d)::xs) =
          let
            val (az,bs,cs,ds) = unzip xs
          in
            (a::az,b::bs,c::cs,d::ds)
          end

      fun getPaths assocs = map name_of_assoc assocs

      (* generate new associations *)
      val pairs = iterate classifiers [] aends
      val (clsses,roleNames,oppAends,splitAssocs) = unzip4 o order pairs 
                                                                   classifiers

      (* update associations in classifiers to the new names *)
      val modifiedClassifiers = foldl updateClassifier classifiers 
                                      (zip (map type_of clsses)
                                           (map getPaths splitAssocs))
    in
      (modifiedClassifiers, roleNames, oppAends, splitAssocs)
    end
  
(* target type and role name is unqiue, even with reflexive links *)
fun matchAends oppRefAends oppAends =
    let
      fun findMatch {aend_type=oppAendType,name=oppName,...} =
          hd o  filter (fn {aend_type=refAendType,name=refName,
                            ...} => oppAendType = refAendType andalso 
                                    List.last oppName = List.last refName) 
                       oppRefAends 
    in
      map findMatch oppAends
    end

        
(* target type and role name is still unqiue for classifiers and role *)
fun matchClassifiers oppRefAends pairs =
    let
      fun matchClassifier (cls,role) =
          hd (filter (fn {aendType,name,...} => 
                         type_of cls = aendType andalso
                         role = List.last name) oppRefAends)
    in
       map matchClassifier  pairs
    end
    

fun binaryAssociations source targets aends =
    let
      val _ = trace function_calls "binaryAssociations\n"
      fun generateAssociation target =
          let
            val assocName =  package_of source@["BinaryAssoc"^(nextUid ())]
          in
            ({name= assocName,
              aends=[{name=assocName@[short_name_of source],
                      aend_type=type_of source,
                      multiplicity=[],
                      ordered=false,
                      visibility=XMI_DataTypes.public,
                      init=NONE},
                     {name=assocName@[short_name_of target],
                      aend_type=type_of target,
                      multiplicity=[(1,1)],
                      ordered=false,
                      visibility=XMI_DataTypes.public,
                      init=NONE}],
              aclass=NONE},
             {name=assocName@[short_name_of target],
              aend_type=type_of target,
              multiplicity=[(1,1)],
              ordered=false,
              visibility=XMI_DataTypes.public,
              init=NONE}
            )
          end

      val (assocs,refAends) = ListPair.unzip (map generateAssociation targets)
      val oppAends = matchAends refAends aends
    in
      (assocs, oppAends)
    end


end
